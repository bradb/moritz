(ns moritz.core
  (:require [odoyle.rules :as o]
            [clojure.string :as str]
            [fen.core :as fen]))

;; TODO:
;; valid bishop move
;; introduce spec
;; introduce pbt
;; ci/cd
;; valid rook move
;; valid queen move
;; valid knight move
;; valid king move
;; valid en passant
;; valid castle kingside
;; valid castle queenside
;; recognise checkmate
;; recognise stalemate
;; prevent castling if not allowed
;; don't allow moving into check
;; generate random legal move
;; basic uci support
;; debug logging
;; introduce bitboards?
;; detect threefold repetition
;; init from fen
;; undo
(def start-position-default "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")

(def ^:private *session (atom nil))

(def ^:private files "abcdefgh")
(def ^:private max-rank 8)

(def white :white)
(def black :black)
(def ^:private pieces {white #{\P \R \N \B \Q \K}, black #{\p \r \n \b \q \k}})
(def ^:private black-pieces (pieces black))
(def ^:private white-pieces (pieces white))
(def ^:private piece-name {\P :pawn
                           \R :rook
                           \N :knight
                           \B :bishop
                           \Q :queen
                           \K :king
                           \p :pawn
                           \r :rook
                           \n :knight
                           \b :bishop
                           \q :queen
                           \k :king})
(def ^:private pawn-start-rank {white 2, black 7})

(defn- square->idx
  [[file rank :as _square]]
  (let [rank (Character/digit rank 10)
        rank-offset (* (- 8 rank) 8)
        file-offset (str/index-of files file)]
    (when (and file-offset (<= 1 rank 8))
      (+ rank-offset file-offset))))

(defn- square->piece
  [board sq]
  (when-some [i (square->idx sq)]
    (nth board i nil)))

(defn- move->from-to
  [move]
  (when (some? move)
    [(subs move 0 2) (subs move 2)]))

(defn- up
  ([sq]
   (up sq 1))
  ([[file rank] n]
   (when (some? file)
     (let [rank-int (Character/digit rank 10)
           up-rank (+ rank-int n)]
       (when (<= up-rank max-rank)
         (format "%s%s" file up-rank))))))

(defn- down
  ([sq]
   (down sq 1))
  ([sq n]
   (up sq (- n))))

(defn- left
  ([sq]
   (left sq 1))
  ([[file rank] n]
   (when (some? file)
     (let [i (str/index-of files file)
           new-file-idx (- i n)]
       (when-let [new-file (get files new-file-idx)]
         (format "%s%s" new-file rank))))))

(defn- right
  ([sq]
   (right sq 1))
  ([sq n]
   (left sq (- n))))

(defn- occupied?
  [board sq]
  (when (and (some? board) (some? sq))
    (let [piece (square->piece board sq)]
      (or ((pieces :white) piece)
          ((pieces :black) piece)))))

(comment
  (def ^:dynamic s (atom nil))
  (let [rules (o/ruleset
               {::first-rule
                [:what
                 [::fact ::x x]
                 [::fact ::y y]
                 :then
                 (do
                   (println "x is" x "y is" y)
                   (-> o/*session*
                       (o/insert ::fact ::z x)
                       o/reset!))]

                ::second-rule
                [:what
                 [::fact ::z z]
                 :then
                 (println "z is" z)]})
        _ (reset! s (reduce o/add-rule (o/->session) rules))]
    (-> @s
        (o/insert ::fact ::x 1)
        (o/insert ::fact ::y 2)
        (o/insert ::fact ::x 3)
        o/fire-rules)

    (o/query-all @s ::first-rule)))

(defn- apply-move!
  [{:keys [board move side-to-move halfmove-clock fullmove-number] :as _opts}]
  {:pre [(some? board)
         (some? move)
         (some? side-to-move)
         (and (some? halfmove-clock) (<= 0 halfmove-clock))]}
  (let [[from to] (move->from-to move)
        piece-from (square->piece board from)
        piece-to (square->piece board to)
        next-player (if (= white side-to-move)
                      black
                      white)
        next-move-number (if (= next-player white)
                           (inc fullmove-number)
                           fullmove-number)
        halfmove-clock (cond
                         (= :pawn (piece-name piece-from)) 0
                         ;; capture
                         (or (white-pieces piece-to) (black-pieces piece-to)) 0
                         :else (inc halfmove-clock))]
    (-> o/*session*
        (o/insert ::board ::state (-> board
                                      vec
                                      (assoc (square->idx from) \-)
                                      (assoc (square->idx to) piece-from)))
        (o/insert ::game ::halfmove-clock halfmove-clock)
        (o/insert ::move ::number next-move-number)
        (o/insert ::player ::turn next-player)
        (o/reset!))))

(defn- allow-pawn-move?
  [{:keys [board side-to-move move]}]
  (let [[from to] (move->from-to move)
        [_ from-rank] from
        from-rank (Character/digit from-rank 10)
        forward (if (= side-to-move white)
                  up
                  down)
        forward-1 (forward from)
        forward-2 (forward from 2)
        allowed-tos (cond
                      (occupied? board forward-1)
                      #{}

                      (= from-rank (get pawn-start-rank side-to-move))
                      #{forward-1 forward-2}

                      forward-1
                      #{forward-1}

                      :else
                      #{})
        allowed-tos (->> allowed-tos
                         (remove nil?)
                         (remove (partial occupied? board))
                         set)]
    (contains? allowed-tos to)))

(defn- allow-bishop-move?
  [{:keys [board move]}]
  (let [[from to] (move->from-to move)
        north-west (comp up left)
        north-east (comp up right)
        south-west (comp down left)
        south-east (comp down right)
        unoccupied-square? (fn [sq]
                             (when (some? sq)
                               (not (occupied? board sq))))
        possible-squares (for [f [north-west north-east south-west south-east]
                               :let [start (f from)]]
                           (take-while unoccupied-square? (iterate f start)))
        possible-squares (-> possible-squares
                             flatten
                             set)]
    (contains? possible-squares to)))

(defn- record-history!
  []
  (let [history (-> (o/query-all o/*session* ::history)
                    first
                    :history
                    vec)

        {:keys [side-to-move
                allow-white-queenside-castle?
                allow-white-kingside-castle?
                allow-black-queenside-castle?
                allow-black-kingside-castle?
                fullmove-number
                en-passant-target-square
                halfmove-clock
                board]}
        (-> (o/query-all o/*session* ::game) first)]
    (o/insert!
     ::derived
     ::history
     (conj history (fen/map->fen
                    {:fen/side-to-move side-to-move
                     :fen/allow-white-queenside-castle? allow-white-queenside-castle?
                     :fen/allow-white-kingside-castle? allow-white-kingside-castle?
                     :fen/allow-black-queenside-castle? allow-black-queenside-castle?
                     :fen/allow-black-kingside-castle? allow-black-kingside-castle?
                     :fen/fullmove-number fullmove-number
                     :fen/en-passant-target-square en-passant-target-square
                     :fen/halfmove-clock halfmove-clock
                     :fen/board board})))))

(def ^:private rules
  (o/ruleset
   {::game
    [:what
     [::player ::turn side-to-move {:then false}]
     [::white ::allow-queenside-castle? allow-white-queenside-castle? {:then false}]
     [::white ::allow-kingside-castle? allow-white-kingside-castle? {:then false}]
     [::black ::allow-queenside-castle? allow-black-queenside-castle? {:then false}]
     [::black ::allow-kingside-castle? allow-black-kingside-castle? {:then false}]
     [::move ::number fullmove-number {:then false}]
     [::move ::en-passant-target-square en-passant-target-square {:then false}]
     [::game ::halfmove-clock halfmove-clock {:then false}]
     [::board ::state board]

     :then
     (record-history!)]

    ::history
    [:what
     [::derived ::history history]]

    ::player-move
    [:what
     [::player ::turn side-to-move {:then false}]
     [::board ::state board {:then false}]
     [::game ::move move]

     :then
     (let [[from _] (move->from-to move)
           piece (square->piece board from)
           player-pieces (pieces side-to-move)]
       (when (player-pieces piece)
         (let [pn (get piece-name piece ::invalid)
               move-type (case pn
                           :pawn ::pawn
                           :bishop ::bishop
                           ::invalid)]
           (condp = pn
             :pawn (o/insert! ::move ::pawn move)
             :bishop (o/insert! ::move ::bishop move)
             (o/insert! ::move ::invalid move)))))]

     ;; for now...
    ::allow-invalid
    [:what
     [::board ::state board {:then false}]
     [::move ::invalid move]

     :then
     (let [[from to] (move->from-to move)
           piece (square->piece board from)]
       (o/insert! ::board ::state (-> board
                                      (assoc (square->idx from) nil)
                                      (assoc (square->idx to) piece))))]

    ::pawn-move
    [:what
     [::board ::state board {:then false}]
     [::player ::turn side-to-move {:then false}]
     [::game ::halfmove-clock halfmove-clock {:then false}]
     [::move ::number fullmove-number {:then false}]
     [::move ::pawn move]

     :when
     (allow-pawn-move? {:board board, :side-to-move side-to-move, :move move})

     :then
     (apply-move! o/*match*)]

    ::bishop-move
    [:what
     [::board ::state board {:then false}]
     [::player ::turn side-to-move {:then false}]
     [::game ::halfmove-clock halfmove-clock {:then false}]
     [::move ::number fullmove-number {:then false}]
     [::move ::bishop move]

     :when
     (allow-bishop-move? {:board board, :side-to-move side-to-move, :move move})

     :then
     (apply-move! o/*match*)]}))

(defn- print-board
  [board]
  (println)
  (let [sep (str/join (repeat 39 "-"))]
    (doseq [rank (reverse (partition 8 board))
            :let [formatted-rank (map #(format " %2s " (or % "")) rank)
                  rank-str (str/join "|" formatted-rank)]]
      (println sep)
      (println rank-str))
    (println sep))
  (println))

(defn reset-session!
  []
  (reset! *session (reduce o/add-rule (o/->session) rules)))

(defn start-game!
  "Initialise the session with the default starting board state.

  Optionally accepts a FEN string to start from an arbitrary position."
  ([]
   (start-game! start-position-default))
  ([fen-str]
   (let [{:fen/keys [board
                     allow-white-kingside-castle?
                     allow-white-queenside-castle?
                     allow-black-kingside-castle?
                     allow-black-queenside-castle?
                     side-to-move
                     en-passant-target-square
                     halfmove-clock
                     fullmove-number]} (fen/fen->map fen-str)]
     (swap! *session
            (fn [session]
              (-> session
                  (o/insert ::player ::turn side-to-move)
                  (o/insert ::white ::allow-queenside-castle? allow-white-queenside-castle?)
                  (o/insert ::white ::allow-kingside-castle? allow-white-queenside-castle?)
                  (o/insert ::black ::allow-queenside-castle? allow-black-queenside-castle?)
                  (o/insert ::black ::allow-kingside-castle? allow-black-queenside-castle?)
                  (o/insert ::board ::state board)
                  (o/insert ::move ::number fullmove-number)
                  (o/insert ::move ::en-passant-target-square en-passant-target-square)
                  (o/insert ::game ::halfmove-clock halfmove-clock)
                  (o/insert ::game ::start true)
                  o/fire-rules))))))

(defn move!
  "Apply moves to current board state."
  [& moves]
  (doseq [m moves]
    (swap! *session
           (fn [session]
             (-> session
                 (o/insert ::game ::move m)
                 o/fire-rules)))))

(defn fen
  "Return the current game state as a FEN string."
  []
  (let [state (-> @*session
                  (o/query-all ::game)
                  first)
        {:keys [board
                allow-white-kingside-castle?
                allow-white-queenside-castle?
                allow-black-kingside-castle?
                allow-black-queenside-castle?
                halfmove-clock
                fullmove-number
                en-passant-target-square
                side-to-move]} state]
    (fen/map->fen {:fen/board board
                   :fen/allow-white-kingside-castle? allow-white-kingside-castle?
                   :fen/allow-white-queenside-castle? allow-white-queenside-castle?
                   :fen/allow-black-kingside-castle?  allow-black-kingside-castle?
                   :fen/allow-black-queenside-castle? allow-black-queenside-castle?
                   :fen/halfmove-clock halfmove-clock
                   :fen/fullmove-number fullmove-number
                   :fen/en-passant-target-square en-passant-target-square
                   :fen/side-to-move side-to-move})))

(defn history
  "Return game history as a coll of FEN strings."
  []
  (-> (o/query-all @*session ::history)
      first
      :history))
