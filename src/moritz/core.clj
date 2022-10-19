(ns moritz.core
  (:require [odoyle.rules :as o]
            [clojure.string :as str]
            [fen.core :as fen]))

;; TODO:
;; valid king move
;; - single square
;; - valid castle kingside
;; - valid castle queenside
;; - prevent castling if not allowed
;; don't allow moving into check
;; don't allow moving across check when castling
;; don't allow moving out of check when castling
;; valid en passant
;; recognise checkmate
;; recognise stalemate
;; generate random legal move
;; basic uci support
;; debug logging
;; track updated packages available
;; detect threefold repetition
;; introduce spec
;; introduce pbt
;; ci/cd
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
(def ^:private empty-square \-)
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
    (let [p (nth board i nil)]
      (if (= empty-square p)
        nil
        p))))

(defn- move->from-to
  [move]
  (when (some? move)
    [(subs move 0 2) (subs move 2)]))

(defn- north
  ([sq]
   (north sq 1))
  ([[file rank] n]
   (when (some? file)
     (let [rank-int (Character/digit rank 10)
           up-rank (+ rank-int n)]
       (when (<= 1 up-rank max-rank)
         (format "%s%s" file up-rank))))))

(defn- south
  ([sq]
   (south sq 1))
  ([sq n]
   (north sq (- n))))

(defn- west
  ([sq]
   (west sq 1))
  ([[file rank] n]
   (when (some? file)
     (let [i (str/index-of files file)
           new-file-idx (- i n)]
       (when-let [new-file (get files new-file-idx)]
         (format "%s%s" new-file rank))))))

(defn- east
  ([sq]
   (east sq 1))
  ([sq n]
   (west sq (- n))))

(def ^:private north-east (comp north east))
(def ^:private north-west (comp north west))
(def ^:private south-east (comp south east))
(def ^:private south-west (comp south west))

(defn- occupied?
  [board sq]
  (some? (square->piece board sq)))

(def ^:private unoccupied? (comp not occupied?))

(defn- apply-move!
  [{:keys [board move side-to-move halfmove-clock fullmove-number
           allow-white-queenside-castle? allow-white-kingside-castle?
           allow-black-queenside-castle? allow-black-kingside-castle?] :as _opts}]
  {:pre [(some? board)
         (some? move)
         (some? side-to-move)
         (and (some? halfmove-clock) (<= 0 halfmove-clock))]}
  (let [[from to] (move->from-to move)
        piece-from (square->piece board from)
        piece-from-name (piece-name piece-from)
        piece-to (square->piece board to)
        next-player (if (= white side-to-move)
                      black
                      white)
        next-move-number (if (= next-player white)
                           (inc fullmove-number)
                           fullmove-number)
        update-castling-rights (fn [s]
                                 (case [side-to-move piece-from-name from]
                                   [:white :rook "a1"]
                                   (when allow-white-queenside-castle?
                                     (o/insert s ::white ::allow-queenside-castle? false))

                                   [:white :rook "h1"]
                                   (when allow-white-kingside-castle?
                                     (o/insert s ::white ::allow-kingside-castle? false))

                                   [:black :rook "a8"]
                                   (when allow-black-queenside-castle?
                                     (o/insert s ::black ::allow-queenside-castle? false))

                                   [:black :rook "h8"]
                                   (when allow-black-kingside-castle?
                                     (o/insert s ::black ::allow-kingside-castle? false))

                                   [:white :king "e1"]
                                   (if (contains? #{"g1"} to)
                                     (-> s
                                         (o/insert ::white ::allow-kingside-castle? false)
                                         (o/insert ::white ::allow-queenside-castle? false))
                                     s)

                                   s))
        halfmove-clock (cond
                         (= :pawn (piece-name piece-from)) 0
                         ;; capture
                         (or (white-pieces piece-to) (black-pieces piece-to)) 0
                         :else (inc halfmove-clock))
        apply-mv (fn apply-mv [b]
                   (case [side-to-move piece-from-name from to]
                     [:white :king "e1" "g1"]
                     (-> b
                         vec
                         (assoc (square->idx from) \-)
                         (assoc (square->idx to) piece-from)
                         (assoc (square->idx "h1") \-)
                         (assoc (square->idx "f1") \R))

                     (-> b
                         vec
                         (assoc (square->idx from) \-)
                         (assoc (square->idx to) piece-from))
                     ))]
    (-> o/*session*
        (update-castling-rights)
        (o/insert ::board ::state (apply-mv board))
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
                  north
                  south)
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

(defn- colour
  [piece]
  (when (some? piece)
    (cond
      (contains? white-pieces piece) :white
      (contains? black-pieces piece) :black
      :else nil)))

(defn- valid-slide-squares
  [{:keys [board side-to-move from slide-fns]}]
  (let [valid-targets (for [f slide-fns
                            :let [start (f from)]
                            :when (some? start)]
                        (let [possible-targets (take-while some? (iterate f start))
                              [unoccupied-squares occupied-squares] (split-with (partial unoccupied? board) possible-targets)
                              blocked-square (first occupied-squares)]
                          (if (some? blocked-square)
                            (let [blocker-colour (->> blocked-square
                                                      (square->piece board)
                                                      colour)]
                              (if (= blocker-colour side-to-move)
                                unoccupied-squares
                                (conj unoccupied-squares blocked-square)))
                            unoccupied-squares)))]
    (-> valid-targets
        flatten
        set)))

(defn- allow-queen-move?
  [{:keys [board side-to-move move]}]
  (let [[from to] (move->from-to move)
        slide-fns [north east south west north-east north-west south-east south-west]
        valid-squares (valid-slide-squares {:board board
                                            :side-to-move side-to-move
                                            :from from
                                            :slide-fns slide-fns})]
    (contains? valid-squares to)))

(defn- allow-bishop-move?
  [{:keys [board side-to-move move]}]
  (let [[start to] (move->from-to move)
        possible-squares (for [f [north-west north-east south-west south-east]]
                           (loop [sq (f start)
                                  pos-squares []]
                             (if (some? sq)
                               (if-some [piece (square->piece board sq)]
                                 (if (= (colour piece) side-to-move)
                                   pos-squares
                                   (conj pos-squares sq))
                                 (recur (f sq)
                                        (conj pos-squares sq)))
                               pos-squares)))
        possible-squares (-> possible-squares
                             flatten
                             set)]
    (contains? possible-squares to)))

(defn- allow-knight-move?
  [{:keys [board side-to-move move]}]
  (let [[from to] (move->from-to move)
        west-2 #(west % 2)
        east-2 #(east % 2)
        north-2 #(north % 2)
        south-2 #(south % 2)

        move-fns #{(comp west-2 north)
                   (comp west-2 south)
                   (comp east-2 north)
                   (comp east-2 south)
                   (comp north-2 east)
                   (comp north-2 west)
                   (comp south-2 east)
                   (comp south-2 west)}

        allowed-squares (for [f move-fns
                              :let [square (f from)
                                    piece (when square (square->piece board square))]
                              :when (and square
                                         (or (nil? piece)
                                             (not= (colour piece) side-to-move)))]
                          square)]
    (contains? (set allowed-squares) to)))

(defn- allow-king-move?
  [{:keys [board side-to-move move allow-white-kingside-castle?] :as opts}]
  (let [[from to] (move->from-to move)
        valid-tos (for [f [north east south west north-east north-west south-east south-west]
                        :let [possible-to (f from)
                              piece (when (some? possible-to)
                                      (square->piece board possible-to))]
                        :when (and (some? possible-to)
                                   (or (nil? piece)
                                       (not= (colour piece) side-to-move)))]
                    possible-to)
        valid-tos (if (and allow-white-kingside-castle?
                           (every? (partial unoccupied? board) ["f1" "g1"]))
                    (conj valid-tos "g1")
                    valid-tos)
        valid-tos (set valid-tos)]
    (contains? valid-tos to)))

(defn- allow-rook-move?
  [{:keys [board side-to-move move]}]
  (let [[from to] (move->from-to move)
        allowed-squares (valid-slide-squares {:board board
                                              :side-to-move side-to-move
                                              :from from
                                              :slide-fns [north east south west]})]
    (contains? allowed-squares to)))

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
                           :rook ::rook
                           :queen ::queen
                           :knight ::knight
                           :king ::king
                           ::invalid)]
           (o/insert! ::move move-type move))))]

     ;; for now...
    ::allow-invalid
    [:what
     [::board ::state board {:then false}]
     [::player ::turn side-to-move {:then false}]
     [::game ::halfmove-clock halfmove-clock {:then false}]
     [::move ::number fullmove-number {:then false}]
     [::move ::invalid move]

     :then
     (apply-move! o/*match*)]

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
     (apply-move! o/*match*)]

    ::queen-move
    [:what
     [::board ::state board {:then false}]
     [::player ::turn side-to-move {:then false}]
     [::game ::halfmove-clock halfmove-clock {:then false}]
     [::move ::number fullmove-number {:then false}]
     [::move ::queen move]

     :when
     (allow-queen-move? {:board board, :side-to-move side-to-move, :move move})

     :then
     (apply-move! o/*match*)]

    ::rook-move
    [:what
     [::board ::state board {:then false}]
     [::player ::turn side-to-move {:then false}]
     [::game ::halfmove-clock halfmove-clock {:then false}]
     [::white ::allow-queenside-castle? allow-white-queenside-castle? {:then false}]
     [::white ::allow-kingside-castle? allow-white-kingside-castle? {:then false}]
     [::black ::allow-queenside-castle? allow-black-queenside-castle? {:then false}]
     [::black ::allow-kingside-castle? allow-black-kingside-castle? {:then false}]
     [::move ::number fullmove-number {:then false}]
     [::move ::rook move]

     :when
     (allow-rook-move? {:board board, :side-to-move side-to-move, :move move})

     :then
     (apply-move! o/*match*)]

    ::knight-move
    [:what
     [::board ::state board {:then false}]
     [::player ::turn side-to-move {:then false}]
     [::game ::halfmove-clock halfmove-clock {:then false}]
     [::move ::number fullmove-number {:then false}]
     [::move ::knight move]

     :when
     (allow-knight-move? {:board board, :side-to-move side-to-move, :move move})

     :then
     (apply-move! o/*match*)]

    ::king-move
    [:what
     [::board ::state board {:then false}]
     [::player ::turn side-to-move {:then false}]
     [::game ::halfmove-clock halfmove-clock {:then false}]
     [::white ::allow-queenside-castle? allow-white-queenside-castle? {:then false}]
     [::white ::allow-kingside-castle? allow-white-kingside-castle? {:then false}]
     [::black ::allow-queenside-castle? allow-black-queenside-castle? {:then false}]
     [::black ::allow-kingside-castle? allow-black-kingside-castle? {:then false}]
     [::move ::number fullmove-number {:then false}]
     [::move ::king move]

     :when
     (allow-king-move? {:board board, :side-to-move side-to-move, :move move, :allow-white-kingside-castle? allow-white-kingside-castle?})

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
                  (o/insert ::white ::allow-kingside-castle? allow-white-kingside-castle?)
                  (o/insert ::black ::allow-queenside-castle? allow-black-queenside-castle?)
                  (o/insert ::black ::allow-kingside-castle? allow-black-kingside-castle?)
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
