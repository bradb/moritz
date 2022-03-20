(ns moritz.core
  (:require [odoyle.rules :as o]
            [clojure.string :as str]
            [fen.core :as fen]))

;; TODO:
;; valid bishop move
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
;; (def ^:private pawn \P)
;; (def ^:private rook \R)
;; (def ^:private knight \N)
;; (def ^:private bishop \B)
;; (def ^:private queen \Q)
;; (def ^:private king \K)

(def ^:private pawn-start-rank {white 2, black 7})

(defn- square->idx
  [[file rank :as _square]]
  (let [rank (Character/digit rank 10)
        rank-offset (* (- 8 rank) 8)
        file-offset (str/index-of files file)]
    (when (and file-offset (<= 1 rank 8))
      (+ rank-offset file-offset))))

(comment
  (square->idx "h7"))

(defn- square->piece
  [board sq]
  (nth board (square->idx sq) nil))

(defn- move->from-to
  [move]
  [(subs move 0 2) (subs move 2)])

(defn- up
  ([sq]
   (up sq 1))
  ([[file rank] n]
   (let [rank-int (Character/digit rank 10)
         up-rank (+ rank-int n)]
     (when (<= up-rank max-rank)
       (format "%s%s" file up-rank)))))

(defn- down
  ([sq]
   (down sq 1))
  ([sq n]
   (up sq (- n))))

(defn- left
  ([sq]
   (left sq 1))
  ([[file rank] n]
   (let [i (str/index-of files file)
         new-file-idx (- i n)]
     (when-let [new-file (get files new-file-idx)]
       (format "%s%s" new-file rank)))))

(defn- occupied?
  [board sq]
  (let [piece (square->piece board sq)]
    (or ((pieces :white) piece)
        ((pieces :black) piece))))

(defn- set-halfmove-clock!
  [n]
  (o/insert! ::game ::halfmove-clock n))

(defn- apply-move!
  [board move halfmove-clock]
  (let [[from to] (move->from-to move)
        piece-from (square->piece board from)
        piece-to (square->piece board to)]
    (o/insert! ::board ::state (-> board
                                   vec
                                   (assoc (square->idx from) \-)
                                   (assoc (square->idx to) piece-from)))
    (cond
      (= :pawn (piece-name piece-from))
      (set-halfmove-clock! 0)

      ;; capture
      (or (white-pieces piece-to)
          (black-pieces piece-to))
      (set-halfmove-clock! 0)

      :else
      (set-halfmove-clock! (inc halfmove-clock)))

    (o/insert! ::game ::moved move)))

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

(def ^:private rules
  (o/ruleset
   {::game
    [:what
     [::player ::turn side-to-move]
     [::white ::allow-queenside-castle? allow-white-queenside-castle?]
     [::white ::allow-kingside-castle? allow-white-kingside-castle?]
     [::black ::allow-queenside-castle? allow-black-queenside-castle?]
     [::black ::allow-kingside-castle? allow-black-kingside-castle?]
     [::move ::number fullmove-number]
     [::move ::en-passant-target-square en-passant-target-square]
     [::game ::halfmove-clock halfmove-clock]
     [::board ::state board]]

    ::history
    [:what
     [::derived ::history history]]

    ::end-turn
    [:what
     [::game ::start started? {:then false}]
     [::board ::state board {:then false}]
     [::move ::number n {:then false}]
     [::player ::turn player {:then false}]
     [::game ::moved move]
     :then
     (let [history (-> (o/query-all o/*session* ::history)
                       first
                       :history
                       vec)
           next-player-turn (if (= player white)
                              black
                              white)]
       (o/insert!
        ::derived
        ::history
        (conj history {:move-number n
                       :player player
                       :board board
                       :move move}))

       (o/insert! ::player ::turn next-player-turn)

       (when (= next-player-turn white)
         (o/insert! ::move ::number (inc n))))]

    ::player-move
    [:what
     [::player ::turn side-to-move]
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
                                      (assoc (square->idx to) piece)))
       (o/insert! ::game ::moved move))]

    ::pawn-move
    [:what
     [::board ::state board {:then false}]
     [::player ::turn side-to-move {:then false}]
     [::game ::halfmove-clock halfmove-clock {:then false}]
     [::move ::pawn move]

     :when
     (allow-pawn-move? {:board board, :side-to-move side-to-move :move move})

     :then
     (apply-move! board move halfmove-clock)]

    ::bishop-move
    [:what
     [::board ::state board {:then false}]
     [::player ::turn player {:then false}]
     [::game ::halfmove-clock halfmove-clock {:then false}]
     [::move ::bishop move]

     :then
     (apply-move! board move halfmove-clock)]}))

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

(comment
  (reset-session!)
  (start-game!)
  @*session
  (o/query-all @*session ::game)
  (fen))

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
  "Return game history as a coll of maps containing the following keys:

  :move-number - The move number
  :move - The move played
  :board - The board state after move was played
  :player - The player whose turn it was"
  []
  (-> (o/query-all @*session ::history)
      first
      :history))
