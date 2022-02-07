(ns moritz.core
  (:require [odoyle.rules :as o]
            [clojure.string :as str]))

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
;; debug logging
;; basic uci support
;; introduce bitboards?
;; detect threefold repetition
;; init from fen
;; undo

(def ^:private default-board
  ["wR" "wN" "wB" "wQ" "wK" "wB" "wN" "wR"
   "wP" "wP" "wP" "wP" "wP" "wP" "wP" "wP"
   nil nil nil nil nil nil nil nil
   nil nil nil nil nil nil nil nil
   nil nil nil nil nil nil nil nil
   nil nil nil nil nil nil nil nil
   "bP" "bP" "bP" "bP" "bP" "bP" "bP" "bP"
   "bR" "bN" "bB" "bQ" "bK" "bB" "bN" "bR"])

(defonce *session (atom nil))

(def ^:private files "abcdefgh")
(def ^:private max-rank 8)

(def ^:private white \w)
(def ^:private black \b)
(def ^:private pawn \P)
(def ^:private rook \R)
(def ^:private knight \N)
(def ^:private bishop \B)
(def ^:private queen \Q)
(def ^:private king \K)

(def ^:private pawn-start-rank {white 2, black 7})

(defn- square->idx
  [sq]
  (let [[file rank] sq
        rank (-> rank
                 str
                 Integer/parseInt)
        rank-offset (* (- rank 1) 8)
        file-offset (- (int file) (int \a))]
    (+ rank-offset file-offset)))

(defn- square->piece
  [board sq]
  (get board (square->idx sq)))

(defn- move->from-to
  [move]
  [(subs move 0 2) (subs move 2)])

;; TODO: maybe implement these as lazy seqs so i can take
;; an abitrary number of squares in a given direction?
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
  (square->piece board sq))

(comment
  (occupied? default-board "e2"))

(def ^:private rules
  (o/ruleset
   {::game
    [:what
     [::player ::turn player]
     [::white ::allow-castle? w-allow-castle?]
     [::black ::allow-castle? b-allow-castle?]
     [::move ::number n]
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
        (conj history {:move/number n
                       :player/turn player
                       :board/state board
                       :game/move move}))

       (o/insert! ::player ::turn next-player-turn)

       (when (= next-player-turn white)
         (o/insert! ::move ::number (inc n))))]

    ::player-move
    [:what
     [::player ::turn player]
     [::board ::state board {:then false}]
     [::game ::move move]
     :then
     (let [[from _] (move->from-to move)
           [color piece] (square->piece board from)]
       (when (= color player)
         (let [move-type (condp = piece
                           pawn ::pawn
                           ::invalid)]
           (o/insert! ::move move-type move))))]

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
     [::player ::turn player {:then false}]
     [::move ::pawn move]

     :when
     (let [[from to] (move->from-to move)
           [_ from-rank] from
           from-rank (Character/digit from-rank 10)
           forward (if (= player white)
                     up
                     down)
           forward-1 (forward from)
           forward-2 (forward from 2)
           allowed-tos (cond
                         (occupied? board forward-1)
                         #{}

                         (= from-rank (get pawn-start-rank player))
                         #{forward-1 forward-2}

                         forward-1
                         #{forward-1}

                         :else
                         #{})
           allowed-tos (->> allowed-tos
                            (remove nil?)
                            (remove (partial occupied? board))
                            set)]
       (contains? allowed-tos to))

     :then
     (let [[from to] (move->from-to move)
           pawn (square->piece board from)]
       (o/insert! ::board ::state (-> board
                                      (assoc (square->idx from) nil)
                                      (assoc (square->idx to) pawn)))
       (o/insert! ::game ::moved move))]}))

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
  ([]
   (start-game! {}))
  ([{:keys [player-turn allow-white-castle? allow-black-castle? board move-number]
     :or {player-turn white
          allow-white-castle? true
          allow-black-castle? true
          board default-board
          move-number 1}}]
   (swap! *session
          (fn [session]
            (-> session
                (o/insert ::player ::turn player-turn)
                (o/insert ::white ::allow-castle? allow-white-castle?)
                (o/insert ::black ::allow-castle? allow-black-castle?)
                (o/insert ::board ::state board)
                (o/insert ::move ::number move-number)
                (o/insert ::game ::start true)
                o/fire-rules)))))

(defn move!
  "Apply moves to current board state."
  [& moves]
  (doseq [m moves]
    (swap! *session
           (fn [session]
             (-> session
                 (o/insert ::game ::move m)
                 o/fire-rules)))))

(defn board
  "Return the current board state.

  Board state is 64 element coll of strings representing pieces on squares.
  For example, a white pawn is \"wP\", black knight \"bN\", and so on.
  Unoccupied squares are nil."
  []
  (-> @*session
      (o/query-all ::game)
      first
      :board))

(comment
  (reset-session!)
  (start-game!)
  (move! "e2e4")
  (move! "e7e5")
  (print-board (board))
  )
