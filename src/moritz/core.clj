(ns moritz.core
  (:require [odoyle.rules :as o]
            [clojure.string :as str]))

;; TODO:
;; valid one space pawn move
;; A* search to test target reachable
;; valid two space pawn move
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
;; init from fen
;; prevent castling if not allowed
;; don't allow moving into check
;; debug logging
;; basic uci support
;; introduce bitboards?
;; threefold repetition
;; undo

(def ^:private board
  ["wR" "wN" "wB" "wQ" "wK" "wB" "wN" "wR"
   "wP" "wP" "wP" "wP" "wP" "wP" "wP" "wP"
   nil nil nil nil nil nil nil nil
   nil nil nil nil nil nil nil nil
   nil nil nil nil nil nil nil nil
   nil nil nil nil nil nil nil nil
   "bP" "bP" "bP" "bP" "bP" "bP" "bP" "bP"
   "bR" "bN" "bB" "bQ" "bK" "bB" "bN" "bR"])

(defonce *session (atom nil))

(def ^:private white \w)
(def ^:private black \b)
(def ^:private pawn \P)
(def ^:private rook \R)
(def ^:private knight \N)
(def ^:private bishop \B)
(def ^:private queen \Q)
(def ^:private king \K)

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
      :then
      (let [[from to] (move->from-to move)
            pawn (square->piece board from)]
        (o/insert! ::board ::state (-> board
                                       (assoc (square->idx from) nil)
                                       (assoc (square->idx to) pawn)))
        (o/insert! ::game ::moved move))]

     }))

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

(comment
  (reset! *session
          (-> (reduce o/add-rule (o/->session) rules)
              (o/insert ::player ::turn white)
              (o/insert ::white ::allow-castle? true)
              (o/insert ::black ::allow-castle? true)
              (o/insert ::board ::state board)
              (o/insert ::move ::number 1)
              (o/insert ::game ::start true)
              (o/insert ::game ::move "g1f3")
              o/fire-rules
              (o/insert ::game ::move "e7e5")
              o/fire-rules
              (o/insert ::game ::move "d2d4")
              o/fire-rules))
  (->
    (o/query-all @*session ::game)
    first
    :board
    print-board)

  (o/query-all @*session ::history))
