(ns moritz.core
  (:require [odoyle.rules :as o]
            [clojure.string :as str]))

;; TODO:
;; make any move, no validation
;; ensure correct player moves
;; store moves
;; store board history
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
      [::board ::state board]]

     ::player-move
     [:what
      #_[::player ::turn player]
      [::board ::state board]
      [::game ::move move]
      :then
      (let [[from _] (move->from-to move)
            [_ piece] (square->piece board from)
            move-type (condp = piece
                        pawn ::pawn
                        ::invalid)]
        (o/insert! ::move move-type move))]

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
                                       (assoc (square->idx to) pawn))))]

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
              #_(o/insert ::game ::move "e2e4")
              o/fire-rules))
  (->
    (o/query-all @*session ::game)
    first
    :board
    print-board)
  )
