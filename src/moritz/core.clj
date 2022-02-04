(ns moritz.core
  (:require [odoyle.rules :as o]
            [clojure.string :as str]))

;; TODO:
;; make any move, no validation
;; undo
;; store moves
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

(def ^:private rules
  (o/ruleset
    {::game
     [:what
      [::player ::turn player]
      [::white ::allow-castle? w-allow-castle?]
      [::black ::allow-castle? b-allow-castle?]
      [::board ::state board]]

     #_#_::player-move
     [:what
      [::board ::state board]
      [::game ::move move]
      :then
      ]

     ::pawn-move
     [:what
      [::board ::state board {:then false}]
      [::player ::turn player {:then false}]
      [::move ::pawn [from to]]
      :then
      (-> )
      ]

     }

    )
  )



(defn play
  [state {:keys [move]}]


  )

(comment
  (reset! *session
          (-> (reduce o/add-rule (o/->session) rules)
              (o/insert ::player ::turn white)
              (o/insert ::white ::allow-castle? true)
              (o/insert ::black ::allow-castle? true)
              (o/insert ::board ::state board)
              o/fire-rules))
  (o/query-all @*session ::game)
  )
