(ns moritz.core-test
  (:require [moritz.core :as mc]
            [clojure.test :refer :all]
            [odoyle.rules :as o]))

(def start-position
  ["wR" "wN" "wB" "wQ" "wK" "wB" "wN" "wR"
   "wP" "wP" "wP" "wP" "wP" "wP" "wP" "wP"
   nil  nil  nil  nil  nil  nil  nil  nil
   nil  nil  nil  nil  nil  nil  nil  nil
   nil  nil  nil  nil  nil  nil  nil  nil
   nil  nil  nil  nil  nil  nil  nil  nil
   "bP" "bP" "bP" "bP" "bP" "bP" "bP" "bP"
   "bR" "bN" "bB" "bQ" "bK" "bB" "bN" "bR"])

(defn reset-session!
  [f]
  (mc/reset-session!)
  (mc/start-game!)
  (f))

(comment
  (reset-session! #())
  (mc/start-game!)
  (o/query-all *session)
  (mc/board))

(use-fixtures :each reset-session!)

(deftest pawn-moves-one-square-test
  (is (= (mc/board) start-position))

  (let [expected ["wR" "wN" "wB" "wQ" "wK" "wB" "wN" "wR"
                  "wP" "wP" "wP" "wP" nil  "wP" "wP" "wP"
                  nil  nil  nil  nil  "wP" nil  nil  nil
                  nil  nil  nil  nil  nil  nil  nil  nil
                  nil  nil  nil  nil  nil  nil  nil  nil
                  nil  nil  nil  nil  nil  nil  nil  nil
                  "bP" "bP" "bP" "bP" "bP" "bP" "bP" "bP"
                  "bR" "bN" "bB" "bQ" "bK" "bB" "bN" "bR"]]
    (mc/move! "e2e3")
    (is (= (mc/board) expected))))

(deftest pawn-moves-two-squares-test
  (is false))

(deftest pawn-cant-move-more-than-two-squares-test
  (is false))

(deftest pawn-cant-move-past-blocker-test
  (is false))
