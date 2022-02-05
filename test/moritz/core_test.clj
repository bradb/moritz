(ns moritz.core-test
  (:require [moritz.core :as sut]
            [clojure.test :refer :all]))

(def ^:dynamic *board* (atom nil))

(def start-position
  ["wR" "wN" "wB" "wQ" "wK" "wB" "wN" "wR"
   "wP" "wP" "wP" "wP" "wP" "wP" "wP" "wP"
   nil  nil  nil  nil  nil  nil  nil  nil
   nil  nil  nil  nil  nil  nil  nil  nil
   nil  nil  nil  nil  nil  nil  nil  nil
   nil  nil  nil  nil  nil  nil  nil  nil
   "bP" "bP" "bP" "bP" "bP" "bP" "bP" "bP"
   "bR" "bN" "bB" "bQ" "bK" "bB" "bN" "bR"])

(defn reset-board!
  [f]
  (reset! *board* start-position)
  (f))

(defn board
  [& moves]
  @*board*)

(comment (reset-board!))

(use-fixtures :each reset-board!)

(deftest pawn-moves-one-square-test
  (is (= (board) start-position)))

(deftest pawn-moves-two-squares-test
  (is false))

(deftest pawn-cant-move-more-than-two-squares-test
  (is false))

(deftest pawn-cant-move-past-blocker-test
  (is false))
