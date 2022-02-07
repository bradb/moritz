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

(use-fixtures :each reset-session!)

(deftest pawn-moves-one-square-test
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
  (let [expected ["wR" "wN" "wB" "wQ" "wK" "wB" "wN" "wR"
                  "wP" "wP" "wP" "wP" nil  "wP" "wP" "wP"
                  nil  nil  nil  nil  nil  nil  nil  nil
                  nil  nil  nil  nil  "wP" nil  nil  nil
                  nil  nil  nil  nil  nil  nil  nil  nil
                  nil  nil  nil  nil  nil  nil  nil  nil
                  "bP" "bP" "bP" "bP" "bP" "bP" "bP" "bP"
                  "bR" "bN" "bB" "bQ" "bK" "bB" "bN" "bR"]]
    (mc/move! "e2e4")
    (is (= (mc/board) expected))))

(deftest pawn-cant-move-more-than-two-squares-test
  (mc/move! "e2e5")
  (is (= (mc/board) start-position)))

(deftest black-move-after-white-move-test
  (let [expected ["wR" "wN" "wB" "wQ" "wK" "wB" "wN" "wR"
                  "wP" "wP" "wP" "wP" nil  "wP" "wP" "wP"
                  nil  nil  nil  nil  "wP" nil  nil  nil
                  nil  nil  nil  nil  nil  nil  nil  nil
                  nil  nil  nil  "bP" nil  nil  nil  nil
                  nil  nil  nil  nil  nil  nil  nil  nil
                  "bP" "bP" "bP" nil  "bP" "bP" "bP" "bP"
                  "bR" "bN" "bB" "bQ" "bK" "bB" "bN" "bR"]]
    (mc/move! "e2e3" "d7d5")
    (is (= (mc/board) expected))))

(deftest pawn-can-only-move-one-square-if-already-moved
  (let [expected ["wR" "wN" "wB" "wQ" "wK" "wB" "wN" "wR"
                  "wP" "wP" "wP" "wP" nil  "wP" "wP" "wP"
                  nil  nil  nil  nil  "wP" nil  nil  nil
                  nil  nil  nil  nil  nil  nil  nil  nil
                  nil  nil  nil  "bP" nil  nil  nil  nil
                  nil  nil  nil  nil  nil  nil  nil  nil
                  "bP" "bP" "bP" nil  "bP" "bP" "bP" "bP"
                  "bR" "bN" "bB" "bQ" "bK" "bB" "bN" "bR"]]
    (mc/move! "e2e3" "d7d5" "e3e5")
    (is (= (mc/board) expected))))

(deftest pawn-cant-move-on-top-of-other-piece-test
  (let [expected ["wR" "wN" "wB" "wQ" "wK" "wB" "wN" "wR"
                  "wP" "wP" "wP" "wP" nil  "wP" "wP" "wP"
                  nil  nil  nil  nil  nil  nil  nil  nil
                  nil  nil  nil  nil  "wP" nil  nil  nil
                  nil  nil  nil  nil  "bP"  nil  nil  nil
                  nil  nil  nil  nil  nil  nil  nil  nil
                  "bP" "bP" "bP" "bP" nil "bP" "bP" "bP"
                  "bR" "bN" "bB" "bQ" "bK" "bB" "bN" "bR"]]
    (mc/move! "e2e4" "e7e5" "e4e5")
    (is (= (mc/board) expected))))

(deftest pawn-cant-move-past-another-piece-test
  (let [expected ["wR" "wN" "wB" "wQ" "wK" nil  "wN" "wR"
                  "wP" "wP" "wP" "wP" nil  "wP" "wP" "wP"
                  nil  nil  nil  nil  nil  nil  nil  nil
                  nil  nil  nil  nil  "wP" nil  nil  nil
                  nil  nil  nil  nil  "bP"  nil  nil  nil
                  "wB" nil  nil  nil  nil  nil  nil  nil
                  "bP" "bP" "bP" "bP" nil "bP" "bP" "bP"
                  "bR" "bN" "bB" "bQ" "bK" "bB" "bN" "bR"]]
    (mc/move! "e2e4" "e7e5" "f1a6")
    (is (= (mc/board) expected))
    (mc/move! "a7a5")
    (is (= (mc/board) expected))))

(deftest capture-history-test
  (is (nil? (mc/history)))

  (let [expected-board-1 ["wR" "wN" "wB" "wQ" "wK" "wB" "wN" "wR"
                        "wP" "wP" "wP" "wP" nil  "wP" "wP" "wP"
                        nil  nil  nil  nil  nil  nil  nil  nil
                        nil  nil  nil  nil  "wP" nil  nil  nil
                        nil  nil  nil  nil  nil  nil  nil  nil
                        nil  nil  nil  nil  nil  nil  nil  nil
                        "bP" "bP" "bP" "bP" "bP" "bP" "bP" "bP"
                        "bR" "bN" "bB" "bQ" "bK" "bB" "bN" "bR"]
        first-entry {:move "e2e4"
                     :move-number 1
                     :player mc/white
                     :board expected-board-1}

        expected-board-2 ["wR" "wN" "wB" "wQ" "wK" "wB" "wN" "wR"
         "wP" "wP" "wP" "wP" nil  "wP" "wP" "wP"
         nil  nil  nil  nil  nil  nil  nil  nil
         nil  nil  nil  nil  "wP" nil  nil  nil
         nil  nil  nil  "bP" nil  nil  nil  nil
         nil  nil  nil  nil  nil  nil  nil  nil
         "bP" "bP" "bP" nil  "bP" "bP" "bP" "bP"
         "bR" "bN" "bB" "bQ" "bK" "bB" "bN" "bR"]
        second-entry {:move "d7d5"
                      :move-number 1
                      :player mc/black
                      :board expected-board-2}

        expected-board-3 ["wR" "wN" "wB" "wQ" "wK" "wB" nil  "wR"
                          "wP" "wP" "wP" "wP" nil  "wP" "wP" "wP"
                          nil  nil  nil  nil  nil  "wN" nil  nil
                          nil  nil  nil  nil  "wP" nil  nil  nil
                          nil  nil  nil  "bP" nil  nil  nil  nil
                          nil  nil  nil  nil  nil  nil  nil  nil
                          "bP" "bP" "bP" nil  "bP" "bP" "bP" "bP"
                          "bR" "bN" "bB" "bQ" "bK" "bB" "bN" "bR"]

        third-entry {:move "g1f3"
                     :move-number 2
                     :player mc/white
                     :board expected-board-3}]
    (mc/move! "e2e4")
    (is (= [first-entry] (mc/history)))

    (mc/move! "d7d5")
    (is (= [first-entry second-entry] (mc/history)))

    (mc/move! "g1f3")
    (is (= [first-entry second-entry third-entry] (mc/history)))))

(deftest bishop-move-test
  (is false))

(deftest same-player-cant-move-twice-in-a-row-test
  (let [expected ["wR" "wN" "wB" "wQ" "wK" "wB" "wN" "wR"
                  "wP" "wP" "wP" "wP" nil  "wP" "wP" "wP"
                  nil  nil  nil  nil  nil  nil  nil  nil
                  nil  nil  nil  nil  "wP" nil  nil  nil
                  nil  nil  nil  nil  nil  nil  nil  nil
                  nil  nil  nil  nil  nil  nil  nil  nil
                  "bP" "bP" "bP" "bP" "bP" "bP" "bP" "bP"
                  "bR" "bN" "bB" "bQ" "bK" "bB" "bN" "bR"]]
    (mc/move! "e2e4" "e4e5")
    (is (= (mc/board) expected))))
