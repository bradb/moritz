(ns moritz.core-test
  (:require [moritz.core :as mc]
            [clojure.test :refer :all]))

(defn reset-session!
  [f]
  (mc/reset-session!)
  (mc/start-game!)
  (f))

(use-fixtures :each reset-session!)

(deftest pawn-moves-one-square-test
  (let [expected "rnbqkbnr/pppppppp/8/8/8/4P3/PPPP1PPP/RNBQKBNR b KQkq - 0 1"]
    (mc/move! "e2e3")
    (is (= expected (mc/fen)))))

(deftest pawn-moves-two-squares-test
  (let [expected "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1"]
    (mc/move! "e2e4")
    (is (= expected (mc/fen)))))

(deftest pawn-cant-move-more-than-two-squares-test
  (mc/move! "e2e5")
  (is (= mc/start-position-default (mc/fen))))

(deftest black-move-after-white-move-test
  (let [expected "rnbqkbnr/ppp1pppp/8/3p4/8/4P3/PPPP1PPP/RNBQKBNR w KQkq - 0 2"]
    (mc/move! "e2e3" "d7d5")
    (is (= expected (mc/fen)))))

(deftest pawn-can-only-move-one-square-if-already-moved
  (let [expected "rnbqkbnr/ppp1pppp/8/3p4/8/4P3/PPPP1PPP/RNBQKBNR w KQkq - 0 2"]
    (mc/move! "e2e3" "d7d5" "e3e5")
    (is (= expected (mc/fen)))))

(deftest pawn-cant-move-on-top-of-other-piece-test
  (let [expected "rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2"]
    (mc/move! "e2e4" "e7e5" "e4e5")
    (is (= expected (mc/fen)))))

(deftest pawn-cant-move-past-another-piece-test
  (let [expected "rnbqkbnr/pppp1ppp/B7/4p3/4P3/8/PPPP1PPP/RNBQK1NR b KQkq - 1 2"]
    (mc/move! "e2e4" "e7e5" "f1a6")
    (is (= expected (mc/fen)))
    (mc/move! "a7a5")
    (is (= expected (mc/fen)))))

#_(deftest capture-history-test
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

(deftest bishop-move-forward-left-white-test
  (let [expected "rnbqkbnr/pppp1ppp/B7/4p3/4P3/8/PPPP1PPP/RNBQK1NR b KQkq - 1 2"]
    (mc/move! "e2e4" "e7e5" "f1a6")
    (is (= expected (mc/fen)))))

(deftest bishop-move-forward-right-black-test
  (let [expected "rnbqk1nr/ppppbppp/8/4p3/4P3/8/PPPPBPPP/RNBQK1NR w KQkq - 2 3"]
    (mc/move! "e2e4" "e7e5" "f1e2" "f8e7")
    (is (= expected (mc/fen)))))

(deftest bishop-move-backward-left-white-test
  (let [expected "rnbqk1nr/ppppbppp/8/4p3/4P3/8/PPPPBPPP/RNBQK1NR b KQkq - 3 3"]
    (mc/move! "e2e4" "e7e5" "f1d3" "f8e7" "d3e2")
    (is (= expected (mc/fen)))))

(deftest bishop-move-backward-left-black-test
  (let [expected "rnbqk1nr/pppp1ppp/1b6/4p3/4P3/8/PPPPBPPP/RNBQK1NR w KQkq - 4 4"]
    (mc/move! "e2e4" "e7e5" "f1d3" "f8c5" "d3e2" "c5b6")
    (is (= expected (mc/fen)))))

(deftest bishop-move-forward-right-white-test
  (let [expected "rnbqk1nr/ppppbppp/8/3Bp3/4P3/8/PPPP1PPP/RNBQK1NR b KQkq - 3 3"]
    (mc/move! "e2e4" "e7e5" "f1c4" "f8e7" "c4d5")
    (is (= expected (mc/fen)))))

(deftest bishop-move-forward-left-black-test
  (let [expected "rnbqk1nr/pppp1ppp/5b2/4p3/2B1P3/3P4/PPP2PPP/RNBQK1NR w KQkq - 1 4"]
    (mc/move! "e2e4" "e7e5" "f1c4" "f8e7" "d2d3" "e7f6")
    (is (= expected (mc/fen)))))

(deftest bishop-move-backward-right-white-test
  (is false))

(deftest bishop-move-backward-right-black-test
  (is false))

(deftest bishop-can-capture-piece-test
  (is false))

(deftest bishop-cant-move-to-non-diagonal-square-test
  (is false))

(deftest bishop-cant-move-past-piece-test
  (is false))

(deftest bishop-cant-move-off-board-test
  (is false))

(deftest same-player-cant-move-twice-in-a-row-test
  (let [expected "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1"]
    (mc/move! "e2e4" "e4e5")
    (is (= expected (mc/fen)))))
