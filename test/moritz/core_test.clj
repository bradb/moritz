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

(deftest history-test
  (is (= ["rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"] (mc/history)))

  (mc/move! "e2e4")
  (is (= ["rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
          "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1"] (mc/history)))

  (mc/move! "e7e5")
  (is (= ["rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
          "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1"
          "rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2"] (mc/history))))

(deftest bishop-move-north-west-white-test
  (let [expected "rnbqkbnr/pppp1ppp/B7/4p3/4P3/8/PPPP1PPP/RNBQK1NR b KQkq - 1 2"]
    (mc/move! "e2e4" "e7e5" "f1a6")
    (is (= expected (mc/fen)))))

(deftest bishop-move-north-east-white-test
  (let [expected "rnbqk1nr/ppppbppp/8/3Bp3/4P3/8/PPPP1PPP/RNBQK1NR b KQkq - 3 3"]
    (mc/move! "e2e4" "e7e5" "f1c4" "f8e7" "c4d5")
    (is (= expected (mc/fen)))))

(deftest bishop-move-south-east-white-test
  (let [expected "rnbqk1nr/ppppbppp/8/4p3/4P3/8/PPPPBPPP/RNBQK1NR b KQkq - 3 3"]
    (mc/move! "e2e4" "e7e5" "f1d3" "f8e7" "d3e2")
    (is (= expected (mc/fen)))))

(deftest bishop-move-south-west-white-test
  (let [expected "rnbqk1nr/ppppbppp/8/4p3/4P3/1B6/PPPP1PPP/RNBQK1NR b KQkq - 3 3"]
    (mc/move! "e2e4" "e7e5" "f1c4" "f8e7" "c4b3")
    (is (= expected (mc/fen)))))

(deftest bishop-move-south-west-black-test
  (let [expected "rnbqk1nr/ppppbppp/8/4p3/4P3/8/PPPPBPPP/RNBQK1NR w KQkq - 2 3"]
    (mc/move! "e2e4" "e7e5" "f1e2" "f8e7")
    (is (= expected (mc/fen)))))

(deftest bishop-move-south-east-black-test
  (let [expected "rnbqk1nr/pppp1ppp/5b2/4p3/2B1P3/3P4/PPP2PPP/RNBQK1NR w KQkq - 1 4"]
    (mc/move! "e2e4" "e7e5" "f1c4" "f8e7" "d2d3" "e7f6")
    (is (= expected (mc/fen)))))

(deftest bishop-move-north-west-black-test
  (let [expected "rnbqk1nr/pppp1ppp/1b6/4p3/4P3/8/PPPPBPPP/RNBQK1NR w KQkq - 4 4"]
    (mc/move! "e2e4" "e7e5" "f1d3" "f8c5" "d3e2" "c5b6")
    (is (= expected (mc/fen)))))

(deftest bishop-move-north-east-black-test
  (let [expected "rnbqkbnr/pppp1ppp/8/4p3/4P3/1B6/PPPP1PPP/RNBQK1NR w KQkq - 4 4"]
    (mc/move! "e2e4" "e7e5" "f1c4" "f8e7" "c4b3" "e7f8")
    (is (= expected (mc/fen)))))

(deftest bishop-can-capture-piece-test
  (let [expected "rnbqkb1r/ppp1pppp/3p1B2/8/3P4/8/PPP1PPPP/RN1QKBNR b KQkq - 0 3"]
    (mc/move! "d2d4" "g8f6" "c1g5" "d7d6" "g5f6")
    (is (= expected (mc/fen)))))

(deftest bishop-cant-move-to-non-diagonal-square-test
  (let [expected "rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2"]
    (mc/move! "e2e4" "e7e5" "f1f2")
    (is (= expected (mc/fen)))))

(deftest bishop-cant-move-past-piece-test
  (let [expected "rnbqk1nr/pppp1ppp/4p3/8/1b1P4/8/PPPBPPPP/RN1QKBNR w KQkq - 2 3"]
    (mc/move! "d2d4" "e7e6" "c1d2" "f8b4" "d2a5")
    (is (= expected (mc/fen)))))

(deftest bishop-cant-move-off-board-test
  (let [expected "rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2"]
    (mc/move! "e2e4" "e7e5" "f1i4")
    (is (= expected (mc/fen)))))

(deftest same-player-cant-move-twice-in-a-row-test
  (let [expected "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1"]
    (mc/move! "e2e4" "e4e5")
    (is (= expected (mc/fen)))))

(deftest kingside-knight-move-test
  (let [expected "rnbqkb1r/pppppppp/5n2/8/3P4/8/PPP1PPPP/RNBQKBNR w KQkq - 1 2"]
    (mc/move! "d2d4" "g8f6")
    (is (= expected (mc/fen)))))

(defn- init-queen-test!
  []
  (mc/start-game! "r1bqkbnr/pppp1ppp/2n5/4p3/4P1Q1/8/PPPP1PPP/RNB1KBNR w KQkq - 2 3"))

(deftest queen-cant-make-knight-move-test
  (init-queen-test!)
  (let [expected "r1bqkbnr/pppp1ppp/2n5/4p3/4P1Q1/8/PPPP1PPP/RNB1KBNR w KQkq - 2 3"]
    (mc/move! "g4f6")
    (is (= expected (mc/fen)))))

(deftest queen-move-east-test
  (init-queen-test!)
  (let [expected "r1bqkbnr/pppp1ppp/2n5/4p3/4P2Q/8/PPPP1PPP/RNB1KBNR b KQkq - 3 3"]
    (mc/move! "g4h4")
    (is (= expected (mc/fen)))))

(deftest queen-move-west-test
  (init-queen-test!)
  (let [expected "r1bqkbnr/pppp1ppp/2n5/4p3/4PQ2/8/PPPP1PPP/RNB1KBNR b KQkq - 3 3"]
    (mc/move! "g4f4")
    (is (= expected (mc/fen)))))

(deftest queen-move-north-test
  (init-queen-test!)
  (let [expected "r1bqkbnr/pppp1ppp/2n5/4p1Q1/4P3/8/PPPP1PPP/RNB1KBNR b KQkq - 3 3"]
    (mc/move! "g4g5")
    (is (= expected (mc/fen)))))

(deftest queen-move-south-test
  (init-queen-test!)
  (let [expected "r1bqkbnr/pppp1ppp/2n5/4p3/4P3/6Q1/PPPP1PPP/RNB1KBNR b KQkq - 3 3"]
    (mc/move! "g4g3")
    (is (= expected (mc/fen)))))

(deftest queen-move-north-east-test
  (init-queen-test!)
  (let [expected "r1bqkbnr/pppp1ppp/2n5/4p2Q/4P3/8/PPPP1PPP/RNB1KBNR b KQkq - 3 3"]
    (mc/move! "g4h5")
    (is (= expected (mc/fen)))))

(deftest queen-move-north-west-test
  (init-queen-test!)
  (let [expected "r1bqkbnr/pppp1ppp/2n5/4pQ2/4P3/8/PPPP1PPP/RNB1KBNR b KQkq - 3 3"]
    (mc/move! "g4f5")
    (is (= expected (mc/fen)))))

(deftest queen-move-south-east-test
  (init-queen-test!)
  (let [expected "r1bqkbnr/pppp1ppp/2n5/4p3/4P3/7Q/PPPP1PPP/RNB1KBNR b KQkq - 3 3"]
    (mc/move! "g4h3")
    (is (= expected (mc/fen)))))

(deftest queen-move-south-west-test
  (init-queen-test!)
  (let [expected "r1bqkbnr/pppp1ppp/2n5/4p3/4P3/5Q2/PPPP1PPP/RNB1KBNR b KQkq - 3 3"]
    (mc/move! "g4f3")
    (is (= expected (mc/fen)))))

(deftest queen-can-capture-piece-test
  (init-queen-test!)
  (let [expected "r1bqkbnr/pppp1pQp/2n5/4p3/4P3/8/PPPP1PPP/RNB1KBNR b KQkq - 0 3"]
    (mc/move! "g4g7")
    (is (= expected (mc/fen)))))

(deftest queen-cant-capture-piece-of-own-colour-test
  (init-queen-test!)
  (let [expected "r1bqkbnr/pppp1ppp/2n5/4p3/4P1Q1/8/PPPP1PPP/RNB1KBNR w KQkq - 2 3"]
    (mc/move! "g4g2")
    (is (= expected (mc/fen)))))

(deftest queen-cant-capture-piece-blocked-by-other-piece-test
  (init-queen-test!)
  (let [expected "r1bqkbnr/pppp1ppp/2n5/4p3/4P1Q1/8/PPPP1PPP/RNB1KBNR w KQkq - 2 3"]
    (mc/move! "g4g8")
    (is (= expected (mc/fen)))))

(deftest valid-knight-moves-test
  (let [start-pos "3k4/1p6/8/8/5N2/8/8/2K5 w - - 0 1"]
    (mc/start-game! start-pos)
    (mc/move! "f4d3")
    (is (= "3k4/1p6/8/8/8/3N4/8/2K5 b - - 1 1" (mc/fen)))

    (mc/start-game! start-pos)
    (mc/move! "f4d5")
    (is (= "3k4/1p6/8/3N4/8/8/8/2K5 b - - 1 1" (mc/fen)))

    (mc/start-game! start-pos)
    (mc/move! "f4e2")
    (is (= "3k4/1p6/8/8/8/8/4N3/2K5 b - - 1 1" (mc/fen)))

    (mc/start-game! start-pos)
    (mc/move! "f4e6")
    (is (= "3k4/1p6/4N3/8/8/8/8/2K5 b - - 1 1" (mc/fen)))

    (mc/start-game! start-pos)
    (mc/move! "f4g6")
    (is (= "3k4/1p6/6N1/8/8/8/8/2K5 b - - 1 1" (mc/fen)))

    (mc/start-game! start-pos)
    (mc/move! "f4g2")
    (is (= "3k4/1p6/8/8/8/8/6N1/2K5 b - - 1 1" (mc/fen)))

    (mc/start-game! start-pos)
    (mc/move! "f4h5")
    (is (= "3k4/1p6/8/7N/8/8/8/2K5 b - - 1 1" (mc/fen)))

    (mc/start-game! start-pos)
    (mc/move! "f4h3")
    (is (= "3k4/1p6/8/8/8/7N/8/2K5 b - - 1 1" (mc/fen)))))

(deftest knight-cant-move-straight-test
  (let [start "rnbqkbnr/ppp1pppp/8/3p4/8/5N2/PPPPPPPP/RNBQKB1R w KQkq - 0 2"]
    (mc/start-game! start)
    (mc/move! "f3h3")
    (is (= start (mc/fen)))))

(deftest knight-can-capture-opponents-piece-test
  (mc/start-game! "rnbqkbnr/pppp1ppp/8/4p3/8/5N2/PPPPPPPP/RNBQKB1R w KQkq - 0 2")
  (mc/move! "f3e5")
  (is (= "rnbqkbnr/pppp1ppp/8/4N3/8/8/PPPPPPPP/RNBQKB1R b KQkq - 0 2" (mc/fen))))

(deftest knight-cant-capture-own-colour-piece-test
  (let [start-pos "rnbqkbnr/pppp1ppp/8/4p3/8/5N2/PPPPPPPP/RNBQKB1R w KQkq - 0 2"]
    (mc/start-game! start-pos)
    (mc/move! "f3h2")
    (is (= start-pos (mc/fen)))))

(deftest knight-can-jump-over-own-pieces-test
  (mc/start-game!)
  (mc/move! "g1f3")
  (is (= "rnbqkbnr/pppppppp/8/8/8/5N2/PPPPPPPP/RNBQKB1R b KQkq - 1 1" (mc/fen))))

(deftest knight-cant-move-off-edge-of-board-test
  (mc/start-game!)
  (mc/move! "g1i2")
  (is (= "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" (mc/fen))))

(comment 
  (deftest rook-move-east
    (let [expected "rnbqkbnr/pppp1ppp/4p3/8/8/2N5/PPPPPPPP/1RBQKBNR b Kkq - 1 2"]
      (mc/move! "b1c3" "e7e6" "a1b1")
      (is (= expected (mc/fen)))))

  (deftest rook-move-west
    (is false))

  (deftest rook-move-north
    (is false))

  (deftest rook-move-south
    (is false))

  (deftest rook-cant-move-past-piece
    (is false))

  (deftest rook-capture-opposite-colour-piece-test
    (is false))

  (deftest rook-cant-capture-own-colour-piece-test
    (is false)))
