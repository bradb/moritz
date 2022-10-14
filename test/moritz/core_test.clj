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

(deftest valid-rook-moves-test
  (let [start-pos "2k5/5p2/5n2/8/8/8/3P1R2/1K6 w - - 0 1"
        reset-pos! (fn [] (mc/start-game! start-pos))]
    (reset-pos!)
    (mc/move! "f2f4")
    (is (= "2k5/5p2/5n2/8/5R2/8/3P4/1K6 b - - 1 1" (mc/fen)) "north")

    (reset-pos!)
    (mc/move! "f2f1")
    (is (= "2k5/5p2/5n2/8/8/8/3P4/1K3R2 b - - 1 1" (mc/fen)) "south")

    (reset-pos!)
    (mc/move! "f2e2")
    (is (= "2k5/5p2/5n2/8/8/8/3PR3/1K6 b - - 1 1" (mc/fen)) "west")

    (reset-pos!)
    (mc/move! "f2h2")
    (is (= "2k5/5p2/5n2/8/8/8/3P3R/1K6 b - - 1 1" (mc/fen)) "east")

    (reset-pos!)
    (mc/move! "f2f6")
    (is (= "2k5/5p2/5R2/8/8/8/3P4/1K6 b - - 0 1" (mc/fen)) "capture enemy piece")))

(deftest invalid-rook-moves-test
  (let [start-pos "2k5/5p2/5n2/8/8/8/3P1R2/1K6 w - - 0 1"
        reset-pos! (fn [] (mc/start-game! start-pos))]
    (reset-pos!)
    (mc/move! "f2d2")
    (is (= start-pos (mc/fen)) "can't capture own piece")

    (reset-pos!)
    (mc/move! "f2f7")
    (is (= start-pos (mc/fen)) "can't capture enemy piece behind other piece")

    (reset-pos!)
    (mc/move! "f2e6")
    (is (= start-pos (mc/fen)) "can't move in non-straight line")

    (reset-pos!)
    (mc/move! "f2j9")
    (is (= start-pos (mc/fen)) "can't move off the board")))

(deftest white-queen-rook-move-removes-castling-rights-test
  (let [start-pos "r1bqkb1r/pppppppp/2n2n2/8/8/2N2N2/PPPPPPPP/R1BQKB1R w KQkq - 4 3"]
    (mc/start-game! start-pos)
    (mc/move! "a1b1")
    (is (= "r1bqkb1r/pppppppp/2n2n2/8/8/2N2N2/PPPPPPPP/1RBQKB1R b Kkq - 5 3" (mc/fen)))))

(deftest black-queen-rook-move-removes-castling-rights-test
  (let [start-pos "r1bqkb1r/pppppppp/2n2n2/8/8/2N2N2/PPPPPPPP/R1BQKB1R w KQkq - 4 3"]
    (mc/start-game! start-pos)
    (mc/move! "e2e3" "a8b8")
    (is (= "1rbqkb1r/pppppppp/2n2n2/8/8/2N1PN2/PPPP1PPP/R1BQKB1R w KQk - 1 4" (mc/fen)))))

(deftest white-king-rook-move-removes-castling-rights-test
  (let [start-pos "r1bqkb1r/pppppppp/2n2n2/8/8/2N2N2/PPPPPPPP/R1BQKB1R w KQkq - 4 3"]
    (mc/start-game! start-pos)
    (mc/move! "h1g1")
    (is (= "r1bqkb1r/pppppppp/2n2n2/8/8/2N2N2/PPPPPPPP/R1BQKBR1 b Qkq - 5 3" (mc/fen)))))

(deftest black-king-rook-move-removes-castling-rights-test
  (let [start-pos "r1bqkb1r/pppppppp/2n2n2/8/8/2N2N2/PPPPPPPP/R1BQKB1R w KQkq - 4 3"]
    (mc/start-game! start-pos)
    (mc/move! "e2e3" "h8g8")
    (is (= "r1bqkbr1/pppppppp/2n2n2/8/8/2N1PN2/PPPP1PPP/R1BQKB1R w KQq - 1 4" (mc/fen)))))

(deftest valid-king-moves-test
  (let [start-pos "8/p4b2/3k4/8/8/2K5/4PPN1/8 b - - 0 1"
        reset-pos! (fn [] (mc/start-game! start-pos))]
    (reset-pos!)
    (mc/move! "d6d5")
    (is (= "8/p4b2/8/3k4/8/2K5/4PPN1/8 w - - 1 2" (mc/fen)) "south")

    (reset-pos!)
    (mc/move! "d6d7")
    (is (= "8/p2k1b2/8/8/8/2K5/4PPN1/8 w - - 1 2" (mc/fen)) "north")

    (reset-pos!)
    (mc/move! "d6c6")
    (is (= "8/p4b2/2k5/8/8/2K5/4PPN1/8 w - - 1 2" (mc/fen)) "west")

    (reset-pos!)
    (mc/move! "d6e6")
    (is (= "8/p4b2/4k3/8/8/2K5/4PPN1/8 w - - 1 2" (mc/fen)) "east")

    (reset-pos!)
    (mc/move! "d6c5")
    (is (= "8/p4b2/8/2k5/8/2K5/4PPN1/8 w - - 1 2" (mc/fen)) "southwest")

    (reset-pos!)
    (mc/move! "d6e5")
    (is (= "8/p4b2/8/4k3/8/2K5/4PPN1/8 w - - 1 2" (mc/fen)) "southeast")

    (reset-pos!)
    (mc/move! "d6c7")
    (is (= "8/p1k2b2/8/8/8/2K5/4PPN1/8 w - - 1 2" (mc/fen)) "northwest")

    (reset-pos!)
    (mc/move! "d6e7")
    (is (= "8/p3kb2/8/8/8/2K5/4PPN1/8 w - - 1 2" (mc/fen)) "northeast")

    (reset-pos!)
    (mc/move! "d6e5" "g2f4" "e5f4")
    (is (= "8/p4b2/8/8/5k2/2K5/4PP2/8 w - - 0 3" (mc/fen)) "capture enemy piece")))

(deftest invalid-king-moves-test
  (let [start-pos "8/p4b2/3k4/8/8/3K4/4PPN1/8 w - - 0 1"]
    (mc/start-game! start-pos)

    (mc/move! "d3e2")
    (is (= start-pos (mc/fen)) "can't capture own piece")

    (mc/move! "d3d5")
    (is (= start-pos (mc/fen)) "can't move more then one square")

    (mc/move! "d3d9")
    (is (= start-pos (mc/fen)) "can't move off the board")))

(deftest king-can-castle-kingside-test
  (is false))

(deftest king-can-castle-queenside-test
  (is false))

(deftest king-cant-castle-kingside-test
  (is false))

(deftest king-cant-castle-queenside-test
  (is false))

(deftest king-cant-castle-through-check-test
  (is false))

(deftest king-cant-castle-out-of-check-test
  (is false))
