(ns moritz.core
  (:require [odoyle.rules :as o]
            [clojure.string :as str]))

(def ^:private ^:const files 8)

(defn- print-board
  [board]
  (println)
  (doseq [row (partition files board)]
    (println (clojure.string/join " " row)))
  (println))

(defn- move->piece
  [m]
  (let [c (first m)
        c->p {\R :rook
              \N :knight
              \B :bishop
              \Q :queen
              \K :king
              \r :rook
              \n :knight
              \b :bishop
              \q :queen
              \k :king}]
    (get c->p c :pawn)))

(defn- move->to
  [m]
  (let [what (move->piece m)]
    (if (= what :pawn)
      m
      (subs m 1))))

(defn- move->from
  [player what to]
  (case what
    :pawn ()
    (throw (Exception. (format "don't know what %s is" (name what))))))

(defn move->map
  [bs move]
  (let [what (move->piece move)
        from (move->from move)
        to (move->to move)]
    {:what what, :from from, :to to}))

(defn- square->idx
  [square]
  (let [[file rank] (str/split square #"")
        x (.indexOf "abcdefgh" file)
        y (* 8 (- (Integer/parseInt rank) 1))]
    (+ x y)))

(defn- char->piece
  [c]
  (case (str/lower-case c)
    "r" :rook
    "n" :knight
    "b" :bishop
    "q" :queen
    "k" :king
    "p" :pawn
    nil))

(defn square->allowed-moves
  [bs square]
  (let [idx (square->idx square)
        piece-char (nth bs idx)
        piece (char->piece piece-char)]
    piece)
  )

(comment
  (square->allowed-moves board "e2"))


(def rules
  (o/ruleset
    {::board-state
     [:what
      [::board ::state board-state]]

     ::game-move
     [:what
      [::game ::move move]
      :then
      (let [{:keys [what from to]} (move->map move)]
        (o/insert! ::move what [from to]))]}))

(def *session
  (atom (reduce o/add-rule (o/->session) rules)))

(def ^:private board
  [\R \N \B \Q \K \B \N \R
   \P \P \P \P \P \P \P \P
   \. \. \. \. \. \. \. \.
   \. \. \. \. \. \. \. \.
   \. \. \. \. \. \. \. \.
   \. \. \. \. \. \. \. \.
   \p \p \p \p \p \p \p \p
   \r \n \b \q \k \b \n \r])

(defn- prompt
  [msg]
  (print msg)
  (read-line))

(defn run [opts]
  (print-board @board)
  (let [move (prompt "Your move ('q' to quit): ")]
    (println "your move is" move)))

(defn move [s m]
  (swap! s
         (fn [session]
           (-> session
               (o/insert ::game ::move m)
               o/fire-rules))))

(defn get-board [s]
  (-> @s
      (o/query-all ::board-state)
      first
      :board-state))

(comment
  (swap! *session
         (fn [session]
           (-> session
               (o/insert ::board ::state board)
               o/fire-rules)))

  (let [go (fn [m]
             (move *session m)
             (-> *session get-board print-board))]
    (go "e4"))

  (get-board *session)
  (print)
  (let [new-state (move *session "e4")]
    (board new-state)))
