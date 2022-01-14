(ns moritz.core
  (:require [odoyle.rules :as o]))

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
              \k :king
              }]
    (get c->p c :pawn)))

(def move->map
  [bs move]
  (let [what ])
  )

(def rules
  (o/ruleset
    {::board-state
     [:what
      [::board ::state board-state]]

     ::game-move
     [:what
      [::game ::move move]
      :then
      (let [{:keys [what from to] (move->map move)}]
        (o/insert! ))]}))

(def *session
  (atom (reduce o/add-rule (o/->session) rules)))

(def ^:private board
  [\r \n \b \q \k \b \n \r
   \p \p \p \p \p \p \p \p
   \. \. \. \. \. \. \. \.
   \. \. \. \. \. \. \. \.
   \. \. \. \. \. \. \. \.
   \. \. \. \. \. \. \. \.
   \P \P \P \P \P \P \P \P
   \R \N \B \Q \K \B \N \R])

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
