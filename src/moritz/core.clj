(ns moritz.core
  (:require [odoyle.rules :as o]))

(def ^:private ^:const files 8)

(defn- print-board
  [board]
  (println)
  (doseq [row (partition files board)]
    (println (clojure.string/join " " row)))
  (println))

(def rules
  (o/ruleset
    {::board-state
     [:what
      [::board ::state board-state]]}))

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
               (o/insert ::board ::move m)))))

(defn get-board [s]
  (o/query-all s ::board ::state))

(comment
  (swap! *session
         (fn [session]
           (-> session
               (o/insert ::board ::state board)
               o/fire-rules)))
  (o/query-all @*session ::board-state)
  (let [new-state (move *session "e4")]
    (board new-state)))
