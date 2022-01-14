(ns moritz.core
  (:require [odoyle.rules :as o]))

(defn- print-board
  [board]
  (println)
  (doseq [row (partition files board)]
    (println (clojure.string/join " " row)))
  (println))

(def rules
  (o/ruleset
    {::print-board
     [:what
      [::board ::state bs]
      :then
      (print-board bs)]

     ::pawn-move
     [:what
      [::pawn at to]]}

    ))

(def ^:private board
  [\r \n \b \q \k \b \n \r
   \p \p \p \p \p \p \p \p
   \. \. \. \. \. \. \. \.
   \. \. \. \. \. \. \. \.
   \. \. \. \. \. \. \. \.
   \. \. \. \. \. \. \. \.
   \P \P \P \P \P \P \P \P
   \R \N \B \Q \K \B \N \R])

(def ^:private ^:const files 8)

(defn- prompt
  [msg]
  (print msg)
  (read-line))

(defn run [opts]
  (print-board @board)
  (let [move (prompt "Your move ('q' to quit): ")]
    (println "your move is" move)))

(def *session
  (atom (reduce o/add-rule (o/->session) rules)))

(comment
  (swap! *session
         (fn [session]
           (-> session
               (o/insert ::board ::state board)
               o/fire-rules))))
