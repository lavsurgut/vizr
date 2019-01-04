(ns infant.vizrec.rank.effectiveness.facet
  (:require [infant.vizrec.spec :as sp]))

(defn- init-scores
  []
  (let [preferred-facet ::sp/row]
    (if (= preferred-facet ::sp/row)
     {::sp/column -0.01}
     {::sp/row -0.01})))

(defn measure-rank
  [spec]
  (let [scores (init-scores)
        fields (::sp/fields spec)]
    (reduce (fn [x y]
              (+ x (get scores (::sp/channel y) 0)))
            0 fields)))
