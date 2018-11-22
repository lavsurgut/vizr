(ns infant.rank.facet
  (:require [infant.spec.spec :as sp]))

(def preferred-facet ::sp/row)

(defn init-scores
  []
  (if (= preferred-facet ::sp/row)
    {::sp/column -0.01}
    {::sp/row -0.01}))

(defn get-score
  [spec]
  (let [scores (init-scores)
        fields (::sp/fields spec)]
    (reduce (fn [x y]
              (+ x (get scores (::sp/channel y) 0)))
            0 fields)))
