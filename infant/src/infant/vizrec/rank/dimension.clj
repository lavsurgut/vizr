(ns infant.vizrec.rank.dimension
  (:require [infant.vizrec.spec :as sp]))


(defn get-score
  [spec]
  (let [fields (::sp/fields spec)
        initial-score 0
        scores {::sp/row -2
                ::sp/column -2
                ::sp/color 0
                ::sp/size 0}]
    (if (sp/is-aggregate? spec)
      (reduce (fn [x y]
               (+ x (if (not (::sp/aggregate? y))
                      (get scores (::sp/channel y) initial-score)
                      initial-score)))
             initial-score fields)
      initial-score)))
