(ns infant.vizrec.recommender
  (:require [infant.vizrec.spec :as sp]
            [infant.vizrec.constraint :as constraint]
            [infant.vizrec.rank.effectiveness :as eff]))


(defn recommend
  [partial-spec]
  (->> partial-spec
       (sp/enumerate)
       (constraint/filter-invalid-specs)
       (eff/rank)
       (sort-by val >)
       (first)
       (key)))
