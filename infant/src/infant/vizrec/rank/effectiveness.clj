(ns infant.vizrec.rank.effectiveness
  (:require
    [infant.vizrec.rank.effectiveness.axis :as axis]
    [infant.vizrec.rank.effectiveness.dimension :as dimension]
    [infant.vizrec.rank.effectiveness.facet :as facet]
    [infant.vizrec.rank.effectiveness.mark :as mark]))


(defn rank [specs]
  (reduce (fn [res spec]
            (reduce
              (fn [res rank-func] (+ res (rank-func spec)))
              res
              [axis/measure-rank
               dimension/measure-rank
               facet/measure-rank
               mark/measure-rank]))
          0
          specs)
  )
