(ns infant.vizrec.rank.effectiveness
  (:require
    [infant.vizrec.rank.effectiveness.axis :as axis]
    [infant.vizrec.rank.effectiveness.dimension :as dimension]
    [infant.vizrec.rank.effectiveness.facet :as facet]
    [infant.vizrec.rank.effectiveness.mark :as mark]
    [infant.vizrec.rank.effectiveness.size-channel :as size-channel]
    [infant.vizrec.rank.effectiveness.type-channel :as type-channel]))


(defn rank [specs]
  (reduce (fn [res spec]
            (assoc res spec (reduce
                            (fn [res rank-func] (+ res (rank-func spec)))
                            0
                            [ axis/measure-rank
                             dimension/measure-rank
                             facet/measure-rank
                             mark/measure-rank
                             size-channel/measure-rank
                             type-channel/measure-rank])))
          {}
          specs)
  )
