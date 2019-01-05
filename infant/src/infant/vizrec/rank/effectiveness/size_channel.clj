(ns infant.vizrec.rank.effectiveness.size-channel
  (:require [infant.vizrec.spec :as sp]))


(defn- set-name
  [mark channel]
  (str mark "-" channel))


(defn measure-rank
  [spec]
  (let [fields (::sp/fields spec)
        mark (::sp/mark spec)
        scores {(set-name ::sp/bar ::sp/size)  -2
                (set-name ::sp/tick ::sp/size) -2}
        initial-score 0]
    (reduce (fn [x y]
              (+ x (get scores (set-name mark (::sp/channel y)) initial-score)))
            initial-score fields)))
