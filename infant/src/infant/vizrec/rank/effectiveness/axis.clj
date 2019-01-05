(ns infant.vizrec.rank.effectiveness.axis
  (:require [infant.vizrec.spec :as sp]))


(defn- set-name
  [type channel]
  (str type "-" channel))


(defn- init-scores
  []
  (let [unpreferred-temporal-axis ::sp/y
        unpreferred-ordinal-axis ::sp/x
        unpreferred-nominal-axis ::sp/x
        unpreferred-axes   {::sp/temporal unpreferred-temporal-axis
                          ::sp/ordinal unpreferred-ordinal-axis
                          ::sp/nominal unpreferred-nominal-axis}]
    (reduce (fn [x [k v]] (conj x {(set-name k v) -0.01})) {} unpreferred-axes)))


(defn measure-rank
  [spec]
  (let [scores (init-scores)
        fields (::sp/fields spec)]
    (reduce (fn [x y]
              (+ x (get scores (set-name (::sp/type y) (::sp/channel y)) 0)))
            0 fields)))
