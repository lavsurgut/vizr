(ns infant.vizrec.rank.effectiveness.axis
  (:require [infant.vizrec.spec :as sp]))


(defn- set-name
  [type channel]
  (str type "-" channel))


(defn- init-scores
  []
  (let [non-preferred-temporal-axis ::sp/y
        non-preferred-ordinal-axis ::sp/x
        non-preferred-nominal-axis ::sp/x
        non-preferred-quantitative-axis ::sp/y
        non-preferred-axes   {::sp/temporal non-preferred-temporal-axis
                              ::sp/ordinal non-preferred-ordinal-axis
                              ::sp/nominal non-preferred-nominal-axis
                              ::sp/quantitative non-preferred-quantitative-axis}]
    (reduce (fn [x [k v]] (conj x {(set-name k v) -0.01})) {} non-preferred-axes)))


(defn measure-rank
  [spec]
  (let [scores (init-scores)
        fields (::sp/fields spec)]
    (reduce (fn [res field]
              (+ res (get scores (set-name (::sp/type field) (::sp/channel field)) 0)))
            0 fields)))
