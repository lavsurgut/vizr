(ns infant.vizrec.rank.axis
  (:require [infant.vizrec.spec :as sp]))


(defn- set-name
  [type channel]
  (str type "-" channel))


(defn- init-scores
  []
  (let [preferred-temporal-axis ::sp/x
        preferred-ordinal-axis ::sp/y
        preferred-nominal-axis ::sp/y
        preferred-axes   {::sp/temporal preferred-temporal-axis
                          ::sp/ordinal preferred-ordinal-axis
                          ::sp/nominal preferred-nominal-axis}]
    (reduce (fn [x [k v]] (conj x {(set-name k v) -0.01})) {} preferred-axes)))


(defn get-score
  [spec]
  (let [scores (init-scores)
        fields (::sp/fields spec)]
    (reduce (fn [x y]
              (+ x (get scores (set-name (::sp/type y) (::sp/channel y)) 0)))
            0 fields)))
