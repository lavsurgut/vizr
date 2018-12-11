(ns infant.vizrec.rank.axis
  (:require [infant.vizrec.spec :as sp]))

(def preferred-bin-axis ::sp/x)
(def preferred-temporal-axis ::sp/x)
(def preferred-ordinal-axis ::sp/y)
(def preferred-nominal-axis ::sp/y)
(def preferred-facet ::sp/row)

(def preferred-axes   {::sp/temporal preferred-temporal-axis
                       ::sp/ordinal preferred-ordinal-axis
                       ::sp/nominal preferred-nominal-axis})

(defn set-name
  [type channel]
  (str type "-" channel))

(defn init-scores
  [preferred-axes]
  (reduce (fn [x [k v]] (conj x {(set-name k v) -0.01})) {} preferred-axes))

(defn get-score
  [spec]
  (let [scores (init-scores preferred-axes)
        fields (::sp/fields spec)]
    (reduce (fn [x y]
              (+ x (get scores (set-name (::sp/type y) (::sp/channel y)) 0)))
            0 fields)))
