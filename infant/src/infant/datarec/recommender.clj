(ns infant.datarec.recommender
  (:require [infant.datarec.schema :as schema])
  (:use (incanter core stats)))


(defn get-data-by-key
  [data]
  (into {} (map (fn [k]
          {k (reduce
               (fn [x y] (if (k y)
                           (conj x (k y))
                           x)) [] data)})
        (keys (first data)))))


(defn get-numeric-data
  [data-by-key schema]
  (->>
    (filter (fn [[_ v]] (= (:type v) java.lang.Long)) schema)
    (reduce (fn [coll [k _]]
              (assoc coll k (k data-by-key))) {})))


(defn get-frequences
  [data-by-key]
  (map (fn [k] {k (frequencies (k data-by-key))})
       (keys data-by-key)))


(defn get-outliers
  [data-by-key]
  (let [quartiles (into {} (map (fn [k]
                                  (let [percentiles (quantile (k data-by-key))]
                                    {k [(nth percentiles 1) (nth percentiles 3)]}))
                                (keys data-by-key)))
        iqrs (into {} (map (fn [[k v]] {k (- (second v) (first v))}) quartiles))
        low-border (into {} (map (fn [[k v]] {k (- (first v)
                                                   (* 1.5 (k iqrs)))}) quartiles))
        high-border (into {} (map (fn [[k v]] {k (+ (second v)
                                                    (* 1.5 (k iqrs)))}) quartiles))
        outliers-by-key (map (fn [[k vals]]
                               {k (filter (fn [el]
                                            (or (< el (k low-border))
                                                (> el (k high-border)))) vals)}) data-by-key)]
    outliers-by-key))
