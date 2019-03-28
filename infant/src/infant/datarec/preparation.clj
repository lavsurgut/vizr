(ns infant.datarec.preparation
  (:require [infant.datarec.schema :as schema]
            [clojure.algo.generic.math-functions :as m])
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


(defn get-frequencies
  [data-by-key]
  (map (fn [k] {k (frequencies (k data-by-key))})
       (keys data-by-key)))


(defn get-modified-box-plot-outliers
  "Adjusted box plot outlier detection algorithm applied to a map of keys with values"
  [num-data-by-key]
  (let [srt-data (into {} (map (fn [k] {k (sort > (k num-data-by-key))})
                               (keys num-data-by-key)))
        medians (into {} (map (fn [k] {k (median (k srt-data))})
                              (keys srt-data)))
        scales (into {} (map (fn [k] {k (* 2 (reduce max (k srt-data)))})
                             (keys srt-data)))
        zpluses (into {} (map (fn [k]
                                {k (for [x (k srt-data)
                                         :let [xm (k medians)
                                               xscale (k scales)]
                                         :when (>= x xm)]
                                     (/ (- x xm) xscale))})
                              (keys srt-data)))
        zminuses (into {} (map (fn [k]
                                 {k (for [x (k srt-data)
                                          :let [xm (k medians)
                                                xscale (k scales)]
                                          :when (<= x xm)]
                                      (/ (- x xm) xscale))})
                               (keys srt-data)))
        p-sizes (into {} (map (fn [k] {k (count (k zpluses))})
                              (keys srt-data)))
        q-sizes (into {} (map (fn [k] {k (count (k zminuses))})
                              (keys srt-data)))
        h (fn [i j k]
            (let [a (nth (k zpluses) i)
                  b (nth (k zminuses) j)
                  p (k p-sizes)]
              (if (= a b)
                (m/sgn (- p 1 i j))
                (/ (+ a b) (- a b)))))
        h-arrs (into {} (map (fn [k]
                               {k (for [i (range 0 (k p-sizes))
                                        j (range 0 (k q-sizes))]
                                    (h i j k))})
                             (keys srt-data)))
        medcouples (into {} (map (fn [k]
                                   {k (median (k h-arrs))})
                                 (keys srt-data)))
        quartiles (into {} (map (fn [k]
                                  (let [percentiles (quantile (k srt-data))]
                                    {k [(nth percentiles 1) (nth percentiles 3)]}))
                                (keys srt-data)))
        iqrs (into {} (map (fn [[k v]] {k (- (second v) (first v))}) quartiles))
        low-border (into {} (map (fn [[k v]] (let [mc (k medcouples)]
                                               {k (- (first v)
                                                     (* 1.5 (k iqrs)
                                                        (if (>= mc 0)
                                                          (m/pow (exp 1) (* -4 mc))
                                                          (m/pow (exp 1) (* -3 mc)))))})) quartiles))
        high-border (into {} (map (fn [[k v]] (let [mc (k medcouples)]
                                                {k (+ (second v)
                                                      (* 1.5 (k iqrs)
                                                         (if (>= mc 0)
                                                           (m/pow (exp 1) (* 3 mc))
                                                           (m/pow (exp 1) (* 4 mc)))))})) quartiles))
        outliers-by-key (map (fn [[k vals]]
                               {k (filter (fn [el]
                                            (or (< el (k low-border))
                                                (> el (k high-border)))) vals)}) srt-data)]
    outliers-by-key))


(defn get-euclidean-distance-from-mean
  [data-by-key]
  (map (fn [[k arr]]
         {k (let [mn (mean arr)]
              (for [el arr]
                (euclidean-distance [mn 0] [el 0])))}) data-by-key))


(defn prepare-data
  [data]
  (let [data-by-key (get-data-by-key data)
        schema (schema/get-schema data)
        numeric-data-by-key (get-numeric-data data-by-key schema)
        outliers (into {} (get-modified-box-plot-outliers numeric-data-by-key))
        euclidean-distance-from-mean (into {} (get-euclidean-distance-from-mean numeric-data-by-key))
        res (assoc {} :outliers outliers)
        res (assoc res :euclidean-distance-from-mean euclidean-distance-from-mean)]
    res))
