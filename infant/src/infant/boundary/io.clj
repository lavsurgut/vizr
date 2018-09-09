(ns infant.boundary.io
  (:require [clojure.data.json :as json]))


(defn read-json
  [reader]
  (with-open [r (clojure.java.io/reader reader)]
    (json/read r
               :key-fn keyword)))


(defn count-frequences-by-key
  [k data]
  (->>
    (reduce (fn [x y] (conj x (k y))) [] data)
    (frequencies))
  )


(defn count-frequences
  [data]
  (->>
    (map (fn [k] {k (count-frequences-by-key k data)})
         (keys (first data)))
    ))


(defn process
  [file]
  (->>
    ;; read file
    ;; do necessary transformations - number of uniques, outliers, pearson correlation
    ;; do data
    (read-json file)
    (count-frequences)))
