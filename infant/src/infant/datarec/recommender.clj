(ns infant.datarec.recommender
  (:require [infant.datarec.preparation :as prep]))


(defn recommend
  [data data-key top-n]
  (->> (prep/prepare-data data)
       (data-key)
       (sort-by (comp count val) >)
       (take top-n)))
