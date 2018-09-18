(ns infant.vizrec.recommender
  (:require [infant.spec.spec :as sp]))


(defn make-enum-map
  [prop]
  (if (= (val prop)
         "?")
    {:property (key prop)
     :enum (keyword (str (key prop) "-enum"))}
    {}))


(defn build-field-enum-list
  ([field] (build-field-enum-list field {}))
  ([field res]
   (if (empty? field)
     res
     (let [prop (first field)
           res (conj res (make-enum-map prop))]
       (recur (rest field) res)))))

;; TODO: put proper map for multiple fields, test
(defn build-fields-enum-list
  ([fields] (build-fields-enum-list fields {}))
  ([fields res]
   (let [field (first fields)
         res (conj res (build-field-enum-list field))]
     (recur (rest fields) res))))


(defn build-mark-enum-list
  [spec]
  (make-enum-map [::sp/mark (get spec ::sp/mark)]))


(defn build-enum-list
  ;;get spec
  ;;build a list of possible enumerations
  ;;
  [spec]
  (->>
    (build-mark-enum-list spec)
    )
  )

(defn recommend
  [spec schema]
  (;; build enum list
    ;; build possible queries
    ;; rank them
    ))