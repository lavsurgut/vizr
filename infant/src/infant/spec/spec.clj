(ns infant.spec.spec
  (:require [clojure.spec.alpha :as spec]
            [clojure.string :as s])
  (:import (clojure.lang MapEntry)))

(defn in?
  "true if coll contains elm"
  [coll elm]
  (some #(= elm %) coll))


(spec/def ::not-empty-string (spec/and string?
                                       #(not (= % ""))
                                       #(not (= (s/lower-case %) "null"))))

(spec/def ::? #(= % "?"))

(spec/def ::point #(= % "point"))

(spec/def ::bar #(= % "bar"))

(spec/def ::line #(= % "line"))

(spec/def ::area #(= % "area"))

(spec/def ::tick #(= % "tick"))

(spec/def ::mark (spec/or :? ::? :point ::point :bar ::bar :line ::line :area ::area :tick ::tick))

(spec/def ::mark-enum #{::point ::bar ::line ::area ::tick})

(spec/def ::nominal #(= % "nominal"))

(spec/def ::ordinal #(= % "ordinal"))

(spec/def ::quantitative #(= % "quantitative"))

(spec/def ::temporal #(= % "temporal"))

(spec/def ::type (spec/or :n ::nominal :o ::ordinal :q ::quantitative :t ::temporal))

(spec/def ::x #(= % "x"))

(spec/def ::y #(= % "y"))

(spec/def ::row #(= % "row"))

(spec/def ::column #(= % "column"))

(spec/def ::size #(= % "size"))

(spec/def ::color #(= % "color"))

(spec/def ::channel (spec/or :? ::? :x ::x :y ::y :row ::row :column ::column :size ::size :color ::color))

(spec/def ::dimension #(= % "dimension"))

(spec/def ::measure #(= % "measure"))

(spec/def ::measure-type (spec/or :dim ::dimension :mea ::measure))

(spec/def ::channel-enum #{::x ::y ::row ::column ::size ::color})

(spec/def ::name ::not-empty-string)

(spec/def ::aggregate boolean?)

; maps
; field
(spec/def ::field
  (spec/keys :req-un [::name ::channel ::type ::aggregate]))

(spec/def ::fields (spec/coll-of ::field :kind vector? :distinct true))

; visual specification
(spec/def ::spec
  (spec/keys :req-un [::fields ::mark]))


(defn make-enum-map
  [prop]
  (if (= (val prop)
         "?")
    {(key prop) (keyword (str (key prop) "-enum"))}))


(defn build-field-enum-list
  ([field] (build-field-enum-list field {}))
  ([field res]
   (if (empty? field)
     res
     (let [prop (first field)
           res (conj res (make-enum-map prop))]
       (recur (rest field) res)))))


(defn build-fields-enum-list
  ([spec] (build-fields-enum-list (get spec ::fields) []))
  ([fields res]
   (if (empty? fields)
     {::fields res}
     (let [field (first fields)
           res (conj res (build-field-enum-list field))]
       (recur (rest fields) res)))))


(defn build-mark-enum-list
  [spec]
  (make-enum-map (MapEntry/create ::mark (get spec ::mark))))


(defn build-enum-list
  [spec]
  (let [result-spec (build-mark-enum-list spec)
        result-spec (conj result-spec (build-fields-enum-list spec))]
    result-spec)
  )

(defn is-discrete?
  [field]
  (condp = (::type field)
    (or ::nominal ::ordinal) true
    (or ::quantitative ::temporal) (= (::aggregate field) true)
    false))

(defn is-continuous?
  [field]
  (not (is-discrete? field)))


(defn is-channel-compatible?
  [field]
  (condp in? (::channel field)
    [::row ::column] (if (and (is-continuous? field)
                              (::aggregate field))
                       false
                       true)
    [::x ::y ::color] true
    [::size] (if (and (is-discrete? field)
                      (::aggregate field))
               false
               true)
    false))


(defn is-aggregate?
  [spec]
  (reduce (fn [x y]
            (or (::aggregate x) (::aggregate y)))
          (::fields spec)))

(defn get-measure-type
  [field]
  (if (or (= (::type field)
             ::nominal)
          (= (::type field)
             ::ordinal)
          (= (::aggregate field)
             true))
    ::dimension
    ::measure))


(defn support-mark?
  [channel mark]
  (if (= mark ::?)
    true
    (condp in? channel
      [::?] true
      [::x ::y ::color ::row ::column]
      (condp in? mark
        [::point ::tick ::bar ::line ::area]
        true
        false)
      [::size]
      (condp in? mark
        [::point ::tick ::bar ::line]
        true
        false)
      false)))


(defn support-role?
  [channel measure-type]
  (condp in? channel
    [::?] true
    [::x ::y ::color] true
    [::row ::column]
    (case measure-type
      ::dimension true
      ::measure false
      false)
    [::size]
    (case measure-type
      ::dimension false
      ::measure true
      false)
    false)
  )