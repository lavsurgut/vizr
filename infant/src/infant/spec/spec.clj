(ns infant.spec.spec
  (:require [clojure.spec.alpha :as spec]
            [clojure.string :as s]
            [clojure.spec.gen.alpha :as gen])
  (:import (clojure.lang MapEntry)))

"TODO: add spec for output/input definition"

(defn in?
  "true if coll contains elm"
  [coll elm]
  (some #(= elm %) coll))


(def mark? #{::? ::point ::bar ::line ::area ::tick})

(def type? #{::nominal ::ordinal ::quantitative ::temporal})

(def channel? #{::? ::x ::y ::row ::column ::size ::color})

(def spatial-channel? #{::x ::y ::row ::column})

(spec/def ::not-empty-string (spec/and string?
                                       #(not (= % ""))
                                       #(not (= (s/lower-case %) "null"))))

(spec/def ::? #(= % "?"))

(spec/def ::point #(= % "point"))

(spec/def ::bar #(= % "bar"))

(spec/def ::line #(= % "line"))

(spec/def ::area #(= % "area"))

(spec/def ::tick #(= % "tick"))

(spec/def ::mark mark?)

(spec/def ::nominal #(= % "nominal"))

(spec/def ::ordinal #(= % "ordinal"))

(spec/def ::quantitative #(= % "quantitative"))

(spec/def ::temporal #(= % "temporal"))

(spec/def ::type type?)

(spec/def ::x #(= % "x"))

(spec/def ::y #(= % "y"))

(spec/def ::row #(= % "row"))

(spec/def ::column #(= % "column"))

(spec/def ::size #(= % "size"))

(spec/def ::color #(= % "color"))

(spec/def ::channel channel?)

(spec/def ::spatial-channel spatial-channel?)

(spec/def ::dimension #(= % "dimension"))

(spec/def ::measure #(= % "measure"))

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
         ::?)
    ; return enumerated spec property and enumeration list from spec property form
    {(key prop) (filter #(not= % ::?)
                        (eval (spec/form (key prop))))}
    prop))


(defn build-field-enum-list
  ([field] (build-field-enum-list field {}))
  ([field res]
   (if (empty? field)
     res
     (let [prop (first field)
           res (conj res (make-enum-map prop))]
       (recur (rest field) res)))))


(defn build-fields-enum-list
  ([spec] (build-fields-enum-list (::fields spec) []))
  ([fields res]
   (if (empty? fields)
     {::fields res}
     (let [field (first fields)
           res (conj res (build-field-enum-list field))]
       (recur (rest fields) res)))))


(defn build-mark-enum-list
  [spec]
  (make-enum-map (MapEntry/create ::mark (::mark spec))))


(defn build-enum-list
  "Build enumeration lists for specification properties with '?'"
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


(defn is-channel-enumerated?
  [spec]
  (reduce (fn [x y]
            (or (= (::channel x) ::?) (= (::channel y) ::?)))
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


(defn is-spatial-channel?
  [spec]
  (spec/valid? ::spatial-channel spec))


(defn build-mark-viz-list
  [spec]
  (map (fn [enc] (assoc spec ::mark enc)) (::mark spec)))

;(defn enumerate
;  "take mark, generate all possible combinations,
;   filter out wrong ones"
;  [spec]
;  (let [result-spec (build-mark-viz-list spec)
;        result-spec (conj result-spec (build-fields-viz-list spec))]
;    result-spec)
;  )
