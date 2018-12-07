(ns infant.vizrec.spec
  (:require [clojure.spec.alpha :as spec]
            [clojure.string :as s]
            [clojure.set :as set]))

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


(defn build-specs
  [partial-spec props]
  (if (= (get-in partial-spec props)
         ::?)
    (let [enum (eval (spec/form (last props)))
          filtered-enum (filter #(not= % ::?) enum)]
      (set (map (fn [enc] (assoc-in partial-spec props enc)) filtered-enum)))
    #{partial-spec}
    ))

(defn build-mark-specs
  [partial-spec]
  (build-specs partial-spec [::mark]))

(defn build-field-specs
  [partial-spec specs]
  (loop [idx 0
         res specs]
    (let [size (count (::fields partial-spec))]
      (if (= idx size)
       res
       (->> res
            (map (fn [spec]
                   (build-specs spec [::fields idx ::channel])))
            (reduce (fn [x y] (set/union x y)))
            (recur (+ idx 1)))))))

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


(defn enumerate
  "Build possible specifications for specification properties with '?'"
  [partial-spec]
  (->> (build-mark-specs partial-spec)
       (build-field-specs partial-spec))
  )
