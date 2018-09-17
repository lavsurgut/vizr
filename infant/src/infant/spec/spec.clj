(ns infant.spec.spec
  (:require [clojure.spec.alpha :as spec]))


(spec/def ::not-empty-string (spec/and string?
                                       #(not (= % ""))
                                       #(not (= (s/lower-case %) "null"))))

(spec/def ::point #(= % "point"))

(spec/def ::bar #(= % "bar"))

(spec/def ::line #(= % "line"))

(spec/def ::area #(= % "area"))

(spec/def ::tick #(= % "tick"))

(spec/def ::mark (spec/or :point ::point :bar ::bar :line ::line :area ::area :tick ::tick))

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

(spec/def ::channel (spec/or :x ::x :y ::y :row ::row :column ::column :size ::size :color ::color))

(spec/def ::name ::not-empty-string)

(spec/def ::aggregate boolean?)
; maps
; field
(spec/def ::field
  (spec/keys :req-un [::name ::channel ::type ::aggregate]))

(spec/def ::fields (s/coll-of ::field :kind vector? :distinct true))

; visual specification
(spec/def ::spec
  (spec/keys :req-un [::field ::mark]))
