(ns infant.vizrec.constraint
  (:require [infant.spec.spec :as sp]))


(defprotocol Satisfiable
  (satisfied? [self y]))

(defrecord Constraint
  [name description properties strict satisfy-fn]
  Satisfiable
  (satisfied? [self y]
    (satisfy-fn y)))

(def field-constraints [(->Constraint
                          "aggregate-op-supported-by-type"
                          "Aggregate function should be supported by data type."
                          #{::sp/type, ::sp/aggregate}
                          true
                          (fn [field]
                            (if (::sp/aggregate field)
                              (and (not= (::sp/type field)
                                         (::sp/ordinal))
                                   (not= (::sp/type field)
                                         (::sp/nominal)))
                              true)))
                        (->Constraint
                          "channel-field-compatible"
                          "Encoding channel's range type be compatible with channel type."
                          #{::sp/channel, ::sp/type, ::sp/aggregate}
                          true
                          (fn [field]
                            (sp/is-channel-compatible? field)))
                        (->Constraint
                          "channel-supports-role"
                          "Encoding channel should support the role of the field."
                          #{::sp/channel, ::sp/type, ::sp/aggregate}
                          true
                          (fn [field]
                            (if (= (::sp/channel field) ::sp/?)
                              true
                              (sp/support-role?
                                (::sp/channel field) (sp/get-measure-type field))
                              )
                            ))])


(def spec-constraints [(->Constraint
                         "no-repeated-channel"
                         "Each encoding shall be used only once"
                         #{::sp/channel}
                         true
                         (fn [spec]
                           (apply distinct?
                                  (map (fn [f] (::sp/channel f)) (::sp/fields spec)))))

                       (->Constraint
                         "channel-permitted-by-mark-type"
                         "Each encoding channel shall be supported by the mark type"
                         #{::sp/channel ::sp/mark}
                         true
                         (fn [spec]
                           (reduce (fn [x y]
                                     (and x y))
                                   (map (fn [f] (sp/support-mark?
                                                  (::sp/channel f) (::sp/mark spec)))
                                        (::sp/fields spec)))))
                       (->Constraint
                         "omit-bar-line-area-with-occlusion"
                         "Don't use bar, line or area to visualize raw plot as they often lead to occlusion"
                         #{::sp/aggregate ::sp/mark}
                         false
                         (fn [spec]
                           (let [spec-mark (::sp/mark spec)]
                             (if (or (= spec-mark ::sp/bar)
                                     (= spec-mark ::sp/line)
                                     (= spec-mark ::sp/area))
                               (sp/is-aggregate? spec)
                               true))
                           ))
                       (->Constraint
                         "omit-non-positional-or-facet-over-positional-channels"
                         "Don't use non-positional channels unless all positional channels are used."
                         #{::sp/channel}
                         false
                         (fn [spec]
                           (loop [curr-fields (::sp/fields spec)
                                  has-enumerated-non-position-or-facet-channel false
                                  has-x false
                                  has-y false]
                             (let [first-field (first curr-fields)
                                   rest-fields (rest curr-fields)]
                               (if (seq curr-fields)
                                 (cond
                                   (= (::sp/channel first-field) ::sp/x)
                                   (recur rest-fields
                                          has-enumerated-non-position-or-facet-channel true has-y)
                                   (= (::sp/channel first-field) ::sp/y)
                                   (recur rest-fields
                                          has-enumerated-non-position-or-facet-channel has-x true)
                                   :else
                                   (recur rest-fields
                                          true has-x has-y))
                                 (if has-enumerated-non-position-or-facet-channel
                                   (and has-x has-y)
                                   true)))
                             )
                           ))
                       (->Constraint
                         "omit-aggregate-plot-with-dimension-only-on-facet"
                         "Omit aggregate plots with dimensions only on facets
                         as that leads to inefficient use of space."
                         #{::sp/channel ::sp/aggregate}
                         false
                         (fn [spec]
                           (if (sp/is-aggregate? spec)
                             (loop [curr-fields (::sp/fields spec)
                                    has-non-facet-dim false
                                    has-dim false
                                    has-enumerated-facet-dim false]
                               (let [first-field (first curr-fields)
                                     rest-fields (rest curr-fields)]
                                 (if (not (empty curr-fields))
                                   (if (not (::sp/aggregate curr-fields))
                                     (if (or (= (::sp/channel first-field) ::sp/row)
                                             (= (::sp/channel first-field) ::sp/column))
                                       (recur rest-fields has-non-facet-dim true true)
                                       (recur rest-fields has-non-facet-dim true has-enumerated-facet-dim))
                                     (recur rest-fields true has-dim has-enumerated-facet-dim))
                                   (if (and has-dim (not has-non-facet-dim))
                                     (if has-enumerated-facet-dim
                                       false
                                       true)
                                     true))))
                             true)))
                       (->Constraint
                         "omit-multiple-non-positional-channels"
                         "Unless manually specified, do not use multiple non-positional encoding
                         channel to avoid over-encoding"
                         #{::sp/channel}
                         false
                         (fn [spec]
                           (loop [curr-fields (::sp/fields spec)
                                  non-positional-channel-count 0
                                  has-enumerated-non-positional-channel false
                                  result true]
                             (let [first-field (first curr-fields)
                                   rest-fields (rest curr-fields)]
                               (if (not (empty curr-fields))
                                 (if (not= (::sp/channel first-field) ::sp/?)
                                   (if (not (sp/is-spatial-channel? (::sp/channel first-field)))
                                     (if (and
                                           (> non-positional-channel-count 1)
                                           (has-enumerated-non-positional-channel))
                                       (recur rest-fields (+ non-positional-channel-count 1) true (and result false))
                                       (recur rest-fields (+ non-positional-channel-count 1) true (and result true)))
                                     (recur rest-fields non-positional-channel-count false (and result true)))
                                   (recur rest-fields non-positional-channel-count false (and result true)))
                                 result
                                 )))))
                       ])


(defn field-constraints-satisfied?
  [fields constraints]
  (loop [curr-fields fields res true]
    (if (not (seq curr-fields))
      res
      (recur (rest curr-fields)
        (reduce (fn [x y] (and x (satisfied? y (first curr-fields)))) res constraints)))))


(defn spec-constraints-satisfied?
  [spec constraints]
  (reduce (fn [x y] (and x (satisfied? y spec))) true constraints))


(defn filter-invalid-specs
  [specs]
  (->> specs
       (filter (fn [x] (field-constraints-satisfied? (::sp/fields x) field-constraints)))
       (filter (fn [x] (spec-constraints-satisfied? x spec-constraints)))))
