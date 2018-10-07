(ns infant.constraint.constraint
  (:require [infant.spec.spec :as sp]))


(defprotocol Satisfiable
  (satisfied? [x]))

(defrecord Constraint
  [name description properties strict satisfy-fn]
  Satisfiable
  (satisfied? [x]
    (satisfy-fn x)))

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
                             (if (not (empty curr-fields))
                               (cond
                                (= (::sp/channel (first curr-fields)) ::sp/x)
                                (recur (rest curr-fields)
                                       has-enumerated-non-position-or-facet-channel true has-y)
                                (= (::sp/channel (first curr-fields)) ::sp/y)
                                (recur (rest curr-fields)
                                       has-enumerated-non-position-or-facet-channel has-x true)
                                (not= (::sp/channel (first curr-fields)) ::sp/?)
                                (if (sp/is-channel-enumerated? spec)
                                  (recur (rest curr-fields)
                                         true has-x has-y)
                                  (recur (rest curr-fields)
                                         has-enumerated-non-position-or-facet-channel has-x has-y)))
                               (if has-enumerated-non-position-or-facet-channel
                                 (and has-x has-y)
                                 true))
                             )
                           ))])
;Constraint(
;            name = "omitAggregatePlotWithDimensionOnlyOnFacet",
;                 description = "Omit aggregate plots with dimensions only on facets as that leads to inefficient use of space.",
;                 properties = Set(Property.CHANNEL, Property.AGGREGATE, Property.AUTO_COUNT),
;                 allowWildCardForProperties = false,
;                 strict = false,
;                 satisfy = (model: WildCardModel) => {
;                                                      if (VizRecSpec.isAggregate(model.spec.fields)) {
;                                                                                                      var hasNonFacetDim_1, hasDim_1, hasEnumeratedFacetDim_1 = false
;                                                                                                      model.spec.fields.foreach(f => {
;                                                                                                                                      if (!Field.isValueQuery(f)) {
;
;                                                                                                                                                                   if (f.aggregate.isEmpty) {
;                                                                                                                                                                                             hasDim_1 = true
;                                                                                                                                                                                                      if (f.channel == Channel.ROW || f.channel == Channel.COLUMN) {
;                                                                                                                                                                                                                                                                    if (model.wildCardIdx.contains(Property.CHANNEL)) {
;                                                                                                                                                                                                                                                                                                                       hasEnumeratedFacetDim_1 = true
;                                                                                                                                                                                                                                                                                                                       }
;                                                                                                                                                                                                                                                                    }
;                                                                                                                                                                                             } else {
;                                                                                                                                                                                                     hasNonFacetDim_1 = true
;                                                                                                                                                                                                     }
;
;                                                                                                                                                                   }
;
;                                                                                                                                      })
;
;                                                                                                      if (hasDim_1 && !hasNonFacetDim_1) {
;                                                                                                                                          if (hasEnumeratedFacetDim_1) {
;                                                                                                                                                                        false
;                                                                                                                                                                        } else true
;                                                                                                                                          } else true
;                                                                                                      } else true
;                                                      }
;                 ),
;Constraint(
;            name = "omitMultipleNonPositionalChannels",
;                 description = "Unless manually specified, do not use multiple non-positional encoding channel to avoid over-encoding.",
;                 properties = Set(Property.CHANNEL),
;                 allowWildCardForProperties = true,
;                 strict = false,
;                 satisfy = (model: WildCardModel) => {
;                                                      var nonPositionChannelCount = 0
;                                                      var hasEnumeratedNonPositionChannel: Boolean = false
;                                                          model.spec.fields.forall(f => {
;                                                                                         if (!Field.isValueQuery(f)) {
;                                                                                                                      if (!WildCard.isWildCard(f.channel.toString)) {
;                                                                                                                                                                     if (Channel.NONSPATIAL_CHANNELS.contains(f.channel)) {
;                                                                                                                                                                                                                           nonPositionChannelCount += 1
;                                                                                                                                                                                                                                                   if (model.wildCardIdx.contains(Property.CHANNEL)) {
;                                                                                                                                                                                                                                                                                                      hasEnumeratedNonPositionChannel = true
;                                                                                                                                                                                                                                                                                                      }
;                                                                                                                                                                                                                           if ((nonPositionChannelCount > 1) && hasEnumeratedNonPositionChannel) {
;                                                                                                                                                                                                                                                                                                  false
;                                                                                                                                                                                                                                                                                                  } else true
;                                                                                                                                                                                                                           } else true
;                                                                                                                                                                     } else true
;                                                                                                                      } else true
;                                                                                         })
;                                                      }
;                 )