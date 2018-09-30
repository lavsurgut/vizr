(ns infant.constraint.constraint
  (:require [infant.spec.spec :as sp]))


(defprotocol Satisfiable
  (satisfied? []))

(defrecord Constraint
  [name description properties strict satisfy-fn]
  Satisfiable
  (satisfied? []
    (satisfy-fn)))

(def field-constraints #{
                         ->Constraint
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
                             true))
                         ->Constraint
                         "channel-field-compatible"
                         "Encoding channel's range type be compatible with channel type."
                         #{::sp/channel, ::sp/type, ::sp/aggregate}
                         true
                         (fn [field]
                           (sp/is-channel-compatible? field))
                         ->Constraint
                         "channel-supports-role"
                         "Encoding channel should support the role of the field."
                         #{::sp/channel, ::sp/type, ::sp/aggregate}
                         true
                         (fn [field]
                           (if (= (::sp/channel field) ::?)
                             true
                             (sp/support-role? (::sp/channel field) (sp/get-measure-type field))
                             )
                           )})
