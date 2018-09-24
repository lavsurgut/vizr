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
                             true))})