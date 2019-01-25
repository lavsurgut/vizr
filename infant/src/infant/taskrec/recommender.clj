(ns infant.taskrec.recommender
  (:require [clojure.spec.alpha :as s]
            [infant.datarec.recommender :as datarec]
            [infant.vizrec.recommender :as vizrec]
            [infant.vizrec.spec :as sp]))


(def config {:identify {:outliers {:datarec {:func  datarec/recommend
                                             :top-n 5}
                                   :vizrec  {:func         vizrec/recommend
                                             :partial-spec {::sp/mark   ::sp/?
                                                            ::sp/fields [{::sp/channel    ::sp/?
                                                                          ::sp/type       ::sp/quantitative
                                                                          ::sp/aggregate? false}]}}}}})


(defn recommend
  [data keys]
  (let [{{datarec-func  :func
          daterec-top-n :top-n}              :datarec
         {vizrec-func         :func
          vizrec-partial-spec :partial-spec} :vizrec} (get-in config keys)
        target-key (second keys)
        datarec-result (datarec-func data target-key daterec-top-n)
        vizrec-result-spec (vizrec-func vizrec-partial-spec)
        vega-lite-encoding (reduce (fn [res el]
                                     (conj res
                                           (condp = (::sp/channel el)
                                             ::sp/x {:x {:field "" :type (name (::sp/type el))}}
                                             ::sp/y {:y {:field "" :type (name (::sp/type el))}})))
                                   {}
                                   (::sp/fields vizrec-result-spec))
        vega-lite-spec {:data     {:values data}
                        :mark     (name (::sp/mark vizrec-result-spec))
                        :encoding vega-lite-encoding}
        result (reduce (fn [res [k _]]
                         (let [vizrec-fields (::sp/fields vizrec-result-spec)
                               first-channel (::sp/channel (first vizrec-fields))
                               first-channel-keyword (keyword (name first-channel))]
                           (conj res (assoc-in vega-lite-spec
                                               [:encoding first-channel-keyword :field]
                                               (name k)))))
                       []
                       datarec-result)
        ]
    result))
