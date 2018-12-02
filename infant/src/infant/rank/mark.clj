(ns infant.rank.mark
  (:require [infant.spec.spec :as sp]))


(def measures #{::sp/quantitative ::sp/temporal})
(def timeunit #{::sp/temporal})
(def no #{::sp/nominal ::sp/ordinal})
(def discrete #{::sp/temporal ::sp/nominal ::sp/ordinal})
(def aggregate #{::sp/aggregate})

(def occluded-measure-mark {::sp/point 0
                    ::sp/tick -0.5
                    ::sp/bar -2
                    ::sp/line -2
                    ::sp/area -2})

(def measure-mark {::sp/point 0
                       ::sp/tick -0.5
                       ::sp/bar -2
                       ::sp/line -2
                       ::sp/area -2})

(def occluded-measure-discrete-mark {::sp/tick 0
                                      ::sp/point -0.2
                                      ::sp/bar -2
                                      ::sp/line -2
                                      ::sp/area -2})

(def occluded-measure-time-mark {::sp/point 0
                                 ::sp/tick -1
                                 ::sp/bar -2
                                 ::sp/line -2
                                 ::sp/area -2})


(def measure-no-mark {::sp/bar 0
              ::sp/point -0.2
              ::sp/tick -0.25
              ::sp/line -2
              ::sp/area -2})

(def measure-aggregate-mark {::sp/bar 0
                     ::sp/point -0.2
                     ::sp/tick -0.25
                     ::sp/line -0.5
                     ::sp/area -0.5})

(def measure-temporal-mark {::sp/line 0
                    ::sp/area -0.1
                    ::sp/bar -0.2
                    ::sp/point -0.3
                    ::sp/tick -0.35})

(def temporal-mark {::sp/point 0
                    ::sp/tick -1
                    ::sp/bar -2
                    ::sp/line -2
                    ::sp/area -2})

(def temporal-discrete-mark {::sp/tick 0
                    ::sp/point -0.2
                    ::sp/bar -2
                    ::sp/line -2
                    ::sp/area -2})


(def discrete-mark {::sp/point 0
                    ::sp/tick  -1
                    ::sp/bar   -2
                    ::sp/line -2
                    ::sp/area -2})



(defn set-name
  [x y has-occlusion mark]
  (str x "-" y "-" has-occlusion + "-" + mark))


(defn init-scores-map
  [score-map x y has-occlusion repeat-reverse]
  (reduce
    (fn [prev next] (let [res (conj prev {(set-name x y has-occlusion (:mark next))})]
                      (if repeat-reverse
                        (conj res {(set-name y x has-occlusion (:mark next))})
                        res))) {} score-map))

(defn init-scores
  []
  )

(defn get-scores
  [spec]
  )