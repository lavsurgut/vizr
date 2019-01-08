(ns infant.vizrec.rank.effectiveness.mark
  (:require [infant.vizrec.spec :as sp]
            [infant.misc.funcs :as funcs]))


(defn- set-name
  [x y is-aggregated? mark]
  (str x "-" y "-" is-aggregated? "-" mark))


(defn- init-scores-map
  [score-map x y is-aggregated?]
  (reduce
    (fn [res [k v]] (conj res
                          {(set-name x y is-aggregated? k) v}
                          {(set-name y x is-aggregated? k) v})) {} score-map))


(defn- init-scores
  []
  (let [measure #{::sp/quantitative}
        temporal #{::sp/temporal}
        discrete #{::sp/nominal ::sp/ordinal}
        none #{::none}
        measure-mark {::sp/point 0
                      ::sp/tick  -0.5
                      ::sp/bar   -2
                      ::sp/line  -2
                      ::sp/area  -2}
        measure-discrete-mark {::sp/tick  0
                               ::sp/point -0.2
                               ::sp/bar   -2
                               ::sp/line  -2
                               ::sp/area  -2}
        measure-temporal-mark {::sp/point 0
                               ::sp/tick  -1
                               ::sp/bar   -2
                               ::sp/line  -2
                               ::sp/area  -2}
        agg-measure-discrete-mark {::sp/bar   0
                                   ::sp/point -0.2
                                   ::sp/tick  -0.25
                                   ::sp/line  -2
                                   ::sp/area  -2}
        agg-measure-mark {::sp/bar   0
                          ::sp/point -0.2
                          ::sp/tick  -0.25
                          ::sp/line  -0.5
                          ::sp/area  -0.5}
        agg-measure-temporal-mark {::sp/line  0
                                   ::sp/area  -0.1
                                   ::sp/bar   -0.2
                                   ::sp/point -0.3
                                   ::sp/tick  -0.35}
        temporal-mark {::sp/point 0
                       ::sp/tick  -1
                       ::sp/bar   -2
                       ::sp/line  -2
                       ::sp/area  -2}
        temporal-discrete-mark {::sp/tick  0
                                ::sp/point -0.2
                                ::sp/bar   -2
                                ::sp/line  -2
                                ::sp/area  -2}
        discrete-mark {::sp/point 0
                       ::sp/tick  -1
                       ::sp/bar   -2
                       ::sp/line  -2
                       ::sp/area  -2}
        mark-maps [[measure measure [[measure-mark false]
                                     [agg-measure-mark true]]]
                   [measure temporal [[measure-temporal-mark false]
                                      [agg-measure-temporal-mark true]]]
                   [measure discrete [[measure-discrete-mark false]
                                      [agg-measure-discrete-mark true]]]
                   [measure none [[measure-discrete-mark false]]]
                   [temporal temporal [[temporal-mark false]
                                       [agg-measure-mark true]]]
                   [temporal discrete [[temporal-discrete-mark false]]]
                   [temporal none [[temporal-discrete-mark false]]]
                   [discrete discrete [[discrete-mark true]
                                       [discrete-mark false]]]]]
    (reduce conj
            (for [[marks-1 marks-2 score-occlusion-maps] mark-maps
                  [x y] (into [] (funcs/cartesian-join (list marks-1 marks-2)))
                  [score-map is-aggregated] score-occlusion-maps]
              (init-scores-map score-map x y is-aggregated)))))


(defn measure-rank
  [spec]
  (let [scores (init-scores)
        fields (::sp/fields spec)
        field-types (reduce (fn [res el] (condp = (::sp/channel el)
                                           ::sp/x (assoc res ::sp/x (::sp/type el))
                                           ::sp/y (assoc res ::sp/y (::sp/type el))))
                            {}
                            fields)
        x (get field-types ::sp/x ::none)
        y (get field-types ::sp/y ::none)]
    (get scores (set-name x y (sp/is-aggregate? spec) (::sp/mark spec)) 0))
  )
