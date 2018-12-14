(ns infant.vizrec.rank.mark
  (:require [infant.vizrec.spec :as sp]
            [infant.misc.funcs :as funcs]))


(def measure #{::sp/quantitative})
(def temporal #{::sp/temporal})
(def discrete #{:sp/nominal ::sp/ordinal})

(def measure-mark {::sp/point 0
                   ::sp/tick  -0.5
                   ::sp/bar   -2
                   ::sp/line  -2
                   ::sp/area  -2})


(def measure-discrete-mark {::sp/tick  0
                            ::sp/point -0.2
                            ::sp/bar   -2
                            ::sp/line  -2
                            ::sp/area  -2})

(def measure-temporal-mark {::sp/point 0
                            ::sp/tick  -1
                            ::sp/bar   -2
                            ::sp/line  -2
                            ::sp/area  -2})


(def agg-measure-discrete-mark {::sp/bar   0
                                ::sp/point -0.2
                                ::sp/tick  -0.25
                                ::sp/line  -2
                                ::sp/area  -2})

(def agg-measure-mark {::sp/bar   0
                       ::sp/point -0.2
                       ::sp/tick  -0.25
                       ::sp/line  -0.5
                       ::sp/area  -0.5})

(def agg-measure-temporal-mark {::sp/line  0
                                ::sp/area  -0.1
                                ::sp/bar   -0.2
                                ::sp/point -0.3
                                ::sp/tick  -0.35})

(def temporal-mark {::sp/point 0
                    ::sp/tick  -1
                    ::sp/bar   -2
                    ::sp/line  -2
                    ::sp/area  -2})

(def temporal-discrete-mark {::sp/tick  0
                             ::sp/point -0.2
                             ::sp/bar   -2
                             ::sp/line  -2
                             ::sp/area  -2})


(def discrete-mark {::sp/point 0
                    ::sp/tick  -1
                    ::sp/bar   -2
                    ::sp/line  -2
                    ::sp/area  -2})


(defn set-name
  [x y is-aggregated mark]
  (str x "-" y "-" is-aggregated "-" mark))


(defn init-scores-map
  [score-map x y is-aggregated]
  (reduce
    (fn [res [k v]] (conj res {(set-name x y is-aggregated k) v})) {} score-map))


(defn init-scores
  [mark-maps]
  (reduce conj
          (for [[marks-1 marks-2 score-occlusion-maps] mark-maps
                [x y] (into [] (funcs/cartesian-join (list marks-1 marks-2)))
                [score-map is-aggregated] score-occlusion-maps]
            (init-scores-map score-map x y is-aggregated))))


(defn get-scores
  [spec]
  (let [scores (init-scores [[measure measure [[measure-mark false]]]
                             [measure measure [[agg-measure-mark true]]]
                             [measure temporal [[measure-temporal-mark false]]]
                             [measure temporal [[agg-measure-temporal-mark true]]]
                             [measure discrete [[measure-discrete-mark false]]]
                             [measure discrete [[agg-measure-discrete-mark true]]]
                             [temporal temporal [[temporal-mark false]]]
                             [temporal temporal [[agg-measure-mark true]]]
                             [temporal discrete [[temporal-discrete-mark false]]]
                             [discrete discrete [[discrete-mark true]
                                                 [discrete-mark false]]]])
        fields (::sp/fields spec)
        field-types (reduce (fn [res el]
                              (cond (= (:sp/channel el) ::sp/x)
                                    (assoc res ::sp/x (::sp/type))
                                    (= (:sp/channel el) ::sp/y)
                                    (assoc res ::sp/y (::sp/type)))) {} fields)
        {x ::sp/x
         y ::sp/y} field-types]
    (get scores (set-name x y (sp/is-aggregate? spec) (::sp/mark spec)) 0))
  )
