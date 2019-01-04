(ns infant.vizrec.rank.effectiveness.type-channel
  (:require [infant.vizrec.spec :as sp]))


(defn- set-name
  [type channel is-aggregated?]
  (str type "-" channel "-" is-aggregated?))


(defn- init-scores-map
  [score-map type is-aggregated?]
  (reduce
    (fn [res [channel v]]
      (conj res {(set-name type channel is-aggregated?) v})) {} score-map))


(defn- init-scores
  []
  (let [continuous-type-channel-score {::sp/x 0
                                       ::sp/y 0
                                       ::sp/size -0.575
                                       ::sp/color -0.725
                                       ::sp/row -10
                                       ::sp/column -10}
        ordered-type-channel-score {::sp/x 0
                                    ::sp/y 0
                                    ::sp/size -0.575
                                    ::sp/color -0.725
                                    ::sp/row -0.75
                                    ::sp/column -0.75}
        nominal-type-channel-score {::sp/x 0
                                    ::sp/y 0
                                    ::sp/color -0.6
                                    ::sp/row -0.7
                                    ::sp/column -0.7
                                    ::sp/size -3}
        score-maps [[[::sp/quantitative ::sp/temporal]
                     continuous-type-channel-score false]
                    [[::sp/quantitative ::sp/temporal ::sp/ordinal]
                     ordered-type-channel-score true]
                    [[::sp/nominal] nominal-type-channel-score false]]]
    (reduce conj (for [[types score-map is-aggregated?] score-maps
                       type types]
                   (init-scores-map score-map type is-aggregated?)))))


(defn measure-rank
  [spec]
  (let [scores (init-scores)
        fields (::sp/fields spec)]
    (reduce (fn [x y]
              (+ x (get scores (set-name (::sp/type y) (::sp/channel y) (::sp/aggregate? y)) 0)))
            0 fields)))
