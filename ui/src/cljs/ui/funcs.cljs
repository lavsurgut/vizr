(ns ui.funcs
  (:require [reagent.core :as r]
            [cljsjs.vega-lite]
            [cljsjs.vega-embed]))


(defn log [a-thing]
  (.log js/console a-thing))


(defn group-data [& names]
  (apply concat (for [n names]
                  (map-indexed (fn [i x] {:x i :y x :col n}) (take 20 (repeatedly #(rand-int 100)))))))


(defn render-vega-lite
  ([spec elem]
   (when spec
     (let [spec (clj->js spec)
           opts {:renderer "canvas"
                 :mode "vega-lite"
                 :actions false}
           vega-spec (. js/vl (compile spec))]
       (-> (js/vegaEmbed elem spec (clj->js opts))
           (.then (fn [res]
                    #_(log res)
                    ;(. js/vegaTooltip (vegaLite (.-view res) spec))
                    ))
           (.catch (fn [err]
                     (log err))))))))


(defn vega-lite
  "Reagent component that renders vega-lite."
  ([spec]
    (vega-lite spec "vis"))
  ([spec id]
   (r/create-class
     {:display-name "vega-lite"
      :component-did-mount (fn [this]
                             (render-vega-lite spec (r/dom-node this)))
      :component-will-update (fn [this [_ new-spec]]
                               (render-vega-lite new-spec (r/dom-node this)))
      :reagent-render (fn [spec]
                        [:div {:id id}])})))

