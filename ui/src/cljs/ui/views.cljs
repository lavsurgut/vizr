(ns ui.views
  (:require
   [re-frame.core :as re-frame]
   [ui.subs :as subs]
   [ui.funcs :as funcs]
   ))


;; home

(defn home-panel []
  (let [name (re-frame/subscribe [::subs/name])]
    [:div
     [:h1 (str "Hello 2 from " @name ". This is the Home Page.")]

     [:div
      [:a {:href "#/about"}
       "go to About Page"]
      [funcs/vega-lite {:data {:values (funcs/group-data "munchkin" "witch" "dog" "lion" "tiger" "bear")}
                          :mark "bar"
                          :encoding {:x {:field "x"
                                         :type "ordinal"}
                                     :y {:aggregate "sum"
                                         :field "y"
                                         :type "quantitative"}
                                     :color {:field "col"
                                             :type "nominal"}}}]]
     ]))


;; about

(defn about-panel []
  [:div
   [:h1 "This is the About Page."]

   [:div
    [:a {:href "#/"}
     "go to Home Page"]]])


;; main

(defn- panels [panel-name]
  (case panel-name
    :home-panel [home-panel]
    :about-panel [about-panel]
    [:div]))

(defn show-panel [panel-name]
  [panels panel-name])

(defn main-panel []
  (let [active-panel (re-frame/subscribe [::subs/active-panel])]
    [show-panel @active-panel]))
