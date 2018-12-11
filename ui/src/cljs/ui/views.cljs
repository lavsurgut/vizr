(ns ui.views
  (:require
   [re-frame.core :as re-frame]
   [ui.subs :as subs]
   [ui.funcs :as funcs]
   ))

;; home
;[:div
; [:a {:href "#/about"}
;  "go to About Page"]]

(defn vl-graphs
  [specs]
  (reduce conj [:div {:class "columns is-multiline vl-graphs"}]
          (map (fn [spec] [:div {:class "column"}
                           [funcs/vega-lite spec]]) specs))
  )


(def dropdown
  [:div {:class "dropdown is-active"}
   [:div {:class "dropdown-trigger"}
    [:button {:class "button is-info"}
     [:span "Show Top 5"]
     [:span {:class "icon is-small"}
      [:i {:class "fas fa-angle-down"}]]]]
   [:div {:class "dropdown-menu" :role "menu"}
    [:div {:class "dropdown-content"}
     [:a {:href "#" :class "dropdown-item is-active"} "Show top 5"]
     [:a {:href "#" :class "dropdown-item"} "Show top 10"]]]])


(def header
  [:div
   [:h2 {:class "title has-text-centered"} "Identify outliers"]
   [:nav {:class "level"}
    [:div {:class "level-item has-text-centered"}
     [:div
      [:div dropdown]]]]])


(defn home-panel []
  (let [name (re-frame/subscribe [::subs/name])
        specs (re-frame/subscribe [::subs/specs])]
    [:div
     [:div {:class "section"}
      [:div {:class "container"}
       header]]
     [:div {:class "section"}
      [:div {:class "container"}
       [vl-graphs @specs]]]]))


;; about

(defn about-panel []
  [:div
   [:h1 "This is the About Page."]

   [:div
    [:a {:href "#/"}
     "go to Home Page"]]])


;; main

(defn panels [panel-name]
  (case panel-name
    :home-panel [home-panel]
    :about-panel [about-panel]
    [:div]))


(defn main-panel []
  (let [active-panel (re-frame/subscribe [::subs/active-panel])]
    [panels @active-panel]))
