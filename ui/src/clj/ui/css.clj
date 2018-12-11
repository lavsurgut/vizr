(ns ui.css
  (:require [garden.core :as g]
            [garden.units :as u]
            [garden.selectors :as s]
            [garden.stylesheet :as stylesheet]))


(def styles
  [[:body {:font-family "monospace"}]
   [:div.success {:color "green"}]])
