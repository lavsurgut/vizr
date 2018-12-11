(ns infant.handler.queries
  (:require [compojure.core :refer :all]
            [integrant.core :as ig]
            [infant.misc.funcs :as funcs]
            [infant.boundary.io :as io]))

(defn process-file
  [file]
  (->>
    ;; read file
    (io/read-json file)))

(defn process
  []
  (let [data (process-file "resources/infant/public/population.json")]
    [{:data      {:values data}
      :transform [{:filter "datum.year == 2000"}],
      :mark      "bar",
      :encoding  {
                  :y {:field "age"
                      :type  "ordinal"
                      :scale {:rangeStep 17}},
                  :x {:aggregate "sum"
                      :field     "people"
                      :type      "quantitative"
                      :axis      {:title "population"}}
                  }}
     {:data      {:values data}
      :transform [{:filter "datum.year == 2000"}],
      :mark      "bar",
      :encoding  {
                  :y {:field "age"
                      :type  "ordinal"
                      :scale {:rangeStep 17}},
                  :x {:aggregate "sum"
                      :field     "people"
                      :type      "quantitative"
                      :axis      {:title "population"}}
                  }}
     {:data      {:values data}
      :transform [{:filter "datum.year == 2000"}],
      :mark      "bar",
      :encoding  {
                  :y {:field "age"
                      :type  "ordinal"
                      :scale {:rangeStep 17}},
                  :x {:aggregate "sum"
                      :field     "people"
                      :type      "quantitative"
                      :axis      {:title "population"}}
                  }}]))


(defmethod ig/init-key :infant.handler/queries [_ options]
  (context "/queries" []
    (GET "/" []
      {:body (process)})))
