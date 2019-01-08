(ns infant.handler.queries
  (:require [compojure.core :refer :all]
            [integrant.core :as ig]
            [infant.boundary.io :as io]
            [infant.taskrec.recommender :as recommender]))

(defn process-file
  [file]
  (->>
    ;; read file
    (io/read-json file)))

(defn process
  []
  (let [data (process-file "resources/infant/public/cars.json")]
    (recommender/recommend data [:identify :outliers])))


(defmethod ig/init-key :infant.handler/queries [_ options]
  (context "/queries" []
    (GET "/" []
      {:body (process)})))
