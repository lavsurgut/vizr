(ns infant.handler.files
  (:require [compojure.core :refer :all]
            [integrant.core :as ig]
            [infant.boundary.io :as io]))

(defmethod ig/init-key :infant.handler/files [_ options]
  (context "/files" []
    (GET "/" []
      {:body {:example (io/process "resources/infant/public/cars.json")}})))
