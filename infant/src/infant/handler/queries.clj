(ns infant.handler.queries
  (:require [compojure.core :refer :all]
            [integrant.core :as ig]))

(defmethod ig/init-key :infant.handler/queries [_ options]
  (context "/queries" []
    (GET "/" []
      {:body {:example "data"}})))
