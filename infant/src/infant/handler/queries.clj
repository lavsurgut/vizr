(ns infant.handler.queries
  (:require [compojure.core :refer :all]
            [integrant.core :as ig]
            [infant.misc.funcs :as funcs]))

(defmethod ig/init-key :infant.handler/queries [_ options]
  (context "/queries" []
    (GET "/" []
      {:body {:data {:values (funcs/group-data "munchkin" "witch" "dog" "lion" "tiger" "bear")}
              :mark "bar"
              :encoding {:x {:field "x"
                             :type "ordinal"}
                         :y {:aggregate "sum"
                             :field "y"
                             :type "quantitative"}
                         :color {:field "col"
                                 :type "nominal"}}}})))
