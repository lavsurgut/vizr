(ns infant.middleware.cors
  (:require [integrant.core :as ig]))


(defn wrap-cors
  "Middleware function to allow cross origin requests from browsers"
  ([handler]
   (wrap-cors handler "*"))
  ([handler allowed-origins]
   (fn [request]
     (-> (handler request)
         ; Pass the request on, but make sure we add this header for CORS support
         (assoc-in [:headers "Access-Control-Allow-Origin"] allowed-origins)
         (assoc-in [:headers "Access-Control-Allow-Methods"] "GET,POST,DELETE")
         (assoc-in [:headers "Access-Control-Allow-Headers"]
                   "X-Requested-With,Content-Type,Cache-Control,Origin,Accept,Authorization")))
    ))


(defmethod ig/init-key ::add-cors [_ options]
  #(wrap-cors %))
