(ns infant.boundary.io
  (:require [clojure.data.json :as json]))


(defn read-json
  [reader]
  (with-open [r (clojure.java.io/reader reader)]
    (json/read r
               :key-fn keyword)))
