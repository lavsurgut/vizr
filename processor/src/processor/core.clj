(ns processor.core)

(ns processor.core
  (:require [clojure.tools.cli :as cli :refer [parse-opts]]
            [processor.io :as io])
  (:gen-class))

(def cli-options
  ;; An option with a required argument
  [["-f" "--file FILE" "File to process"]
   ["-h" "--help"]])


(defn -main [& args]
  (let [{:keys [options]} (cli/parse-opts args cli-options)]
    (io/process (:file options))))
