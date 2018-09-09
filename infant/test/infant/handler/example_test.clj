(ns infant.handler.example-test
  (:require [clojure.test :refer :all]
            [integrant.core :as ig]
            [ring.mock.request :as mock]
            [infant.handler.queries :as example]))

(deftest smoke-test
  (testing "example page exists"
    (let [handler  (ig/init-key :infant.handler/example {})
          response (handler (mock/request :get "/example"))]
      (is (= 200 (:status response)) "response ok"))))
