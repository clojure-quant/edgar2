(ns edgar2.web-net-income-model-test
  (:require [clojure.test :refer [deftest is]]
            [tech.v3.dataset :as ds]
            [edgar2.web.net-income-model :as nim]))

(deftest display-value-covers-model-types
  (is (= "" (nim/display-value nil)))
  (is (= "stable" (nim/display-value :stable)))
  (is (= "true" (nim/display-value true)))
  (is (= "1.5" (nim/display-value 1.5)))
  (is (= "Inf" (nim/display-value Double/POSITIVE_INFINITY)))
  (is (= "-Inf" (nim/display-value Double/NEGATIVE_INFINITY)))
  (is (= "NaN" (nim/display-value Double/NaN)))
  (is (= "0.0, 1.0" (nim/display-value [0.0 1.0]))))

(deftest model-rows-sorts-keys-and-formats-values
  (is (= [["intercept" "0.0"]
          ["model" "trend-linear"]
          ["slope" "1.0"]]
         (nim/model-rows {:model :trend-linear :slope 1.0 :intercept 0.0})))
  (is (= [] (nim/model-rows {}))))

(deftest year-rows-oldest-first
  (let [ds (ds/->dataset [{:end "2024-12-31" :revenue 100 :net-income 10}
                          {:end "2023-12-31" :revenue 90 :net-income 9}])]
    (is (= [{:end "2023-12-31" :revenue 90 :net-income 9}
            {:end "2024-12-31" :revenue 100 :net-income 10}]
           (nim/year-rows ds))))
  (is (= [] (nim/year-rows nil))))
