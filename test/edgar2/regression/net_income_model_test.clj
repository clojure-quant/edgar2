(ns edgar2.regression.net-income-model-test
  (:require [clojure.test :refer [deftest is testing]]
            [edgar2.regression.net-income-model :as nim]))

(deftest flat-series-is-stable
  (let [result (nim/classify-series [5 5 5 5 5])]
    (is (= :stable (:model result)))
    (is (true? (:stable? result)))))

(deftest rising-line-is-trend
  (let [result (nim/classify-series [1 2 3 4 5])]
    (is (= :trend-linear (:model result)))))

(deftest empty-series-has-no-model
  (is (nil? (nim/classify-series [])))
  (is (nil? (nim/classify-series nil)))
  (is (nil? (nim/classify-series [nil nil nil]))))

(deftest missing-observations-are-dropped
  (is (= :stable (:model (nim/classify-series [nil 5 5 nil 5]))))
  (is (= :trend-linear (:model (nim/classify-series [nil 1 2 3 nil 5])))))

(deftest trend-significance-threshold
  (is (nim/trend-significant? {:r-squared 0.8}))
  (is (nim/trend-significant? {:r-squared 1.0}))
  (is (not (nim/trend-significant? {:r-squared 0.79})))
  (is (not (nim/trend-significant? {}))))

(deftest short-series-still-classifies
  (testing "two equal values"
    (is (= :stable (:model (nim/classify-series [4 4])))))
  (testing "two distinct values"
    (is (= :trend-linear (:model (nim/classify-series [1 2]))))))
