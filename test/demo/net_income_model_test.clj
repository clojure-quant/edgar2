(ns demo.net-income-model-test
  (:require [clojure.test :refer [deftest is testing]]
            [demo.net-income-model :as demo]))

(deftest flat-series-is-stable
  (let [result (demo/classify-series [5 5 5 5 5])]
    (is (= :stable (:model result)))
    (is (true? (:stable? result)))))

(deftest rising-line-is-trend
  (let [result (demo/classify-series [1 2 3 4 5])]
    (is (= :trend-linear (:model result)))))

(deftest empty-series-has-no-model
  (is (nil? (demo/classify-series [])))
  (is (nil? (demo/classify-series nil)))
  (is (nil? (demo/classify-series [nil nil nil]))))

(deftest missing-observations-are-dropped
  (is (= :stable (:model (demo/classify-series [nil 5 5 nil 5]))))
  (is (= :trend-linear (:model (demo/classify-series [nil 1 2 3 nil 5])))))

(deftest trend-quintile-bounds-cover-equal-count-groups
  (is (= [nil nil nil nil nil] (#'demo/trend-quintile-bounds [])))
  (is (= [[0 1] [2 3] [4 5] [6 7] [8 9]]
         (#'demo/trend-quintile-bounds (range 10))))
  (is (= [[0 2] [3 5] [6 8] [9 11] [12 14]]
         (#'demo/trend-quintile-bounds (range 15)))))

(deftest rows-need-five-valid-observations
  (is (nil? (#'demo/classify-row {:ticker "X" :ni [1 2 3 4]})))
  (is (nil? (#'demo/classify-row {:ticker "X" :ni [1 2 3 4 nil]})))
  (is (some? (#'demo/classify-row {:ticker "X" :ni [nil 1 2 3 4 5]})))
  (is (some? (#'demo/classify-row {:ticker "X" :ni [1 2 3 4 5]}))))

(deftest trend-significance-threshold
  (is (#'demo/trend-significant? {:r-squared 0.8}))
  (is (#'demo/trend-significant? {:r-squared 1.0}))
  (is (not (#'demo/trend-significant? {:r-squared 0.79})))
  (is (not (#'demo/trend-significant? {}))))

(deftest short-series-still-classifies
  (testing "two equal values"
    (is (= :stable (:model (demo/classify-series [4 4])))))
  (testing "two distinct values"
    (is (= :trend-linear (:model (demo/classify-series [1 2]))))))
