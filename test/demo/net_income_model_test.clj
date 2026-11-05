(ns demo.net-income-model-test
  (:require             [clojure.test :refer [deftest is]]
            [demo.net-income-model :as demo]))

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
