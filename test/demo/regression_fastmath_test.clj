(ns demo.regression-fastmath-test
  (:require [clojure.test :refer [deftest is testing]]
            [demo.regression-fastmath :as demo]))

(defn- approx=
  ([a b] (approx= a b 1e-9))
  ([a b tol]
   (let [a (double a)
         b (double b)]
     (<= (Math/abs (- a b))
         (* tol (max 1.0 (Math/abs a) (Math/abs b)))))))

(deftest constant-series-is-stable
  (let [result (demo/stable [5 5 5 5 5])]
    (is (approx= 1.0 (:likelihood result)))
    (is (true? (:stable? result)))
    (is (approx= 0.0 (:t result)))
    (is (approx= 1.0 (:p-value result)))))

(deftest exact-rising-line-is-all-trend
  (let [result (demo/stable [1 2 3 4 5])]
    (is (approx= 0.0 (:likelihood result)))
    (is (false? (:stable? result)))
    (is (approx= 0.0 (:p-value result)))
    (is (= Double/POSITIVE_INFINITY (:t result)))))

(deftest noisy-flat-series-favors-stable
  (let [result (demo/stable [10 11 9 10 10 9 11])]
    (is (> (:likelihood result) 0.5))
    (is (true? (:stable? result)))))

(deftest strong-noisy-trend-favors-trend
  (let [result (demo/stable [1 5 2 8 4 12 9 16])]
    (is (< (:likelihood result) 0.5))
    (is (false? (:stable? result)))))

(deftest too-few-values-have-no-test
  (testing "two distinct values"
    (let [result (demo/stable [1 2])]
      (is (nil? (:p-value result)))
      (is (nil? (:likelihood result)))
      (is (false? (:stable? result)))))
  (testing "two equal values"
    (let [result (demo/stable [4 4])]
      (is (true? (:stable? result))))))

(deftest stable-flag-matches-delta-bic-and-p-value-bounds
  (doseq [vals [[5 5 5 5 5]
                [1 2 3 4 5]
                [10 11 9 10 10 9 11]
                [1 5 2 8 4 12 9 16]
                [-10 -5 0 5 10]
                [3 3 4 2 5 4 6]]]
    (let [result (demo/stable vals)]
      (when-some [delta (:delta-bic result)]
        (is (= (:stable? result) (< delta 0))))
      (is (<= 0.0 (:p-value result) 1.0)))))
