(ns edgar2.financials.regression.linear-test
  (:require [clojure.test :refer [deftest is]]
            [edgar2.financials.regression.linear :as linear]))

(defn- approx=
  ([a b] (approx= a b 1e-9))
  ([a b tol]
   (let [a (double a)
         b (double b)]
     (<= (Math/abs (- a b))
         (* tol (max 1.0 (Math/abs a) (Math/abs b)))))))

(defn- finite?
  [x]
  (and (number? x)
       (not (Double/isNaN (double x)))
       (not (Double/isInfinite (double x)))))

(deftest exact-line-has-known-growth-and-zero-variation
  (let [result (linear/linear-regression [10 20 30 40])]
    (is (approx= 40.0 (:earnings-stabilized-yoy result)))
    (is (approx= 40.0 (:earnings-stabilized result)))
    (is (approx= 0.0 (:earnings-variation result)))))

(deftest negatives-and-zeros-stay-in-fit
  (let [result (linear/linear-regression [-10 0 10 20])]
    (is (map? result))
    (is (every? finite? (vals result)))
    (is (approx= 100.0 (:earnings-stabilized-yoy result)))
    (is (approx= 20.0 (:earnings-stabilized result)))))

(deftest symmetric-series-stays-finite
  (let [result (linear/linear-regression [-10 -5 0 5 10])]
    (is (every? finite? (vals result)))
    (is (approx= (* 100.0 (/ 5.0 6.0))
                 (:earnings-stabilized-yoy result)))
    (is (approx= 10.0 (:earnings-stabilized result)))))

(deftest all-zero-series-is-zero
  (let [result (linear/linear-regression [0 0 0])]
    (is (approx= 0.0 (:earnings-stabilized-yoy result)))
    (is (approx= 0.0 (:earnings-stabilized result)))
    (is (approx= 0.0 (:earnings-variation result)))))

(deftest constant-series-is-zero
  (let [result (linear/linear-regression [5 5 5])]
    (is (approx= 0.0 (:earnings-stabilized-yoy result)))
    (is (approx= 5.0 (:earnings-stabilized result)))
    (is (approx= 0.0 (:earnings-variation result)))))

(deftest nils-skip-but-advance-index
  (let [vals [0.0 1.0 2.0 3.0 4.0]
        with-nil (assoc vals 2 nil)
        collapsed (vec (concat (subvec vals 0 2) (subvec vals 3)))
        with-nil-result (linear/linear-regression with-nil)
        collapsed-result (linear/linear-regression collapsed)]
    (is (approx= 4.0 (:earnings-stabilized with-nil-result)))
    (is (not (approx= 4.0 (:earnings-stabilized collapsed-result))))))

(deftest too-few-values-return-nil
  (doseq [vals [[] [nil] [1.0] [1.0 nil]]]
    (is (nil? (linear/linear-regression vals)))))

(deftest absolute-error-only-changes-variation
  (let [vals [10 20 31 40]
        square (linear/regression {:error :square} vals)
        absolute (linear/regression {:error :absolute} vals)]
    (is (approx= (:earnings-stabilized-yoy square)
                 (:earnings-stabilized-yoy absolute)))
    (is (approx= (:earnings-stabilized square)
                 (:earnings-stabilized absolute)))
    (is (not (approx= (:earnings-variation square)
                      (:earnings-variation absolute))))
    (is (approx= (:earnings-variation square)
                 (:earnings-variation (linear/regression vals))))))
