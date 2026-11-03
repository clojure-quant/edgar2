(ns edgar2.regression.asinh-test
  (:require [clojure.test :refer [deftest is]]
            [edgar2.regression.log :as log]
            [edgar2.regression.asinh :as asinh]))

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

(deftest positive-series-matches-log-fit
  (let [vals [1034000000 825000000 319000000 699000000]
        log-fit (log/log-regression vals)
        asinh-fit (asinh/asinh-regression vals)]
    (is (approx= (:earnings-stabilized-yoy log-fit)
                 (:earnings-stabilized-yoy asinh-fit)
                 1e-6))
    (is (approx= (:earnings-stabilized log-fit)
                 (:earnings-stabilized asinh-fit)
                 1e-6))))

(deftest losses-and-zeros-stay-in-fit
  (let [pay (asinh/asinh-regression
              [9300000 -513000 22300000 44200000 66900000])
        mixed (asinh/asinh-regression [10 0 -5 20])]
    (is (map? pay))
    (is (every? finite? (vals pay)))
    (is (map? mixed))
    (is (every? finite? (vals mixed)))))

(deftest large-negative-values-stay-finite
  (let [result (asinh/asinh-regression [-1e9 -2e9 -3e9])]
    (is (every? finite? (vals result)))
    (is (neg? (:earnings-stabilized-yoy result)))))

(deftest nils-skip-but-advance-index
  (let [vals (mapv #(Math/sinh (double %)) [0 1 2 3 4])
        with-nil (assoc vals 2 nil)
        collapsed (vec (concat (subvec vals 0 2) (subvec vals 3)))
        with-nil-result (asinh/asinh-regression with-nil)
        collapsed-result (asinh/asinh-regression collapsed)]
    (is (approx= (Math/sinh 4.0)
                 (:earnings-stabilized with-nil-result)))
    (is (not (approx= (Math/sinh 4.0)
                      (:earnings-stabilized collapsed-result))))))

(deftest too-few-points-return-nil
  (doseq [vals [[] [nil] [1.0] [1.0 nil]]]
    (is (nil? (asinh/asinh-regression vals)))))

(deftest constant-series-is-zero
  (let [result (asinh/asinh-regression [0 0 0])]
    (is (approx= 0.0 (:earnings-stabilized-yoy result)))
    (is (approx= 0.0 (:earnings-stabilized result)))
    (is (approx= 0.0 (:earnings-variation result)))))

(deftest exact-asinh-line-has-known-growth-and-zero-variation
  (let [vals (mapv #(Math/sinh (double %)) [0 1 2 3])
        result (asinh/asinh-regression vals)]
    (is (approx= (* 100.0 (- Math/E 1.0))
                 (:earnings-stabilized-yoy result)))
    (is (approx= (Math/sinh 3.0) (:earnings-stabilized result)))
    (is (approx= 0.0 (:earnings-variation result)))))