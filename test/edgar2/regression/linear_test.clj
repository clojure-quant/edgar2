(ns edgar2.regression.linear-test
  (:require [clojure.test :refer [deftest is]]
            [edgar2.regression.linear :as linear]))

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
  (let [result (linear/regression [10 20 30 40])]
    (is (approx= 3.0 (:n result)))
    (is (approx= 10.0 (:intercept result)))
    (is (approx= 10.0 (:slope result)))
    (is (approx= 25.0 (:mean result)))
    (is (approx= 40.0 (linear/slope-percent result)))
    (is (approx= 40.0 (linear/estimate result 0)))
    (is (approx= 50.0 (linear/estimate result 1)))
    (is (approx= 0.0 (:variation result)))))

(deftest negatives-and-zeros-stay-in-fit
  (let [result (linear/regression [-10 0 10 20])]
    (is (map? result))
    (is (every? finite? (vals result)))
    (is (approx= 100.0 (linear/slope-percent result)))
    (is (approx= 20.0 (linear/estimate result 0)))))

(deftest symmetric-series-stays-finite
  (let [result (linear/regression [-10 -5 0 5 10])]
    (is (every? finite? (vals result)))
    (is (approx= (* 100.0 (/ 5.0 6.0))
                 (linear/slope-percent result)))
    (is (approx= 10.0 (linear/estimate result 0)))))

(deftest all-zero-series-is-zero
  (let [result (linear/regression [0 0 0])]
    (is (approx= 0.0 (:mean result)))
    (is (approx= 0.0 (linear/slope-percent result)))
    (is (approx= 0.0 (linear/estimate result 0)))
    (is (approx= 0.0 (:variation result)))))

(deftest constant-series-is-zero
  (let [result (linear/regression [5 5 5])]
    (is (approx= 0.0 (linear/slope-percent result)))
    (is (approx= 5.0 (linear/estimate result 0)))
    (is (approx= 0.0 (:variation result)))))

(deftest nils-skip-but-advance-index
  (let [vals [0.0 1.0 2.0 3.0 4.0]
        with-nil (assoc vals 2 nil)
        collapsed (vec (concat (subvec vals 0 2) (subvec vals 3)))
        with-nil-result (linear/regression with-nil)
        collapsed-result (linear/regression collapsed)]
    (is (approx= 4.0 (:n with-nil-result)))
    (is (approx= 3.0 (:n collapsed-result)))
    (is (approx= 4.0 (linear/estimate with-nil-result 0)))
    (is (not (approx= 4.0 (linear/estimate collapsed-result 0))))))

(deftest too-few-values-return-nil
  (doseq [vals [[] [nil] [1.0] [1.0 nil]]]
    (let [result (linear/regression vals)]
      (is (nil? result))
      (is (nil? (linear/estimate result 0)))
      (is (nil? (linear/slope-percent result))))))

(deftest absolute-error-fits-least-absolute-deviations
  (let [vals [10 20 31 40]
        square (linear/regression {:error :square} vals)
        absolute (linear/regression {:error :absolute} vals)
        sae (fn [fit]
              (reduce + (map-indexed
                         (fn [i v]
                           (Math/abs (- v (+ (:intercept fit)
                                             (* (:slope fit) i)))))
                         vals)))]
    (is (not (approx= (:slope square) (:slope absolute))))
    (is (not (approx= (:intercept square) (:intercept absolute))))
    (is (approx= (:mean square) (:mean absolute)))
    (is (<= (sae absolute) (sae square)))
    (is (not (approx= (:variation square) (:variation absolute))))
    (is (approx= (:variation square)
                 (:variation (linear/regression vals))))))
