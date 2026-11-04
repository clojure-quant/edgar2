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

(deftest preprocess-drops-non-numbers-and-keeps-indices
  (is (= [[0.0 0.0] [2.0 (Math/log (+ 1.0 (Math/sqrt 2.0)))]]
         (asinh/preprocess [0 nil 1]))))

(deftest positive-series-matches-log-fit
  (let [vals [1034000000 825000000 319000000 699000000]
        log-fit (log/regression vals)
        asinh-fit (asinh/regression vals)]
    (is (approx= (log/slope-percent log-fit)
                 (asinh/slope-percent asinh-fit)
                 1e-6))
    (is (approx= (log/estimate log-fit 0)
                 (asinh/estimate asinh-fit 0)
                 1e-6))))

(deftest losses-and-zeros-stay-in-fit
  (let [pay (asinh/regression
              [9300000 -513000 22300000 44200000 66900000])
        mixed (asinh/regression [10 0 -5 20])]
    (is (map? pay))
    (is (every? finite? (filter number? (vals pay))))
    (is (map? mixed))
    (is (every? finite? (filter number? (vals mixed))))))

(deftest large-negative-values-stay-finite
  (let [result (asinh/regression [-1e9 -2e9 -3e9])]
    (is (every? finite? (filter number? (vals result))))
    (is (neg? (asinh/slope-percent result)))))

(deftest nils-skip-but-advance-index
  (let [vals (mapv #(Math/sinh (double %)) [0 1 2 3 4])
        with-nil (assoc vals 2 nil)
        collapsed (vec (concat (subvec vals 0 2) (subvec vals 3)))
        with-nil-result (asinh/regression with-nil)
        collapsed-result (asinh/regression collapsed)]
    (is (approx= 4.0 (:n with-nil-result)))
    (is (approx= 3.0 (:n collapsed-result)))
    (is (approx= (Math/sinh 4.0)
                 (asinh/estimate with-nil-result 0)))
    (is (not (approx= (Math/sinh 4.0)
                      (asinh/estimate collapsed-result 0))))))

(deftest too-few-points-return-nil
  (doseq [vals [[] [nil] [1.0] [1.0 nil]]]
    (let [result (asinh/regression vals)]
      (is (nil? result))
      (is (nil? (asinh/estimate result 0)))
      (is (nil? (asinh/slope-percent result))))))

(deftest constant-series-is-zero
  (let [result (asinh/regression [0 0 0])]
    (is (approx= 0.0 (asinh/slope-percent result)))
    (is (approx= 0.0 (asinh/estimate result 0)))
    (is (approx= 0.0 (:variation result)))))

(deftest absolute-error-fits-least-absolute-deviations
  (let [vals [10 0 -5 20]
        square (asinh/regression {:error :square} vals)
        absolute (asinh/regression {:error :absolute} vals)
        asinh-fn (fn [x]
                   (let [x (double x)
                         ax (Math/abs x)
                         m (Math/log (+ ax (Math/sqrt (inc (* ax ax)))))]
                     (if (neg? x) (- m) m)))
        sae (fn [fit]
              (reduce + (map-indexed
                         (fn [i v]
                           (Math/abs (- (asinh-fn v)
                                        (+ (:intercept fit)
                                           (* (:slope fit) i)))))
                         vals)))
        k (count vals)
        expected-variation (* 100.0
                              (- (Math/exp (Math/sqrt (* 2.0 (/ (sae absolute)
                                                                (- k 2)))))
                                 1.0))]
    (is (not (approx= (:slope square) (:slope absolute))))
    (is (not (approx= (:intercept square) (:intercept absolute))))
    (is (= (:n square) (:n absolute)))
    (is (<= (sae absolute) (sae square)))
    (is (not (approx= (:variation square) (:variation absolute))))
    (is (approx= expected-variation (:variation absolute)))
    (is (approx= (:variation square)
                 (:variation (asinh/regression vals))))))

(deftest exact-asinh-line-has-known-growth-and-zero-variation
  (let [vals (mapv #(Math/sinh (double %)) [0 1 2 3])
        result (asinh/regression vals)]
    (is (approx= 3.0 (:n result)))
    (is (approx= (* 100.0 (- Math/E 1.0))
                 (asinh/slope-percent result)))
    (is (approx= (Math/sinh 3.0) (asinh/estimate result 0)))
    (is (approx= (Math/sinh 4.0) (asinh/estimate result 1)))
    (is (approx= 0.0 (:variation result)))))
