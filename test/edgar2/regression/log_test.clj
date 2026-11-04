(ns edgar2.regression.log-test
  (:require [clojure.test :refer [deftest is]]
            [edgar2.regression.log :as log]))

(defn- approx=
  ([a b] (approx= a b 1e-9))
  ([a b tol]
   (let [a (double a)
         b (double b)]
     (<= (Math/abs (- a b))
         (* tol (max 1.0 (Math/abs a) (Math/abs b)))))))

(deftest exact-geometric-series-has-known-growth-and-zero-variation
  (let [result (log/regression [1 2 4 8])]
    (is (approx= 3.0 (:n result)))
    (is (approx= 0.0 (:intercept result)))
    (is (approx= (Math/log 2.0) (:slope result)))
    (is (approx= 100.0 (log/slope-percent result)))
    (is (approx= 8.0 (log/estimate result 0)))
    (is (approx= 16.0 (log/estimate result 1)))
    (is (approx= 4.0 (log/estimate result -1)))
    (is (approx= 0.0 (:variation result)))))

(deftest non-positive-values-drop-while-indices-advance
  (let [with-zero (log/regression [1 2 0 8])
        collapsed (log/regression [1 2 8])]
    (is (approx= 3.0 (:n with-zero)))
    (is (approx= 2.0 (:n collapsed)))
    (is (not (approx= (log/estimate with-zero 0)
                      (log/estimate collapsed 0))))))

(deftest too-few-positive-points-return-nil
  (doseq [vals [[] [nil] [1.0] [1.0 0]]]
    (let [result (log/regression vals)]
      (is (nil? result))
      (is (nil? (log/estimate result 0)))
      (is (nil? (log/slope-percent result))))))

(deftest constant-series-is-zero
  (let [result (log/regression [5 5 5])]
    (is (approx= 0.0 (log/slope-percent result)))
    (is (approx= 5.0 (log/estimate result 0)))
    (is (approx= 5.0 (log/estimate result 1)))
    (is (approx= 0.0 (:variation result)))))

(deftest absolute-error-only-changes-variation
  (let [vals [1 2 4 7 12]
        square (log/regression {:error :square} vals)
        absolute (log/regression {:error :absolute} vals)]
    (is (approx= (:slope square) (:slope absolute)))
    (is (approx= (:intercept square) (:intercept absolute)))
    (is (approx= (:n square) (:n absolute)))
    (is (not (approx= (:variation square) (:variation absolute))))
    (is (approx= (:variation square)
                 (:variation (log/regression vals))))))
