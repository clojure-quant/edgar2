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
  (let [result (log/log-regression [1 2 4 8])]
    (is (approx= 100.0 (:earnings-stabilized-yoy result)))
    (is (approx= 8.0 (:earnings-stabilized result)))
    (is (approx= 0.0 (:earnings-variation result)))))

(deftest non-positive-values-drop-while-indices-advance
  (let [with-zero (log/log-regression [1 2 0 8])
        collapsed (log/log-regression [1 2 8])]
    (is (not (approx= (:earnings-stabilized with-zero)
                      (:earnings-stabilized collapsed))))))

(deftest too-few-positive-points-return-nil
  (doseq [vals [[] [nil] [1.0] [1.0 0]]]
    (is (nil? (log/log-regression vals)))))

(deftest constant-series-is-zero
  (let [result (log/log-regression [5 5 5])]
    (is (approx= 0.0 (:earnings-stabilized-yoy result)))
    (is (approx= 5.0 (:earnings-stabilized result)))
    (is (approx= 0.0 (:earnings-variation result)))))

(deftest absolute-error-only-changes-variation
  (let [vals [1 2 4 7 12]
        square (log/regression {:error :square} vals)
        absolute (log/regression {:error :absolute} vals)]
    (is (approx= (:earnings-stabilized-yoy square)
                 (:earnings-stabilized-yoy absolute)))
    (is (approx= (:earnings-stabilized square)
                 (:earnings-stabilized absolute)))
    (is (not (approx= (:earnings-variation square)
                      (:earnings-variation absolute))))
    (is (approx= (:earnings-variation square)
                 (:earnings-variation (log/regression vals))))))
