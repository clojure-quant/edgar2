(ns edgar2.regression.return-prct-test
  (:require [clojure.test :refer [deftest is]]
            [edgar2.regression.return-prct :as return-prct]))

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

(deftest preprocess-computes-adjacent-returns
  (is (= [[1.0 0.1] [2.0 0.1]]
         (return-prct/preprocess [100 110 121]))))

(deftest preprocess-skips-invalid-pairs
  (is (= [] (return-prct/preprocess [100 nil 121])))
  (is (= [[2.0 1.0]] (return-prct/preprocess [0 10 20])))
  (is (= [[1.0 0.1]] (return-prct/preprocess [100 110 nil 130]))))

(deftest exact-constant-return
  (let [result (return-prct/regression [100 110 121 133.1])]
    (is (approx= 0.0 (:intercept result)))
    (is (approx= 0.1 (:slope result)))
    (is (approx= 3.0 (:n result)))
    (is (approx= 0.0 (:base-index result)))
    (is (approx= 100.0 (:base-value result)))
    (is (approx= 130.0 (return-prct/estimate result 0)))
    (is (approx= 140.0 (return-prct/estimate result 1)))
    (is (approx= 10.0 (return-prct/slope-percent result)))
    (is (approx= 0.0 (:variation result)))))

(deftest varying-returns
  (let [result (return-prct/regression [100 110 120 130])]
    (is (approx= 0.0 (:intercept result)))
    (is (approx= 0.0914141414141414 (:slope result)))
    (is (approx= 127.42424242424242 (return-prct/estimate result 0)))
    (is (approx= 9.14141414141414 (return-prct/slope-percent result)))
    (is (approx= 0.008344803859524403 (:variation result)))
    (is (finite? (:variation result)))
    (is (pos? (:variation result)))))

(deftest leading-nil-anchors-at-first-usable-price
  (let [result (return-prct/regression [nil 100 110 121])]
    (is (approx= 1.0 (:base-index result)))
    (is (approx= 100.0 (:base-value result)))
    (is (approx= 120.0 (return-prct/estimate result 0)))
    (is (approx= 130.0 (return-prct/estimate result 1)))
    (is (approx= 10.0 (return-prct/slope-percent result)))))

(deftest nils-skip-and-do-not-count-as-returns
  (let [vals [100 110 nil 130 143]
        collapsed [100 110 130 143]
        with-nil-result (return-prct/regression vals)
        collapsed-result (return-prct/regression collapsed)]
    (is (approx= 2.0 (:n with-nil-result)))
    (is (approx= 3.0 (:n collapsed-result)))
    (is (approx= 120.0 (return-prct/estimate with-nil-result 0)))
    (is (not (approx= 120.0 (return-prct/estimate collapsed-result 0))))))

(deftest too-few-returns-return-nil
  (doseq [vals [[] [100] [100 110] [100 nil 121] [0 10]]]
    (let [result (return-prct/regression vals)]
      (is (nil? result))
      (is (nil? (return-prct/estimate result 0)))
      (is (nil? (return-prct/slope-percent result))))))

(deftest absolute-error-uses-median-and-mean-absolute-deviation
  (let [vals [100 110 121 130 145]
        points (return-prct/preprocess vals)
        returns (mapv second points)
        square (return-prct/regression {:error :square} vals)
        absolute (return-prct/regression {:error :absolute} vals)
        sorted (vec (sort returns))
        median (/ (+ (nth sorted 1) (nth sorted 2)) 2.0)
        mean-abs-dev (/ (reduce + (map #(Math/abs (- % median)) returns))
                        (count returns))]
    (is (not (approx= (:slope square) (:slope absolute))))
    (is (approx= median (:slope absolute)))
    (is (approx= mean-abs-dev (:variation absolute)))
    (is (not (approx= (:variation square) (:variation absolute))))
    (is (approx= (:variation square)
                 (:variation (return-prct/regression vals))))))
