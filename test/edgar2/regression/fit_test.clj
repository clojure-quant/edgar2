(ns edgar2.regression.fit-test
  (:require [clojure.test :refer [deftest is]]
            [edgar2.regression.fit :as fit]
            [edgar2.regression.util :as util]))

(defn- approx=
  ([a b] (approx= a b 1e-9))
  ([a b tol]
   (let [a (double a)
         b (double b)]
     (<= (Math/abs (- a b))
         (* tol (max 1.0 (Math/abs a) (Math/abs b)))))))

(defn- number-preprocess
  [vals]
  (util/point-map vals (fn [v] (when (number? v) (double v)))))

(defn- constant-summarize
  [_]
  {:variation 0.0})

(deftest skipped-slots-advance-index
  (let [result (fit/regression {:error :square} [1 nil 3 4]
                               number-preprocess
                               constant-summarize)]
    (is (approx= 3.0 (:n result)))
    (is (approx= 1.0 (:intercept result)))
    (is (approx= 1.0 (:slope result)))))

(deftest err-normalize-receives-fit-context
  (let [seen (atom nil)
        result (fit/regression {:error :square} [1 2 3]
                               number-preprocess
                               (fn [ctx] (reset! seen ctx) 42.0))]
    (is (= 42.0 (:variation result)))
    (is (= :square (:error @seen)))
    (is (= [[0.0 1.0] [1.0 2.0] [2.0 3.0]] (:points @seen)))
    (is (approx= 0.0 (:e @seen)))
    (is (approx= 1.0 (:r-squared @seen)))
    (is (approx= 1.0 (:intercept result)))
    (is (approx= 1.0 (:slope result)))
    (is (approx= 2.0 (:n result)))))

(deftest square-and-absolute-reach-ols-and-lad
  (let [vals [10 20 31 40]
        points (mapv (fn [i v] [(double i) (double v)])
                     (range) vals)
        square (fit/regression {:error :square} vals
                               number-preprocess
                               constant-summarize)
        absolute (fit/regression {:error :absolute} vals
                                 number-preprocess
                                 constant-summarize)
        [ols-intercept ols-slope] (fit/ols points)
        [lad-intercept lad-slope] (fit/lad points)]
    (is (approx= ols-intercept (:intercept square)))
    (is (approx= ols-slope (:slope square)))
    (is (approx= lad-intercept (:intercept absolute)))
    (is (approx= lad-slope (:slope absolute)))
    (is (not (approx= (:slope square) (:slope absolute))))))

(deftest err-reports-standard-deviation-equivalent
  (let [points [[0.0 1.0] [1.0 3.0] [2.0 2.0] [3.0 5.0]]
        k (count points)
        residual (fn [[x y]] (- y (+ 1.0 (* 1.0 x))))
        sse (reduce + (map (fn [p] (let [d (residual p)] (* d d))) points))
        sae (reduce + (map (fn [p] (Math/abs (residual p))) points))]
    (is (approx= (Math/sqrt (/ sse (- k 2)))
                 (:e (fit/err 1.0 1.0 points :square))))
    (is (approx= (Math/sqrt (* 2.0 (/ sae (- k 2))))
                 (:e (fit/err 1.0 1.0 points :absolute))))
    (let [three (take 3 points)
          sae3 (reduce + (map (fn [p] (Math/abs (residual p))) three))]
      (is (approx= (Math/sqrt (* 2.0 sae3))
                   (:e (fit/err 1.0 1.0 three :absolute)))))
    (is (approx= 0.0 (:e (fit/err 1.0 1.0 (take 2 points) :absolute))))
    (is (approx= 0.0 (:e (fit/err 1.0 1.0 (take 2 points) :square))))))

(deftest err-reports-r-squared
  (let [points [[0.0 1.0] [1.0 3.0] [2.0 2.0] [3.0 5.0]]]
    (is (approx= 0.6571428571428571
                 (:r-squared (fit/err 1.0 1.0 points :square))))
    (is (approx= 0.6571428571428571
                 (:r-squared (fit/err 1.0 1.0 points :absolute))))
    (is (approx= 0.5
                 (:r-squared (fit/err 1.0 1.0 (take 2 points) :square))))
    (is (approx= 0.0
                 (:r-squared (fit/err 1.0 1.0 (take 1 points) :square)))))
  (is (approx= 1.0
               (:r-squared (fit/err 1.0 1.0
                                    [[0.0 1.0] [1.0 2.0] [2.0 3.0]]
                                    :square))))
  (is (approx= 1.0
               (:r-squared (fit/err 5.0 0.0
                                    [[0.0 5.0] [1.0 5.0] [2.0 5.0]]
                                    :square))))
  (is (approx= 0.0
               (:r-squared (fit/err 0.0 0.0
                                    [[0.0 5.0] [1.0 5.0] [2.0 5.0]]
                                    :square))))
  (is (approx= -0.5
               (:r-squared (fit/err 0.0 0.0
                                    [[0.0 0.0] [1.0 10.0] [2.0 0.0]]
                                    :absolute)))))

(deftest too-few-transformed-points-return-nil
  (doseq [vals [[] [nil] [1] [1 nil]]]
    (is (nil? (fit/regression {:error :square} vals
                              number-preprocess
                              constant-summarize)))))

(deftest unknown-error-throws
  (is (thrown? IllegalArgumentException
               (fit/regression {:error :bogus} [1 2]
                               number-preprocess
                               constant-summarize))))

(deftest ols-and-lad-still-behave-as-before
  (is (= [1.0 2.0] (fit/ols [[0.0 1.0] [1.0 3.0]])))
  (is (nil? (fit/ols [[1.0 1.0] [1.0 2.0]])))
  (is (= [1.0 2.0] (fit/lad [[0.0 1.0] [1.0 3.0]])))
  (is (nil? (fit/lad [[1.0 1.0] [1.0 2.0]]))))
