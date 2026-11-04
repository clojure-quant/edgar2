(ns edgar2.regression.fit-test
  (:require [clojure.test :refer [deftest is]]
            [edgar2.regression.fit :as fit]))

(defn- approx=
  ([a b] (approx= a b 1e-9))
  ([a b tol]
   (let [a (double a)
         b (double b)]
     (<= (Math/abs (- a b))
         (* tol (max 1.0 (Math/abs a) (Math/abs b)))))))

(defn- number-preprocess
  [v]
  (when (number? v)
    (double v)))

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

(deftest summarize-fields-are-merged
  (let [result (fit/regression {:error :square} [1 2 3]
                               number-preprocess
                               (fn [{:keys [error k err ys]}]
                                 {:error error :k k :err err :ys ys
                                  :custom 42}))]
    (is (= 42 (:custom result)))
    (is (= :square (:error result)))
    (is (= 3 (:k result)))
    (is (approx= 0.0 (:err result)))
    (is (= [1.0 2.0 3.0] (:ys result)))
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
