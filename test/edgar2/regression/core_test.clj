(ns edgar2.regression.core-test
  (:require [clojure.test :refer [deftest is]]
            [edgar2.regression.core :as regression]
            [edgar2.regression.log :as log]
            [edgar2.regression.linear :as linear]
            [edgar2.regression.asinh :as asinh]
            [edgar2.regression.return-prct :as return-prct]))

(deftest each-regression-type-dispatches-to-its-namespace
  (let [vals [1 2 4 8]]
    (is (= (log/regression {:error :square} vals)
           (regression/regression
            {:regression-type :log :error :square :v vals})))
    (is (= (linear/regression {:error :square} vals)
           (regression/regression
            {:regression-type :linear :error :square :v vals})))
    (is (= (asinh/regression {:error :square} vals)
           (regression/regression
            {:regression-type :asinh :error :square :v vals})))
    (is (= (return-prct/regression {:error :square} vals)
           (regression/regression
            {:regression-type :return-prct :error :square :v vals})))))

(deftest error-defaults-to-square
  (let [vals [1 2 4 7 12]]
    (is (= (log/regression {:error :square} vals)
           (regression/regression
            {:regression-type :log :v vals})))))

(deftest explicit-error-and-opts-are-honored
  (let [vals [1 2 4 7 12]]
    (is (= (log/regression {:error :absolute} vals)
           (regression/regression
            {:regression-type :log :error :absolute :v vals})))
    (is (= (log/regression {:error :absolute} vals)
           (regression/regression
            {:regression-type :log :opts {:error :absolute} :v vals})))))

(deftest unknown-regression-type-throws
  (is (thrown? IllegalArgumentException
               (regression/regression
                {:regression-type :bogus :v [1 2]})))
  (is (thrown? IllegalArgumentException
               (regression/regression {:v [1 2]}))))

(deftest too-few-points-return-nil
  (is (nil? (regression/regression
             {:regression-type :log :v [1.0]})))
  (is (nil? (regression/regression
             {:regression-type :linear :v []})))
  (is (nil? (regression/regression
             {:regression-type :asinh :v [nil]})))
  (is (nil? (regression/regression
             {:regression-type :return-prct :v [1.0 2.0]}))))

(deftest estimate-and-slope-percent-dispatch-to-their-namespace
  (let [vals [1 2 4 8]
        log-opts {:regression-type :log}
        linear-opts {:regression-type :linear}
        asinh-opts {:regression-type :asinh}
        return-prct-opts {:regression-type :return-prct}
        log-fit (regression/regression log-opts vals)
        linear-fit (regression/regression linear-opts vals)
        asinh-fit (regression/regression asinh-opts vals)
        return-prct-fit (regression/regression return-prct-opts vals)]
    (is (= (log/estimate log-fit 0) (regression/estimate log-opts log-fit 0)))
    (is (= (linear/estimate linear-fit 0)
           (regression/estimate linear-opts linear-fit 0)))
    (is (= (asinh/estimate asinh-fit 0)
           (regression/estimate asinh-opts asinh-fit 0)))
    (is (= (return-prct/estimate return-prct-fit 0)
           (regression/estimate return-prct-opts return-prct-fit 0)))
    (is (= (log/slope-percent log-fit)
           (regression/slope-percent log-opts log-fit)))
    (is (= (linear/slope-percent linear-fit)
           (regression/slope-percent linear-opts linear-fit)))
    (is (= (asinh/slope-percent asinh-fit)
           (regression/slope-percent asinh-opts asinh-fit)))
    (is (= (return-prct/slope-percent return-prct-fit)
           (regression/slope-percent return-prct-opts return-prct-fit)))))

(deftest estimate-and-slope-percent-unknown-type-throws
  (let [fit {:intercept 0.0 :slope 1.0 :n 1.0 :variation 0.0 :mean 1.0}]
    (is (thrown? IllegalArgumentException
                 (regression/estimate {:regression-type :bogus} fit 0)))
    (is (thrown? IllegalArgumentException
                 (regression/slope-percent {:regression-type :bogus} fit)))))

(deftest estimate-and-slope-percent-nil-fit-return-nil
  (is (nil? (regression/estimate {:regression-type :log} nil 0)))
  (is (nil? (regression/slope-percent {:regression-type :log} nil)))
  (is (nil? (regression/estimate {:regression-type :return-prct} nil 0)))
  (is (nil? (regression/slope-percent {:regression-type :return-prct} nil))))
