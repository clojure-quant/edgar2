(ns edgar2.screen.util-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [edgar2.screen.util :as util]))

(defn- count-column
  "Index where the trailing count starts."
  [line]
  (let [n (re-find #"\d+$" line)]
    (- (count line) (count n))))

(deftest pipeline-narrows-and-reports-each-stage
  (let [rows (mapv (fn [i] {:n i :k (if (even? i) 10 1)}) (range 9))
        result (util/apply-and-pipeline
                rows
                [{:label "k >= 5" :pred (util/at-least :k 5)}
                 {:label "n >= 4" :pred (util/at-least :n 4)}])
        lines (util/report-pipeline-info result)]
    (is (= [4 6 8] (mapv :n (:tickers result))))
    (is (= [{:label "universe" :n 9}
            {:label "k >= 5" :n 5}
            {:label "n >= 4" :n 3}]
           (:stages result)))
    (is (str/starts-with? (first lines) "universe"))
    (is (str/starts-with? (second lines) "k >= 5"))
    (is (str/ends-with? (first lines) "9"))
    (is (str/ends-with? (second lines) "5"))
    (is (str/ends-with? (nth lines 2) "3"))
    (is (apply = (map count lines)))
    (is (apply = (map count-column lines)))))

(deftest between-is-inclusive-and-rejects-missing
  (let [pred (util/between :g 12 65)
        result (util/apply-and-pipeline
                [{:g 12} {:g 65} {:g 11.9} {:g 65.1} {}]
                [{:label "g between 12 and 65" :pred pred}])]
    (is (= [12 65] (mapv :g (:tickers result))))
    (is (= [{:label "universe" :n 5}
            {:label "g between 12 and 65" :n 2}]
           (:stages result)))))

(deftest missing-pred-is-an-error
  (is (thrown-with-msg? clojure.lang.ExceptionInfo
                        #"needs a :pred"
                        (util/apply-and-pipeline [{:a 1}] [{:label "a"}]))))
