(ns edgar2.algo.growth-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [edgar2.algo.growth :as growth]))

(defn- item
  [ticker overrides]
  {:ticker ticker
   :row (merge {:ticker ticker
                :annual-count 8
                :earnings-stabilized-yoy 30
                :sales-growth-avg-yoy 20
                :revenue-score 80
                :operating-income-score 80
                :earnings-future-3-yield 1}
               overrides)})

(deftest calculate-filters-in-stages-then-sorts-by-future-yield
  (let [items [(item "PASS-LO" {:annual-count 7
                                :earnings-stabilized-yoy 12
                                :sales-growth-avg-yoy 15
                                :revenue-score 65
                                :operating-income-score 70
                                :earnings-future-3-yield 2})
               (item "PASS-HI" {:earnings-stabilized-yoy 65
                                :earnings-future-3-yield 5})
               (item "FAIL-ANN" {:annual-count 6})
               (item "FAIL-YOY-LO" {:earnings-stabilized-yoy 11.9})
               (item "FAIL-YOY-HI" {:earnings-stabilized-yoy 65.1})
               (item "FAIL-SALES" {:sales-growth-avg-yoy 11.9})
               (item "FAIL-REV" {:revenue-score 64.9})
               (item "FAIL-OI" {:operating-income-score 69.9})
               (item "NO-YIELD" {:earnings-future-3-yield nil})]
        {:keys [rows lines]} (growth/calculate items {})
        counts (mapv #(Long/parseLong (re-find #"\d+$" %)) (butlast lines))
        count-at (fn [line]
                   (let [n (re-find #"\d+$" line)]
                     (- (count line) (count n))))]
    (is (= ["PASS-HI" "PASS-LO" "NO-YIELD"] (mapv :ticker rows)))
    (is (= [8 6 5 4 3] counts))
    (is (re-matches #"annual-count >= 7\s+8" (nth lines 0)))
    (is (re-matches #"earnings-stabilized-yoy between 12 and 65\s+6" (nth lines 1)))
    (is (re-matches #"sales-growth-avg-yoy >= 12\s+5" (nth lines 2)))
    (is (re-matches #"revenue-score >= 65\s+4" (nth lines 3)))
    (is (re-matches #"operating-income-score >= 70\s+3" (nth lines 4)))
    (is (= growth/sort-line (last lines)))
    (is (apply = (map count (butlast lines))))
    (is (apply = (map count-at (butlast lines))))
    (is (str/starts-with? (first lines) "annual-count >= 7"))
    (is (not (str/starts-with? (first lines) " ")))))
