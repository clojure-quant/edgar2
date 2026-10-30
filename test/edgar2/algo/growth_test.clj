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
                :earnings-future-3-yield 1
                :marketcap 200e6}
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
               (item "FAIL-MC" {:marketcap 50e6})
               (item "NO-YIELD" {:earnings-future-3-yield nil})]
        {:keys [rows lines]} (growth/calculate items {})
        counts (mapv #(Long/parseLong (re-find #"\d+$" %)) (butlast lines))
        count-at (fn [line]
                   (let [n (re-find #"\d+$" line)]
                     (- (count line) (count n))))]
    (is (= ["PASS-HI" "PASS-LO" "NO-YIELD"] (mapv :ticker rows)))
    (is (= [10 9 7 6 5 4 3] counts))
    (is (re-matches #"universe\s+10" (nth lines 0)))
    (is (re-matches #"annual-count >= 7\s+9" (nth lines 1)))
    (is (re-matches #"earnings-stabilized-yoy between 12 and 65\s+7" (nth lines 2)))
    (is (re-matches #"sales-growth-avg-yoy >= 12\s+6" (nth lines 3)))
    (is (re-matches #"revenue-score >= 65\s+5" (nth lines 4)))
    (is (re-matches #"operating-income-score >= 70\s+4" (nth lines 5)))
    (is (re-matches #"mc >= 100 \(\$ millions\)\s+3" (nth lines 6)))
    (is (= growth/sort-line (last lines)))
    (is (apply = (map count (butlast lines))))
    (is (apply = (map count-at (rest (butlast lines)))))
    (is (str/starts-with? (first lines) "universe"))
    (is (not (str/starts-with? (first lines) " ")))))

(deftest calculate-reads-thresholds-from-params
  (let [items [(item "KEEP" {:annual-count 8
                             :earnings-stabilized-yoy 20
                             :marketcap 2.0e9})
               (item "YOUNG" {:annual-count 7 :marketcap 2.0e9})
               (item "SMALL" {:annual-count 8 :marketcap 150e6})
               (item "NOMC" {:annual-count 8 :marketcap nil})]
        {:keys [rows lines]}
        (growth/calculate items {:min-annual-years 8
                                 :min-earnings-growth 15
                                 :max-earnings-growth 40
                                 :min-mc 1000})]
    (is (= ["KEEP"] (mapv :ticker rows)))
    (is (str/starts-with? (first lines) "universe"))
    (is (some #(str/starts-with? % "annual-count >= 8") lines))
    (is (some #(str/starts-with? % "earnings-stabilized-yoy between 15 and 40")
              lines))
    (is (some #(str/starts-with? % "mc >= 1000") lines))))
