(ns edgar2.algo.industry-test
  (:require [clojure.test :refer [deftest is]]
            [edgar2.algo.growth :as growth]
            [edgar2.algo.industry :as industry]))

(defn- stat
  [ticker sector industry-name subindustry future-yield]
  (cond-> {:ticker ticker
           :entityName (str ticker " Inc")
           :sector sector
           :industry industry-name
           :subindustry subindustry
           :annual-count 8
           :earnings-yield 1.0
           :earnings-stabilized-yield 2.0
           :earnings-stabilized-yoy 10.0
           :revenue-score 80.0
           :operating-income-score 90.0
           :sales-growth-avg-yoy 12.0
           :net-income-change-q-on-y 5.0
           :earnings-variation 1.0}
    (some? future-yield) (assoc :earnings-future-3-yield future-yield)))

(def ^:private stats-rows
  [(stat "AAA" "Manufacturing" "Chemicals" "Pharma" 3.0)
   (stat "BBB" "Manufacturing" "Chemicals" "Pharma" 5.0)
   (stat "CCC" "Manufacturing" "Chemicals" "Industrial" nil)
   (stat "DDD" "Finance" "Banks" "Retail" 1.0)])

(deftest sector-tickers-filters-by-any-subset
  (is (= ["AAA" "BBB" "CCC" "DDD"] (industry/sector-tickers stats-rows {})))
  (is (= ["AAA" "BBB" "CCC"]
         (industry/sector-tickers stats-rows {:sector "Manufacturing"})))
  (is (= ["AAA" "BBB" "CCC"]
         (industry/sector-tickers stats-rows {:industry "Chemicals"})))
  (is (= ["AAA" "BBB"]
         (industry/sector-tickers stats-rows {:subindustry "Pharma"})))
  (is (= ["AAA" "BBB"]
         (industry/sector-tickers stats-rows
                                  {:sector "Manufacturing"
                                   :industry "Chemicals"
                                   :subindustry "Pharma"}))))

(deftest sector-tickers-treats-blank-filters-as-unset
  (is (= ["AAA" "BBB" "CCC" "DDD"]
         (industry/sector-tickers stats-rows
                                  {:sector ""
                                   :industry "  "
                                   :subindustry nil})))
  (is (= ["AAA" "BBB" "CCC"]
         (industry/sector-tickers stats-rows {:sector "Manufacturing"
                                              :industry ""
                                              :subindustry "  "}))))

(deftest sector-tickers-trims-filter-values
  (is (= ["AAA" "BBB" "CCC"]
         (industry/sector-tickers stats-rows {:industry " Chemicals "}))))

(deftest sector-tickers-returns-empty-for-unknown-values
  (is (= [] (industry/sector-tickers stats-rows {:sector "Unknown"})))
  (is (= [] (industry/sector-tickers stats-rows {:industry "Unknown"})))
  (is (= [] (industry/sector-tickers stats-rows {:subindustry "Unknown"}))))

(deftest rows-uses-the-growth-columns
  (is (= growth/columns industry/columns))
  (let [row (first (industry/rows [(stat "AAA" "Manufacturing"
                                        "Chemicals" "Pharma" 4.0)]
                                  {}))]
    (is (= "AAA" (:ticker row)))
    (is (= "AAA Inc" (:entityName row)))
    (is (every? #(contains? row (:key %)) growth/columns))))

(deftest rows-sorts-by-future-yield-with-missing-values-last
  (let [rows (industry/rows stats-rows {})]
    (is (= ["BBB" "AAA" "DDD" "CCC"] (mapv :ticker rows)))))
