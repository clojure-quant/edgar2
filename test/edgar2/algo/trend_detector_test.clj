(ns edgar2.algo.trend-detector-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [edgar2.algo.trend-detector :as trend]
            [edgar2.download.dataset :as dataset]))

(defn- item
  [ticker model r-squared]
  {:ticker ticker
   :model model
   :r-squared r-squared})

(deftest calculate-ticker-sort-ascending
  (let [{:keys [rows lines]}
        (trend/calculate [(item "MSFT" :stable 0.4)
                          (item "AAPL" :trend-linear 0.9)
                          (item "GOOG" :stable 0.1)]
                         {:sort :ticker})]
    (is (= ["AAPL" "GOOG" "MSFT"] (mapv :ticker rows)))
    (is (= ["trend-linear" "stable" "stable"] (mapv :model rows)))
    (is (= ["sorted by ticker"] lines))))

(deftest default-sort-is-ticker
  (let [{:keys [rows]}
        (trend/calculate [(item "B" :stable 0.5)
                          (item "A" :stable 0.5)]
                         {})]
    (is (= ["A" "B"] (mapv :ticker rows)))))

(deftest calculate-model-sort-groups-and-orders-by-r-squared-desc
  (let [{:keys [rows lines]}
        (trend/calculate [(item "A" :stable 0.2)
                          (item "B" :trend-linear 0.3)
                          (item "C" :stable 0.8)
                          (item "D" :trend-linear 0.9)]
                         {:sort :model})]
    (is (= ["stable" "stable" "trend-linear" "trend-linear"]
           (mapv :model rows)))
    (is (= ["C" "A" "D" "B"] (mapv :ticker rows)))
    (is (= ["sorted by model, then r-squared descending"] lines))))

(deftest nil-r-squared-sorts-last-in-its-group
  (let [{:keys [rows]}
        (trend/calculate [(item "A" :stable nil)
                          (item "B" :stable 0.1)
                          (item "C" :stable 0.9)]
                         {:sort :model})]
    (is (= ["C" "B" "A"] (mapv :ticker rows)))
    (is (nil? (:r-squared (last rows))))))

(deftest r-squared-is-rounded-to-three-decimals
  (let [{:keys [rows]} (trend/calculate [(item "A" :stable 0.1234567)] {})]
    (is (= 0.123 (:r-squared (first rows))))))

(deftest parse-sort-accepts-strings-and-keywords
  (let [parse (:parse trend/sort-param)]
    (is (= :ticker (parse :ticker)))
    (is (= :model (parse :model)))
    (is (= :ticker (parse "ticker")))
    (is (= :ticker (parse ":ticker")))
    (is (= :model (parse "model")))
    (is (= :model (parse ":model")))
    (is (nil? (parse "bogus")))))

(deftest model-keyword-is-rendered-as-a-string
  (let [{:keys [rows]} (trend/calculate [(item "A" :trend-linear 0.5)] {})]
    (is (string? (:model (first rows))))
    (is (= "trend-linear" (:model (first rows))))))

(deftest universe-reports-missing-file
  (with-redefs [dataset/read-rows (fn [_] nil)]
    (let [{:keys [items lines]} (trend/universe {})]
      (is (= [] items))
      (is (str/includes? (first lines) "No rows")))))

(deftest universe-reports-count
  (with-redefs [dataset/read-rows (fn [_] [(item "A" :stable 0.1)])]
    (let [{:keys [items lines]} (trend/universe {})]
      (is (= 1 (count items)))
      (is (str/includes? (first lines) "1 rows")))))
