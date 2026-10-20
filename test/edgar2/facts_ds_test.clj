(ns edgar2.facts-ds-test
  "report-table wide layout: concept columns can be sparse across periods."
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [edgar.api :as e]
            [edgar.xbrl :as xbrl]
            [edgar2.download.facts :as dl]
            [edgar2.facts-ds :as facts-ds]
            [jsonista.core :as json]
            [tech.v3.dataset :as ds])
  (:import [java.util.zip ZipFile]))

(def companyfacts-zip "data/companyfacts.zip")

(def demo-fields
  "Same shape as edgar2.demo/demo-fields, plus CostOfGoodsSold so the
  pre-ASC-606 cost series is visible beside CostOfGoodsAndServicesSold."
  [[:dei :EntityCommonStockSharesOutstanding]
   [:us-gaap :CostOfGoodsAndServicesSold]
   [:us-gaap :CommonStockSharesOutstanding]
   [:us-gaap :CostOfGoodsSold]])

(defn- zip-available?
  []
  (.exists (io/file companyfacts-zip)))

(defn- load-facts
  [zf cik]
  (let [entry (.getEntry zf (str "CIK" cik ".json"))]
    (when-not entry
      (throw (ex-info "Missing CIK json in companyfacts.zip" {:cik cik})))
    (with-open [in (.getInputStream zf entry)]
      (xbrl/facts->dataset
       (json/read-value in json/keyword-keys-object-mapper)))))

(defn- ends-with-number
  "Period-ends (oldest first) where `col` is numeric."
  [table col]
  (->> (ds/rows table {:nil-missing? true})
       reverse
       (keep (fn [r]
               (when (number? (get r col))
                 (str (:end r)))))
       vec))

(defn- ends-missing
  "Period-ends (oldest first) where `col` is not numeric."
  [table col]
  (->> (ds/rows table {:nil-missing? true})
       reverse
       (keep (fn [r]
               (when-not (number? (get r col))
                 (str (:end r)))))
       vec))

(deftest annual-and-quarterly-period-labels-aapl
  "`:period` is fy-fp; 10-K rows with fp FY become fy-Q4 on the quarterly table."
  (when (zip-available?)
    (e/init! dl/identity-header)
    (with-open [zf (ZipFile. companyfacts-zip)]
      (let [dss (load-facts zf "0000320193")
            annual (facts-ds/annual-periods dss)
            quarterly (facts-ds/quarterly-periods dss)
            a-rows (vec (ds/rows annual {:nil-missing? true}))
            q-rows (vec (ds/rows quarterly {:nil-missing? true}))]
        (testing "annual periods"
          (is (every? :period a-rows))
          (is (every? #(= "FY" (str (:fp %))) a-rows))
          (is (every? #(= (:period %) (str (:fy %) "-FY")) a-rows))
          (is (not-any? #(str/starts-with? (str (:form %)) "10-Q") a-rows)))
        (testing "quarterly periods"
          (is (every? :period q-rows))
          (is (every? #(not= "FY" (str (:fp %))) q-rows))
          (is (some #(= "Q4" (str (:fp %))) q-rows))
          (is (some #(and (#{"10-Q" "10-Q/A"} (str (:form %)))
                          (= (:period %) (str (:fy %) "-" (:fp %))))
                    q-rows)))))))

(deftest annual-periods-one-per-filing-date-mo
  "MO 10-Ks restate several FY windows on one filed date; keep only max :start."
  (when (zip-available?)
    (e/init! dl/identity-header)
    (with-open [zf (ZipFile. companyfacts-zip)]
      (let [rows (vec (ds/rows (facts-ds/annual-periods (load-facts zf "0000764180"))
                               {:nil-missing? true}))
            by-filed (group-by #(str (:filed %)) rows)
            filing-2026 (filter #(= "2026-02-25" (str (:filed %))) rows)]
        (is (every? #(= 1 (count %)) (vals by-filed)))
        (is (= 1 (count filing-2026)))
        (is (= "2025-12-31" (str (:end (first filing-2026)))))
        (is (= "2025-01-01" (str (:start (first filing-2026)))))))))

(deftest quarterly-periods-no-fy-mo
  "MO must not keep 10-K windows tagged `:fp` FY as quarters."
  (when (zip-available?)
    (e/init! dl/identity-header)
    (with-open [zf (ZipFile. companyfacts-zip)]
      (let [rows (vec (ds/rows (facts-ds/quarterly-periods (load-facts zf "0000764180"))
                               {:nil-missing? true}))
            ends-2024-q4 (filter #(= "2024-12-31" (str (:end %))) rows)
            bad-q3-as-q4 (filter #(and (= "2024-09-30" (str (:end %)))
                                       (= "2024-Q4" (:period %)))
                                rows)]
        (is (seq rows))
        (is (every? #(not= "FY" (str (:fp %))) rows))
        (is (seq ends-2024-q4))
        (is (every? #(= "Q4" (str (:fp %))) ends-2024-q4))
        (is (empty? bad-q3-as-q4))))))

(deftest report-table-sparse-cost-columns-abt
  "Abbott: CostOfGoodsSold has early annual values then stops; later years
  only carry CostOfGoodsAndServicesSold. report-table keeps both columns
  with nils where that concept was not filed."
  (when (zip-available?)
    (e/init! dl/identity-header)
    (with-open [zf (ZipFile. companyfacts-zip)]
      (let [table (facts-ds/report-table (load-facts zf "0000001800") :y demo-fields)
            sold (ends-with-number table "CostOfGoodsSold")
            sold-miss (ends-missing table "CostOfGoodsSold")
            services (ends-with-number table "CostOfGoodsAndServicesSold")]
        (testing "wide table has one row per annual period, newest first"
          (is (pos? (ds/row-count table)))
          (is (= (mapv #(str (:end %)) (ds/rows table {:nil-missing? true}))
                 (vec (sort #(compare %2 %1)
                            (mapv #(str (:end %))
                                  (ds/rows table {:nil-missing? true})))))))
        (testing "period columns are only :filed :period :end"
          (let [meta-cols (set (map name facts-ds/report-period-cols))
                concept-cols #{"CostOfGoodsSold" "CostOfGoodsAndServicesSold"
                               "EntityCommonStockSharesOutstanding"
                               "CommonStockSharesOutstanding"}
                cols (set (map name (ds/column-names table)))]
            (is (set/subset? meta-cols cols))
            (is (empty? (set/difference cols (set/union meta-cols concept-cols))))
            (is (every? :period (ds/rows table {:nil-missing? true})))))
        (testing "CostOfGoodsSold stops — early years present, later missing"
          (is (seq sold))
          (is (seq sold-miss))
          (is (neg? (compare (last sold) (first sold-miss)))))
        (testing "CostOfGoodsAndServicesSold continues after CostOfGoodsSold ends"
          (is (seq services))
          (is (neg? (compare (last sold) (last services)))))))))
