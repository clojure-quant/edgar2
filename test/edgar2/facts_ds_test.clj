(ns edgar2.facts-ds-test
  "report-table wide layout: concept columns can be sparse across periods.

  Fixtures under test/edgar2/facts are companyfacts documents copied out of
  companyfacts.zip. The suite does not read the zip."
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [edgar.xbrl :as xbrl]
            [edgar2.financials.facts-ds :as facts-ds]
            [jsonista.core :as json]
            [tech.v3.dataset :as ds])
  (:import [java.time LocalDate]))

(def facts-dir "test/edgar2/facts")

(def demo-fields
  "Same shape as edgar2.demo/demo-fields, plus CostOfGoodsSold so the
  pre-ASC-606 cost series is visible beside CostOfGoodsAndServicesSold."
  [[:dei :EntityCommonStockSharesOutstanding]
   [:us-gaap :CostOfGoodsAndServicesSold]
   [:us-gaap :CommonStockSharesOutstanding]
   [:us-gaap :CostOfGoodsSold]])

(defn- load-facts
  "Companyfacts dataset for a fixture named `ticker`.json."
  [ticker]
  (let [f (io/file facts-dir (str ticker ".json"))]
    (when-not (.exists f)
      (throw (ex-info "Missing facts fixture" {:path (str f)})))
    (xbrl/facts->dataset
     (json/read-value (slurp f) json/keyword-keys-object-mapper))))

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
  (let [dss (load-facts "AAPL")
        annual (facts-ds/annual-periods dss)
        quarterly (facts-ds/quarterly-periods dss)
        a-rows (vec (ds/rows annual {:nil-missing? true}))
        q-rows (vec (ds/rows quarterly {:nil-missing? true}))]
    (testing "annual periods"
      (is (every? :period a-rows))
      (is (every? #(= "FY" (str (:fp %))) a-rows))
      (is (every? #(str/ends-with? (:period %) "-FY") a-rows))
      (is (every? (fn [row]
                    (let [end-year (str (.getYear (LocalDate/parse (str (:end row)))))]
                      (if (= end-year (str (:fy row)))
                        (= (:period row) (str (:fy row) "-FY"))
                        (= (:period row) (str end-year "-FY")))))
                  a-rows))
      (is (not-any? #(str/starts-with? (str (:form %)) "10-Q") a-rows)))
    (testing "quarterly periods"
      (is (every? :period q-rows))
      (is (every? #(not= "FY" (str (:fp %))) q-rows))
      (is (some #(= "Q4" (str (:fp %))) q-rows))
      (is (some #(and (#{"10-Q" "10-Q/A"} (str (:form %)))
                      (= (:period %) (str (:fy %) "-" (:fp %))))
                q-rows)))))

(deftest annual-periods-one-per-filing-date-mo
  "MO 10-Ks repeat prior years on one filed date. A year that already has
  its own 10-K is not kept again. The 2026-02-25 filing keeps only FY 2025."
  (let [rows (vec (ds/rows (facts-ds/annual-periods (load-facts "MO"))
                           {:nil-missing? true}))
        filing-2026 (filter #(= "2026-02-25" (str (:filed %))) rows)
        ends (map #(str (:end %)) rows)]
    (is (= (count ends) (count (set ends))))
    (is (= 1 (count filing-2026)))
    (is (= "2025-12-31" (str (:end (first filing-2026)))))
    (is (= "2025-01-01" (str (:start (first filing-2026)))))
    (is (= "2025-02-26" (str (:filed (first (filter #(= "2024-12-31" (str (:end %))) rows))))))))

(deftest quarterly-periods-no-fy-mo
  "MO must not keep 10-K windows tagged `:fp` FY as quarters."
  (let [rows (vec (ds/rows (facts-ds/quarterly-periods (load-facts "MO"))
                           {:nil-missing? true}))
        ends-2024-q4 (filter #(= "2024-12-31" (str (:end %))) rows)
        bad-q3-as-q4 (filter #(and (= "2024-09-30" (str (:end %)))
                                   (= "2024-Q4" (:period %)))
                            rows)]
    (is (seq rows))
    (is (every? #(not= "FY" (str (:fp %))) rows))
    (is (seq ends-2024-q4))
    (is (every? #(= "Q4" (str (:fp %))) ends-2024-q4))
    (is (empty? bad-q3-as-q4))))

(deftest select-data-nil-fields-keeps-every-concept
  "Nil fields is no concept filter. A supplied list still drops the others."
  (let [dss (load-facts "AAPL")
        concepts (fn [fields]
                   (set (map #(str (:concept %))
                             (ds/rows (facts-ds/select-data dss :y fields)
                                      {:nil-missing? true}))))
        all (concepts nil)
        limited (concepts demo-fields)
        table (facts-ds/report-table dss :y nil)]
    (is (contains? all "NetIncomeLoss"))
    (is (not (contains? limited "NetIncomeLoss")))
    (is (set/subset? limited all))
    (is (> (count all) (count limited)))
    (is (contains? (set (map name (ds/column-names table))) "NetIncomeLoss"))
    (is (= (ds/row-count (facts-ds/annual-periods dss))
           (ds/row-count table)))))

(deftest report-table-sparse-cost-columns-abt
  "Abbott: CostOfGoodsSold has early annual values then stops; later years
  only carry CostOfGoodsAndServicesSold. report-table keeps both columns
  with nils where that concept was not filed."
  (let [table (facts-ds/report-table (load-facts "ABT") :y demo-fields)
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
      (is (neg? (compare (last sold) (last services)))))))

(deftest report-table-ignores-proxy-net-income-jakk
  "JAKK's 2026-04-22 DEF 14A repeats 2023–2025 NetIncomeLoss at 1000× the
  10-K (thousands tagged as dollars). The annual table keeps the 10-K."
  (let [table (facts-ds/report-table (load-facts "JAKK")
                                     :y
                                     [[:us-gaap :NetIncomeLoss]])
        by-end (into {} (map (fn [r] [(str (:end r)) (get r "NetIncomeLoss")])
                             (ds/rows table {:nil-missing? true})))]
    (is (= 91413000.0 (double (get by-end "2022-12-31"))))
    (is (= 38406000.0 (double (get by-end "2023-12-31"))))
    (is (= 33920000.0 (double (get by-end "2024-12-31"))))
    (is (= 9871000.0 (double (get by-end "2025-12-31"))))))

(deftest annual-periods-predecessor-years-sw
  "SW's 2024 10-K carries 2022 and 2023 with no 10-K of their own.
  The 2025 10-K repeats 2023 and 2024; those years stay on the 2024 filing.
  Only 2025 comes from the 2025 filing."
  (let [dss (load-facts "SW")
        periods (vec (ds/rows (facts-ds/annual-periods dss) {:nil-missing? true}))
        by-end (into {} (map (fn [r] [(str (:end r)) r]) periods))
        table (facts-ds/report-table dss :y
                                     [[:us-gaap :NetIncomeLoss]
                                      [:us-gaap :SellingGeneralAndAdministrativeExpense]])
        ni (into {} (map (fn [r] [(str (:end r)) (get r "NetIncomeLoss")])
                         (ds/rows table {:nil-missing? true})))
        sga (into {} (map (fn [r]
                            [(str (:end r))
                             (get r "SellingGeneralAndAdministrativeExpense")])
                          (ds/rows table {:nil-missing? true})))]
    (is (= ["2022-12-31" "2023-12-31" "2024-12-31" "2025-12-31"]
           (mapv #(str (:end %)) periods)))
    (is (= ["2022-FY" "2023-FY" "2024-FY" "2025-FY"]
           (mapv :period periods)))
    (is (= "2025-03-07" (str (:filed (get by-end "2022-12-31")))))
    (is (= "2025-03-07" (str (:filed (get by-end "2023-12-31")))))
    (is (= "2025-03-07" (str (:filed (get by-end "2024-12-31")))))
    (is (= "2026-02-27" (str (:filed (get by-end "2025-12-31")))))
    (is (= 1034000000.0 (double (get ni "2022-12-31"))))
    (is (= 825000000.0 (double (get ni "2023-12-31"))))
    (is (= 319000000.0 (double (get ni "2024-12-31"))))
    (is (= 699000000.0 (double (get ni "2025-12-31"))))
    (is (= 1604000000.0 (double (get sga "2023-12-31"))))
    (is (= 2793000000.0 (double (get sga "2024-12-31"))))))
