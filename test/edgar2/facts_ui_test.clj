(ns edgar2.facts-ui-test
  "Field views: edgarjure industry chains, formats, and facts-ds rows."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [edgar.api :as e]
            [edgar.xbrl :as xbrl]
            [edgar2.financials.facts-ui :as facts-ui]
            [jsonista.core :as json]))

(def facts-dir "test/edgar2/facts")

(defn- load-facts
  [ticker]
  (let [f (io/file facts-dir (str ticker ".json"))]
    (xbrl/facts->dataset
     (json/read-value (slurp f) json/keyword-keys-object-mapper))))

(defn- income-labels
  [view industry]
  (->> (facts-ui/view-fields view industry)
       (filter #(= :income (:section %)))
       (map :label)
       vec))

(deftest view-choices
  (is (= [:current :all :insurance :bank :reit]
         (mapv first facts-ui/view-choices)))
  (is (= :current (facts-ui/parse-view nil)))
  (is (= :bank (facts-ui/parse-view "bank")))
  (is (= :current (facts-ui/parse-view "nope")))
  (is (= [] (facts-ui/view-fields :all :standard))))

(deftest industry-income-matches-edgarjure
  (doseq [industry [:insurance :bank :reit]]
    (let [chains (mapv first (:chains (e/concepts-for :income :industry industry)))
          shown (income-labels industry industry)]
      (is (= chains (vec (take (count chains) shown))))
      (is (= ["Dividends Paid" "Stock Repurchase"]
             (vec (take-last 2 shown)))))))

(deftest formats-and-kinds
  (let [current (into {} (map (juxt :label identity)
                              (facts-ui/view-fields :current :standard)))
        bank (into {} (map (juxt :label identity)
                           (facts-ui/view-fields :bank :bank)))
        insurance (into {} (map (juxt :label identity)
                                (facts-ui/view-fields :insurance :insurance)))]
    (is (= :edgar2 (:kind (get current "Operating Income"))))
    (is (= :edgar2 (:kind (get current "Operating Expenses"))))
    (is (= :edgarjure (:kind (get current "Net Income"))))
    (is (= :background-pl-green (:format (get current "Net Income"))))
    (is (= :background-pl-green (:format (get current "Gross Profit"))))
    (is (= :background-pl-green (:format (get current "Dividends Paid"))))
    (is (= :background-balance-active (:format (get current "Total Assets"))))
    (is (= :background-balance-passive (:format (get current "Total Liabilities"))))
    (is (= :background-balance-passive (:format (get current "Total Equity"))))
    (is (= :background-pl-green (:format (get bank "Net Interest Income"))))
    (is (= :background-pl-green (:format (get insurance "Premiums Earned"))))))

(deftest statement-current-and-all-sw
  (let [dss (load-facts "SW")
        current (facts-ui/statement dss :y :current :standard)
        all (facts-ui/statement dss :y :all :standard)
        net-income (first (filter #(= "Net Income" (:label %)) (:lines current)))]
    (is (seq (:periods current)))
    (is (some number? (:vals net-income)))
    (is (= :background-pl-green (:format net-income)))
    (is (some #(= "Total Assets" (:label %)) (:lines current)))
    (is (some #(= :raw (:kind %)) (:lines all)))
    (is (> (count (:lines all)) (count (:lines current))))))
