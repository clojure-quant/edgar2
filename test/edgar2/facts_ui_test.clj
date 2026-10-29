(ns edgar2.facts-ui-test
  "Field views: edgarjure industry chains, formats, and facts-ds rows."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [edgar.api :as e]
            [edgar.xbrl :as xbrl]
            [edgar2.financials.facts-ds :as facts-ds]
            [edgar2.financials.facts-ui :as facts-ui]
            [jsonista.core :as json]
            [tech.v3.dataset :as ds]))

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
    (is (= :operating-income (:math (get current "Operating Income"))))
    (is (= :operating-expenses (:math (get current "Operating Expenses"))))
    (is (= :revenue (:concept (get current "Revenue"))))
    (is (= :depreciation (:concept (get current "Depreciation"))))
    (is (= "Net Income" (:edgarjure (get current "Net Income"))))
    (is (= :background-pl-green (:style (get current "Net Income"))))
    (is (= :background-pl-green (:style (get current "Gross Profit"))))
    (is (= :background-pl-green (:style (get current "Dividends Paid"))))
    (is (= :dividends-paid (:math (get current "Dividends Paid"))))
    (is (= :background-balance-active (:style (get current "Total Assets"))))
    (is (= :background-balance-passive (:style (get current "Total Liabilities"))))
    (is (= :background-balance-passive (:style (get current "Total Equity"))))
    (is (= :background-pl-green (:style (get bank "Net Interest Income"))))
    (is (= "Net Interest Income" (:edgarjure (get bank "Net Interest Income"))))
    (is (= :background-pl-green (:style (get insurance "Premiums Earned"))))))

(deftest statement-current-and-all-sw
  (let [dss (load-facts "SW")
        current (facts-ui/statement dss :y :current :standard)
        all (facts-ui/statement dss :y :all :standard)
        present (facts-ui/presentable current)
        net-col (ds/column current "Net Income")]
    (is (pos? (ds/row-count current)))
    (is (ds/has-column? current :end))
    (is (ds/has-column? current "Net Income"))
    (is (some number? net-col))
    (is (= :background-pl-green (:style (meta net-col))))
    (is (ds/has-column? current "Total Assets"))
    (is (pos? (ds/row-count all)))
    (is (ds/has-column? all :end))
    (is (> (count (ds/column-names all)) (count (ds/column-names current))))
    (is (ds/has-column? present :field))
    (is (some #(= "Net Income" %) (ds/column present :field)))
    (is (every? #(some number? (vals (dissoc % :field :style :format :section)))
                (ds/rows present {:nil-missing? true})))))

(deftest format-columns-scales-non-eps
  (let [dss (load-facts "AAPL")
        wide (facts-ui/statement dss :y :current :standard)
        scaled (facts-ui/format-columns wide)
        rev (first (filter number? (ds/column wide "Revenue")))
        rev-m (first (filter number? (ds/column scaled "Revenue")))]
    (is (number? rev))
    (is (number? rev-m))
    (is (= (long (/ (double rev) 1.0e6)) (long rev-m)))
    (is (= :background-pl-green
           (:style (meta (ds/column scaled "Net Income")))))))

(deftest to-presentable-column-order
  (let [dss (load-facts "AAPL")
        ;; Newest-first statement, then reverse so periods ascend like
        ;; financials-data's take-n slice.
        wide (facts-ui/statement dss :y :current :standard)
        n (min 10 (ds/row-count wide))
        oldest-first (ds/select-rows wide (vec (range (dec n) -1 -1)))
        ends (mapv #(str (:end %)) (ds/rows oldest-first {:nil-missing? true}))
        present (facts-ui/to-presentable oldest-first)
        cols (vec (ds/column-names present))
        period-cols (filterv string? cols)]
    (is (= :field (first cols)))
    (is (= ends period-cols))
    (is (every? #{:field :style :format :section} (take-last 3 cols)))))

(deftest report-table-full-metadata-and-math
  (let [dss (load-facts "AAPL")
        fields [{:field [:us-gaap :Revenues]
                 :label "Revenue tag"
                 :style :background-pl-green
                 :section :income}
                {:concept :operating-income
                 :label "Operating Income reported"
                 :section :income}
                {:edgarjure "Interest Expense"
                 :label "Interest Expense"
                 :style :background-pl-green
                 :section :income}
                {:math :operating-income
                 :label "Operating Income"
                 :style :background-pl-green
                 :section :income}]
        table (facts-ds/report-table-full dss :y fields)
        reported (vec (ds/column table "Operating Income reported"))
        income (vec (ds/column table "Operating Income"))]
    (is (= {:style :background-pl-green :section :income}
           (select-keys (meta (ds/column table "Interest Expense"))
                        [:style :section])))
    (is (= :income (:section (meta (ds/column table "Revenue tag")))))
    (is (some number? (ds/column table "Interest Expense")))
    (is (some number? reported))
    (is (every? (fn [[calc raw]]
                  (or (not (number? raw)) (= calc raw)))
                (map vector income reported)))))
