(ns edgar2.facts-ui-test
  "Field views: edgarjure industry chains, formats, and facts-ds rows."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [edgar.api :as e]
            [edgar.xbrl :as xbrl]
            [edgar2.financials.facts-ds :as facts-ds]
            [edgar2.financials.facts-ui :as facts-ui]
            [edgar2.financials.view.core :as view]
            [edgar2.financials.view.edgarjure :as edgarjure]
            [jsonista.core :as json]
            [tech.v3.dataset :as ds]))

(def facts-dir "test/edgar2/facts")

(defn- load-facts
  [ticker]
  (let [f (io/file facts-dir (str ticker ".json"))]
    (xbrl/facts->dataset
     (json/read-value (slurp f) json/keyword-keys-object-mapper))))

(defn- income-labels
  [view-id industry]
  (->> (view/view-fields view-id industry)
       (filter #(= :income (:section %)))
       (map :label)
       vec))

(deftest view-choices
  (is (= [:default :all :edgarjure-standard :edgarjure-insurance
          :edgarjure-bank :edgarjure-reit]
         (mapv first view/view-choices)))
  (is (= :default (view/parse-view nil)))
  (is (= :edgarjure-bank (view/parse-view "edgarjure-bank")))
  (is (= :default (view/parse-view "nope")))
  (is (= [] (view/view-fields :all :standard))))

(deftest industry-income-matches-edgarjure
  (doseq [industry [:standard :insurance :bank :reit]]
    (let [chains (mapv first (:chains (e/concepts-for :income :industry industry)))
          view-id (keyword (str "edgarjure-" (name industry)))
          shown (income-labels view-id industry)
          from-fn (mapv :label (edgarjure/edgarjure-income-industry industry))]
      (is (= chains shown))
      (is (= chains from-fn))
      (is (= shown from-fn)))))

(deftest formats-and-kinds
  (let [default (into {} (map (juxt :label identity)
                              (view/view-fields :default :standard)))
        bank (into {} (map (juxt :label identity)
                           (view/view-fields :edgarjure-bank :standard)))
        insurance (into {} (map (juxt :label identity)
                                (view/view-fields :edgarjure-insurance :standard)))]
    (is (= :operating-income (:math (get default "Operating Income"))))
    (is (= :operating-expenses (:math (get default "Operating Expenses"))))
    (is (= :revenue (:concept (get default "Revenue"))))
    (is (= :depreciation (:concept (get default "Depreciation"))))
    (is (= "Net Income" (:edgarjure (get default "Net Income"))))
    (is (= :background-pl-green (:style (get default "Net Income"))))
    (is (= :background-pl-green (:style (get default "Gross Profit"))))
    (is (= :background-pl-green (:style (get default "Dividends Paid"))))
    (is (= :dividends-paid (:math (get default "Dividends Paid"))))
    (is (= :background-balance-active (:style (get default "Total Assets"))))
    (is (= :background-balance-passive (:style (get default "Total Liabilities"))))
    (is (= :background-balance-passive (:style (get default "Total Equity"))))
    (is (= :background-pl-green (:style (get bank "Net Interest Income"))))
    (is (= "Net Interest Income" (:edgarjure (get bank "Net Interest Income"))))
    (is (nil? (:math (get bank "Net Interest Income"))))
    (is (= :background-pl-green (:style (get insurance "Premiums Earned"))))))

(deftest statement-current-and-all-sw
  (let [dss (load-facts "SW")
        current (facts-ui/statement dss :y :default :standard)
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
        wide (facts-ui/statement dss :y :default :standard)
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
        wide (facts-ui/statement dss :y :default :standard)
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
