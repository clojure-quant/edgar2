(ns edgar2.concepts-series-test
  "series-mixed on report-table for filers that change concept tags over time."
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.test :refer [deftest is testing]]
            [edgar.api :as e]
            [edgar.xbrl :as xbrl]
            [edgar2.concepts :as concepts]
            [edgar2.download.facts :as dl]
            [edgar2.facts-ds :as facts-ds]
            [jsonista.core :as json]
            [tech.v3.dataset :as ds])
  (:import [java.util.zip ZipFile]))

(def companyfacts-zip "data/companyfacts.zip")

(defn- zip-available?
  []
  (.exists (io/file companyfacts-zip)))

(defn- load-facts
  "Companyfacts dataset for a 10-digit CIK from the local zip."
  [zf cik]
  (let [entry (.getEntry zf (str "CIK" cik ".json"))]
    (when-not entry
      (throw (ex-info "Missing CIK json in companyfacts.zip" {:cik cik})))
    (with-open [in (.getInputStream zf entry)]
      (xbrl/facts->dataset
       (json/read-value in json/keyword-keys-object-mapper)))))

(defn- concept-series
  [dss concept-id]
  (let [tags (:data (concepts/get-concept-id concept-id))
        table (facts-ds/report-table dss :y tags)]
    (concepts/series-mixed table tags)))

(defn- concepts-used
  [series]
  (mapv :concept series))

(defn- switches?
  "True when the series uses at least two distinct concepts and the first
  year of `later` is after the last year of `earlier`."
  [series earlier later]
  (let [by-c (group-by :concept series)
        early-ends (mapv :end (by-c earlier))
        late-ends (mapv :end (by-c later))]
    (and (seq early-ends)
         (seq late-ends)
         (neg? (compare (last early-ends) (first late-ends))))))

(deftest series-mixed-net-income-rail
  ;; FreightCar America: NetIncomeLoss through 2021, then ProfitLoss.
  (when (zip-available?)
    (e/init! dl/identity-header)
    (with-open [zf (ZipFile. companyfacts-zip)]
      (let [series (concept-series (load-facts zf "0001320854") :net-income)
            used (set (concepts-used series))]
        (testing "uses both NetIncomeLoss and ProfitLoss"
          (is (contains? used :NetIncomeLoss))
          (is (contains? used :ProfitLoss)))
        (testing "ProfitLoss years follow NetIncomeLoss years"
          (is (switches? series :NetIncomeLoss :ProfitLoss)))
        (testing "oldest-first order"
          (is (= (mapv :end series)
                 (vec (sort (mapv :end series))))))))))

(deftest series-mixed-revenue-abt
  ;; Abbott: SalesRevenueNet, then ASC 606 contract revenue.
  (when (zip-available?)
    (e/init! dl/identity-header)
    (with-open [zf (ZipFile. companyfacts-zip)]
      (let [series (concept-series (load-facts zf "0000001800") :revenue)
            used (set (concepts-used series))]
        (is (contains? used :SalesRevenueNet))
        (is (contains? used :RevenueFromContractWithCustomerExcludingAssessedTax))
        (is (switches? series
                       :SalesRevenueNet
                       :RevenueFromContractWithCustomerExcludingAssessedTax))))))

(deftest series-mixed-cost-of-revenue-abt
  (when (zip-available?)
    (e/init! dl/identity-header)
    (with-open [zf (ZipFile. companyfacts-zip)]
      (let [series (concept-series (load-facts zf "0000001800") :cost-of-revenue)]
        (is (switches? series :CostOfGoodsSold :CostOfGoodsAndServicesSold))))))

(deftest series-mixed-revenue-amd
  ;; AMD: SalesRevenueGoodsNet / SalesRevenueNet, then ASC 606.
  (when (zip-available?)
    (e/init! dl/identity-header)
    (with-open [zf (ZipFile. companyfacts-zip)]
      (let [series (concept-series (load-facts zf "0000002488") :revenue)
            used (set (concepts-used series))]
        (is (seq (set/intersection used #{:SalesRevenueGoodsNet :SalesRevenueNet})))
        (is (contains? used :RevenueFromContractWithCustomerExcludingAssessedTax))
        (let [early (if (some #(= :SalesRevenueGoodsNet (:concept %)) series)
                      :SalesRevenueGoodsNet
                      :SalesRevenueNet)]
          (is (switches? series
                         early
                         :RevenueFromContractWithCustomerExcludingAssessedTax)))))))

(deftest series-mixed-cost-of-revenue-amd
  (when (zip-available?)
    (e/init! dl/identity-header)
    (with-open [zf (ZipFile. companyfacts-zip)]
      (let [series (concept-series (load-facts zf "0000002488") :cost-of-revenue)]
        (is (switches? series :CostOfGoodsSold :CostOfGoodsAndServicesSold))))))

(deftest series-mixed-empty-table
  (is (= [] (concepts/series-mixed (ds/->dataset [])
                                   (:data (concepts/get-concept-id :net-income))))))
