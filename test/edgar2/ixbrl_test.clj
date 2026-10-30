(ns edgar2.ixbrl-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [edgar2.financials.facts-ds :as facts-ds]
            [edgar2.financials.ixbrl :as ixbrl]
            [tech.v3.dataset :as ds]))

(def as-q2
  {:form "6-K"
   :cik "0001988894"
   :accessionNumber "0001988894-26-000085"
   :filingDate "2026-08-18"
   :isXBRLNumeric 1
   :reportDate "2026-06-30"})

(defn- revenue
  [facts start end]
  (->> facts
       (filter #(and (= "RevenueFromContractsWithCustomers" (:concept %))
                     (= start (:start %))
                     (= end (:end %))))
       first
       :val))

(deftest june-2026-quarter-from-cached-6k
  (when (.isDirectory (io/file "data/filings/6-K/0001988894/0001988894-26-000085"))
    (let [facts (ixbrl/filing-facts as-q2)]
      (is (= 1632600000.0 (revenue facts "2026-04-01" "2026-06-30")))
      (is (= 10227200000.0
             (->> facts
                  (filter #(and (= "Assets" (:concept %))
                                (= "2026-06-30" (:end %))
                                (nil? (:start %))))
                  first
                  :val))))))

(deftest pending-annual-is-the-inline-20f-companyfacts-only-covered
  "Companyfacts can list a 20-F as the cover-page share count and still
  lack the annual window. The prior 20-F with no XBRL is not a source."
  (let [rows [{:form "20-F" :accn "0001062993-24-013172"
               :start "2023-04-01" :end "2024-03-31" :filed "2024-07-01"}
              {:form "20-F" :accn "0001062993-26-004458"
               :start nil :end "2026-03-31" :filed "2026-08-18" :fp "FY"}]
        filings [{:form "20-F" :accessionNumber "0001062993-24-013172"
                  :filingDate "2024-07-01" :reportDate "2024-03-31" :isInlineXBRL 1}
                 {:form "20-F" :accessionNumber "0001062993-25-013312"
                  :filingDate "2025-07-31" :reportDate "2025-03-31" :isInlineXBRL 0}
                 {:form "20-F" :accessionNumber "0001062993-26-004458"
                  :filingDate "2026-08-18" :reportDate "2026-03-31" :isInlineXBRL 1}]
        pending (ixbrl/pending-annual-filings filings rows)]
    (is (= ["0001062993-26-004458"] (mapv :accessionNumber pending)))))

(deftest annual-statement-facts-keep-the-year-and-its-comparative
  (let [facts [{:concept "WeightedAverageShares" :start "2025-04-01" :end "2026-03-31"
                :filed "2026-08-18" :val 3494860}
               {:concept "WeightedAverageShares" :start "2024-04-01" :end "2025-03-31"
                :filed "2026-08-18" :val 2758020}
               {:concept "IncreaseDecreaseInNumberOfSharesOutstanding"
                :start "2026-06-01" :end "2026-06-30" :filed "2026-08-18" :val 1500}
               {:concept "EntityCommonStockSharesOutstanding" :start nil :end "2026-03-31"
                :filed "2026-08-18" :val 5029291}
               {:concept "EntityCommonStockSharesOutstanding" :start nil :end "2026-08-01"
                :filed "2026-08-18" :val 9}]
        kept (ixbrl/annual-statement-facts facts)
        by-end (group-by #(str (:end %)) kept)]
    (is (= #{"2026-03-31" "2025-03-31"} (set (keys by-end))))
    (is (every? #(= "FY" (:fp %)) kept))
    (is (= 2025 (:fy (first (get by-end "2025-03-31")))))
    (is (= 2026 (:fy (some #(when (= "EntityCommonStockSharesOutstanding" (:concept %)) %)
                           (get by-end "2026-03-31")))))))

(deftest merged-20f-adds-the-missing-annual-year
  "The 2026 20-F's own year and the 2025 comparative become annual periods.
  The 2024 comparative does not replace the 2024 20-F."
  (let [base [{:accn "0001062993-24-013172" :filed "2024-07-01" :form "20-F"
               :fy 2024 :fp "FY" :start "2023-04-01" :end "2024-03-31" :frame ""}]
        extra (ixbrl/annual-statement-facts
               [{:accn "0001062993-26-004458" :filed "2026-08-18" :form "20-F"
                 :concept "WeightedAverageShares" :start "2025-04-01" :end "2026-03-31"
                 :val 1}
                {:accn "0001062993-26-004458" :filed "2026-08-18" :form "20-F"
                 :concept "WeightedAverageShares" :start "2024-04-01" :end "2025-03-31"
                 :val 1}
                {:accn "0001062993-26-004458" :filed "2026-08-18" :form "20-F"
                 :concept "WeightedAverageShares" :start "2023-04-01" :end "2024-03-31"
                 :val 1}])
        dss (ds/->dataset (concat base extra))
        rows (vec (ds/rows (facts-ds/annual-periods dss) {:nil-missing? true}))
        by-end (into {} (map (fn [r] [(str (:end r)) (str (:period r))]) rows))]
    (is (= {"2024-03-31" "2024-FY"
            "2025-03-31" "2025-FY"
            "2026-03-31" "2026-FY"}
           by-end))
    (is (= "2024-07-01" (str (:filed (some #(when (= "2024-03-31" (str (:end %))) %) rows)))))
    (is (= "2026-08-18" (str (:filed (some #(when (= "2025-03-31" (str (:end %))) %) rows)))))))

(deftest statement-package-skips-the-earnings-release
  (is (ixbrl/statement-package? as-q2 "2025-12-31"))
  (is (not (ixbrl/statement-package?
            {:form "6-K" :isXBRLNumeric 0 :isInlineXBRL 0
             :reportDate "2026-06-30"}
            "2025-12-31")))
  (is (not (ixbrl/statement-package?
            {:form "6-K" :isInlineXBRL 1 :reportDate "2025-09-30"}
            "2025-12-31"))))
