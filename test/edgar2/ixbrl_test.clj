(ns edgar2.ixbrl-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [edgar2.financials.ixbrl :as ixbrl]))

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

(deftest statement-package-skips-the-earnings-release
  (is (ixbrl/statement-package? as-q2 "2025-12-31"))
  (is (not (ixbrl/statement-package?
            {:form "6-K" :isXBRLNumeric 0 :isInlineXBRL 0
             :reportDate "2026-06-30"}
            "2025-12-31")))
  (is (not (ixbrl/statement-package?
            {:form "6-K" :isInlineXBRL 1 :reportDate "2025-09-30"}
            "2025-12-31"))))
