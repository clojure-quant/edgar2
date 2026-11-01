(ns edgar2.util.filing-test
  (:require [clojure.test :refer [deftest is]]
            [edgar2.util.filing :as filing]))

(deftest newer-40-f-wins-over-older-20-f
  (is (= "40-F"
         (:form (filing/pick-latest-annual
                 [{:form "8-K" :filingDate "2026-04-01"}
                  {:form "40-F" :filingDate "2026-03-19"
                   :primaryDocument "aem-20251231x40f.htm"}
                  {:form "20-F" :filingDate "2013-03-28"
                   :primaryDocument "a2211934z20-f.htm"}
                  {:form "10-Q" :filingDate "2012-08-01"}])))))

(deftest amendment-with-xbrl-wins-within-the-period
  (is (= "10-K/A"
         (:form (filing/pick-latest-annual
                 [{:form "10-K/A" :filingDate "2026-09-30" :reportDate "2026-05-31" :isXBRL 1}
                  {:form "10-K" :filingDate "2026-09-14" :reportDate "2026-05-31" :isXBRL 0}
                  {:form "10-K" :filingDate "2025-09-14" :reportDate "2025-05-31" :isXBRL 1}])))))

(deftest amendment-without-xbrl-does-not-displace-the-original
  (is (= "10-K"
         (:form (filing/pick-latest-annual
                 [{:form "10-K/A" :filingDate "2026-03-26" :reportDate "2025-12-31" :isXBRL 0}
                  {:form "10-K" :filingDate "2026-03-25" :reportDate "2025-12-31" :isXBRL 1}])))))

(deftest an-amendment-of-an-older-period-does-not-win
  (is (= "10-K"
         (:form (filing/pick-latest-annual
                 [{:form "10-K/A" :filingDate "2026-06-01" :reportDate "2024-12-31" :isXBRL 1}
                  {:form "10-K" :filingDate "2026-03-01" :reportDate "2025-12-31" :isXBRL 1}])))))

(deftest original-follows-a-stub-amendment
  (is (= ["10-K/A" "10-K"]
         (mapv :form
               (filing/pick-annual-filings
                [{:form "10-K/A" :filingDate "2026-05-27" :reportDate "2025-12-31" :isXBRL 1}
                 {:form "10-K" :filingDate "2026-03-13" :reportDate "2025-12-31" :isXBRL 1}])))))
