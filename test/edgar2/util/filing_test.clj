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
