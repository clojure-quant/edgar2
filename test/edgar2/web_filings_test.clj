(ns edgar2.web-filings-test
  (:require [clojure.test :refer [deftest is]]
            [edgar2.web.filings :as filings]))

(deftest classify-keeps-form-families
  (is (= :quarterly (filings/classify "10-Q")))
  (is (= :quarterly (filings/classify "10-Q/A")))
  (is (= :annual (filings/classify "20-F")))
  (is (= :current (filings/classify "8-K")))
  (is (= :current (filings/classify "6-K"))))

(deftest classify-quarterly-6k-by-xbrl-flags
  (is (= :quarterly
         (filings/classify {:form "6-K"
                            :isXBRLNumeric 1
                            :isInlineXBRL 1
                            :accessionNumber "0001988894-26-000085"}))
      "numeric inline XBRL is the quarterly statement package")
  (is (= :current
         (filings/classify {:form "6-K"
                            :isXBRLNumeric 0
                            :isInlineXBRL 0
                            :accessionNumber "0001988894-26-000083"}))
      "the same-day earnings release stays a current report")
  (is (= :quarterly
         (filings/classify {:form "6-K" :isInlineXBRL 1}))
      "older submissions omit isXBRLNumeric; inline XBRL is the fallback")
  (is (= :current
         (filings/classify {:form "6-K/A" :isInlineXBRL 0}))))
