(ns edgar2.filing.xbrl.xbrl-shares-test
  (:require [clojure.test :refer [deftest is]]
            [edgar2.filing.xbrl.xbrl-shares :as xbrl-shares]))

(deftest coke-cover-summary-reads-classes-and-listing
  (is (= {:cik "0000317540"
          :company-name "COCA-COLA CONSOLIDATED, INC."
          :end-date "December 31, 2025"
          :shares {:coke/CommonClassUndefinedMember
                   {:diluted nil :outstanding 56517334 :weighted nil}
                   :us-gaap/CommonClassBMember
                   {:diluted nil :outstanding 10046960 :weighted nil}}
          :tickers [{:class {}
                     :exchange "The Nasdaq Global Select Market"
                     :ticker "COKE"
                     :title "Common Stock, par value $1.00 per share"}]}
         (-> "test/html/COKE.html"
             xbrl-shares/load-filing-facts
             xbrl-shares/share-summary))))

(deftest coke-shares-outstanding-sums-the-classes
  (is (= 66564294
         (-> "test/html/COKE.html"
             xbrl-shares/load-filing-facts
             xbrl-shares/share-summary
             :shares
             xbrl-shares/shares-outstanding))))