(ns edgar2.filing.xbrl.xbrl-shares-test
  (:require [clojure.test :refer [deftest is]]
            [edgar2.filing.xbrl.xbrl-shares :as xbrl-shares]))

; COKE is a 10-K stock with 2 classes, 1 listed, no notes
; Voting Rights: Holders of Class B shares are entitled to 20 votes per share
; , whereas regular Common Stock is entitled to 1 vote per share

(deftest coke-cover-summary-reads-classes-and-listing
  (is (= {:cik "0000317540"
          :company-name "COCA-COLA CONSOLIDATED, INC."
          :end-date "December 31, 2025"
          :shares {:coke/CommonClassUndefinedMember
                   {:diluted 83807000 :outstanding 56517334 :weighted 73658000}
                   :us-gaap/CommonClassBMember
                   {:diluted 10149000 :outstanding 10046960 :weighted 10047000}}
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

; BRK-B is a 10-K stock with 2 listed classes (A and B) and equivalent share classes
; and a lot of notes
; no diluted number of shares 
; different participation ratio between the share classes.

(deftest brk-b-cover-summary-reads-classes-and-listing
  (is (= {:cik "0001067983"
          :company-name "BERKSHIRE HATHAWAY INC"
          :end-date "December 31, 2025"
          :shares {:us-gaap/CommonClassAMember
                   {:diluted nil :outstanding 511820 :weighted nil}
                   :us-gaap/CommonClassBMember
                   {:diluted nil :outstanding 1389605139 :weighted nil}
                   :brka/EquivalentClassAMember
                   {:diluted nil :outstanding nil :weighted 1438223}
                   :brka/EquivalentClassBMember
                   {:diluted nil :outstanding nil :weighted 2157335139}}
          :tickers [{:class {:us-gaap/StatementClassOfStockAxis :us-gaap/CommonClassAMember}
                     :exchange "New York Stock Exchange"
                     :ticker "BRK.A"
                     :title "Class A Common Stock"}
                    {:class {:us-gaap/StatementClassOfStockAxis :us-gaap/CommonClassBMember}
                     :exchange "New York Stock Exchange"
                     :ticker "BRK.B"
                     :title "Class B Common Stock"}]}
         (-> "test/html/BRK-B.html"
             xbrl-shares/load-filing-facts
             xbrl-shares/share-summary))))
;  Class B share outstanding is equal to one-fifteen-hundredth of the equivalent Class A amount. 

;(+ (* 511820 1500) 1389605139)
;;=> 2157335139
;(int (+ 511820 (/ 1389605139 1500)))
;;=> 1438223
;; we may not add up the weighted shares. and we may not add up the non weighted shares

(deftest brk-b-shares-outstanding-sums-the-classes
  (is (= 1390116959
         (-> "test/html/BRK-B.html"
             xbrl-shares/load-filing-facts
             xbrl-shares/share-summary
             :shares
             xbrl-shares/shares-outstanding))))

; AAPL is a 10-K stock with 1 listed class (CommonStockTotal) and returns the share map as is

(deftest aapl-cover-summary-reads-classes-and-listing
  (is (= {:cik "0000320193"
          :company-name "Apple Inc."
          :end-date "September 27, 2025"
          :shares {"CommonStockTotal" {:outstanding 14776353000 :weighted 14948500000 :diluted 15004697000}}
          :tickers [{:class {:us-gaap/StatementClassOfStockAxis :us-gaap/CommonStockMember}
                     :exchange "The Nasdaq Stock Market LLC"
                     :ticker "AAPL"
                     :title "Common Stock, $0.00001 par value per share"}]}
         (-> "test/html/AAPL.html"
             xbrl-shares/load-filing-facts
             xbrl-shares/share-summary))))

(deftest aapl-shares-outstanding-sums-the-classes
  (is (= 14776353000
         (-> "test/html/AAPL.html"
             xbrl-shares/load-filing-facts
             xbrl-shares/share-summary
             :shares
             xbrl-shares/shares-outstanding))))

; CPA is a 20-F with a total and two classes that sum to the total

(deftest cpa-cover-summary-reads-classes-and-listing
  (is (= {:cik "0001345105"
          :company-name "COPA HOLDINGS, S.A."
          :end-date "DECEMBER 31, 2025"
          :shares {"CommonStockTotal" {:outstanding 41138553 :weighted nil :diluted 41258000}
                   :cpa/ClassACommonStockMember {:outstanding 30200428 :weighted nil :diluted nil}
                   :cpa/ClassBCommonStockMember {:outstanding 10938125 :weighted nil :diluted nil}}
          :tickers [{:class {}
                     :exchange "New York Stock Exchange"
                     :ticker "CPA"
                     :title "Class A Common Stock, without par value"}]}
         (-> "test/html/CPA.html"
             xbrl-shares/load-filing-facts
             xbrl-shares/share-summary))))

(deftest cpa-shares-outstanding-sums-the-classes
  (is (= 41138553
         (-> "test/html/CPA.html"
             xbrl-shares/load-filing-facts
             xbrl-shares/share-summary
             :shares
             xbrl-shares/shares-outstanding))))

; TDAC is a 10-K stock with 2 classes (Class A and Class B) and 3 listed tickers
; (TDACU units, TDAC class A ordinary shares, TDACW redeemable warrants)

(deftest tdac-cover-summary-reads-classes-and-listing
  (is (= {:cik "0001926599"
          :company-name "TRANSLATIONAL DEVELOPMENT ACQUISITION CORP."
          :end-date "December 31, 2025"
          :shares {:us-gaap/CommonClassAMember {:outstanding 17250000 :weighted nil :diluted nil}
                   :us-gaap/CommonClassBMember {:outstanding 4657500 :weighted nil :diluted nil}}
          :tickers [{:class {:us-gaap/StatementClassOfStockAxis :TDAC/UnitseachconsistingofoneClassAordinaryshareMember}
                     :exchange "The Nasdaq Stock Market LLC"
                     :ticker "TDACU"
                     :title "Units, each consisting of one Class A ordinary share, $0.0001 par value, and one-half of one redeemable warrant"}
                    {:class {:us-gaap/StatementClassOfStockAxis :TDAC/ClassAordinarysharesMember}
                     :exchange "The Nasdaq Stock Market LLC"
                     :ticker "TDAC"
                     :title "Class A ordinary shares, $0.0001 par value per share"}
                    {:class {:us-gaap/StatementClassOfStockAxis :TDAC/RedeemablewarrantsMember}
                     :exchange "The Nasdaq Stock Market LLC"
                     :ticker "TDACW"
                     :title "Redeemable warrants included as part of the units, each whole warrant exercisable for one Class A ordinary share at an exercise price of $11.50"}]}
         (-> "test/html/TDAC.html"
             xbrl-shares/load-filing-facts
             xbrl-shares/share-summary))))

(deftest tdac-shares-outstanding-sums-the-classes
  (is (= 21907500
         (-> "test/html/TDAC.html"
             xbrl-shares/load-filing-facts
             xbrl-shares/share-summary
             :shares
             xbrl-shares/shares-outstanding))))

; UAL is a 10-K stock with 1 class and facts for multiple legal entities,
; which are filtered out so only the undimensioned total remains.

(deftest ual-cover-summary-reads-classes-and-listing
  (is (= {:cik "0000100517"
          :company-name "United Airlines Holdings, Inc."
          :end-date "December 31, 2025"
          :shares {"CommonStockTotal"
                   {:outstanding 323430419 :weighted 324900000 :diluted 328500000}}
          :tickers [{:class {:us-gaap/StatementClassOfStockAxis :us-gaap/CommonStockMember}
                     :exchange "The Nasdaq Stock Market LLC"
                     :ticker "UAL"
                     :title "Common Stock, $0.01 par value"}
                    {:class {:us-gaap/StatementClassOfStockAxis :us-gaap/WarrantMember}
                     :exchange nil
                     :ticker nil
                     :title "Preferred Stock Purchase Rights"}]}
         (-> "test/html/UAL.html"
             xbrl-shares/load-filing-facts
             xbrl-shares/share-summary))))

(deftest ual-shares-outstanding-is-the-total
  (is (= 323430419
         (-> "test/html/UAL.html"
             xbrl-shares/load-filing-facts
             xbrl-shares/share-summary
             :shares
             xbrl-shares/shares-outstanding))))
