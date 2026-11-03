(ns demo.xbrl-shares-one
  (:require
   [clojure.pprint :refer [print-table]]
   [edgar2.filing.xbrl.xbrl-shares :as xbrl-shares]
   [edgar2.db.shares :as db-shares]))

(defn shares-ui [summary]
  (->> summary
       :shares
       (map (fn [[k v]]
              (assoc v :class k)))
       (map #(select-keys % [:class :outstanding :weighted :diluted]))
       (into [])))

(defn ticker-ui [summary]
  (->> summary
       :tickers
       (map #(select-keys % [:ticker :exchange :title :class]))
       (into [])))


;; add test for UAL
;; live test for one ticker

;; UAL is reporting shares for a subsidiary

(-> "UAL"
    xbrl-shares/share-summary-ticker
    shares-ui
    print-table)

(-> "UAL"
    xbrl-shares/share-summary-ticker
    ticker-ui
    print-table)

(-> "UAL"
    xbrl-shares/share-summary-ticker
    :shares
    (xbrl-shares/shares-outstanding))


(db-shares/get-shares-ticker "LUV")
(db-shares/get-shares-cik "0000092380")
(db-shares/get-shares-cik "92380")

;; store for unit test

(defn store-test [ticker]
  (let [url (xbrl-shares/ticker->url-file ticker)
        content (slurp url)]
    (spit (str "test/html/" ticker ".html") content)))

(store-test "UAL")

(->> "test/html/BRK-B.html"
     (xbrl-shares/load-filing-facts)
     (filter #(= :dei (:taxonomy %)))
     (filter #(= :EntityCommonStockSharesOutstanding (:concept %)))
     (map #(select-keys % [:entity :value :dimensions])))

; UAL
#_({:dimensions [], :entity {:cik "0000100517"}, :value 323430419}
 {:dimensions [{:axis-concept :LegalEntityAxis,
                :axis-taxonomy :dei,
                :member-concept :UnitedAirLinesIncMember,
                :member-taxonomy :ual}],
  :entity {:cik "0000100517", :dimension #:dei{:LegalEntityAxis :ual/UnitedAirLinesIncMember}},
  :value 1000})
;; LegalEntitytAxis - no. 
;; ClassesOfShareCaptialAxis - yes
; CPA
#_({:dimensions [], :entity {:cik "0001345105"}, :value 41138553}
 {:dimensions [{:axis-concept :ClassesOfShareCapitalAxis,
                :axis-taxonomy :ifrs-full,
                :member-concept :ClassACommonStockMember,
                :member-taxonomy :cpa}],
  :entity {:cik "0001345105", :dimension #:ifrs-full{:ClassesOfShareCapitalAxis :cpa/ClassACommonStockMember}},
  :value 30200428}
 {:dimensions [{:axis-concept :ClassesOfShareCapitalAxis,
                :axis-taxonomy :ifrs-full,
                :member-concept :ClassBCommonStockMember,
                :member-taxonomy :cpa}],
  :entity {:cik "0001345105", :dimension #:ifrs-full{:ClassesOfShareCapitalAxis :cpa/ClassBCommonStockMember}},
  :value 10938125})

;; run from unit test data folder

(-> "test/html/TDAC.html"
    (xbrl-shares/load-filing-facts)
    (xbrl-shares/share-summary)
    shares-ui
    print-table)

(-> "test/html/TDAC.html"
    (xbrl-shares/load-filing-facts)
    (xbrl-shares/share-summary)
    :shares
    (xbrl-shares/shares-outstanding))
