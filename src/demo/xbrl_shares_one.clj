(ns demo.xbrl-shares-one
  (:require
   [edgar2.filing.xbrl.xbrl-shares :as xbrl-shares]
   [clojure.pprint :refer [print-table]]))



(def msft (xbrl-shares/share-summary "MSFT"))
msft

(defn shares-ui [summary]
  (->> summary
       :shares
       (map (fn [[k v]]
              (assoc v :class k)))
       #_(map #(select-keys % [:class :ticker :exchange :title
                             ;:outstanding :weighted :diluted
                               ]))
       (into [])))

(-> "MSFT"
    xbrl-shares/share-summary
    shares-ui
    print-table)

(-> "CPA"
    xbrl-shares/share-summary
    shares-ui
    print-table)


(def cpa (xbrl-shares/share-summary "CPA"))
cpa
{:outstanding
 {"CommonStockTotal" 41138553, :cpa/ClassACommonStockMember 30200428, :cpa/ClassBCommonStockMember 10938125}}

(def periods
  [{:start "2025-07-01", :end "2026-06-30"}
   {:instant "2026-07-23"}
   {:start "2024-07-01", :end "2025-06-30"}
   {:start "2023-07-01", :end "2024-06-30"}])


