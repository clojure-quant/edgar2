(ns demo.xbrl-shares-one
  (:require
   [edgar2.filing.xbrl.xbrl-shares :as xbrl-shares]
   [clojure.pprint :refer [print-table]]))


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

