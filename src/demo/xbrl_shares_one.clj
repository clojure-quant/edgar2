(ns demo.xbrl-shares-one
  (:require
   [edgar2.filing.xbrl.xbrl-shares :as xbrl-shares]
   [clojure.pprint :refer [print-table]]))

(defn shares-ui [summary]
  (->> summary
       :shares
       (map (fn [[k v]]
              (assoc v :class k)))
       (map #(select-keys % [:class :outstanding :weighted :diluted
                             ]))
       (into [])))

(defn ticker-ui [summary]
  (->> summary
       :tickers
       (map #(select-keys % [:ticker :exchange :title :class]))
       (into [])))


(-> "AAPL" ; 10-K with 1 class, lots of notes
    xbrl-shares/share-summary-ticker
    shares-ui
    print-table)

(-> "AAPL" ; 10-K stock with 2 listed classes and lots of notes
    xbrl-shares/share-summary-ticker
    ticker-ui
    print-table)

(-> "CPA" ; 20-F filer
    xbrl-shares/share-summary-ticker
    shares-ui
    print-table)

(-> "COKE" ; 10-K stock with 2 classes, 1 listed
    xbrl-shares/share-summary-ticker
    shares-ui
    print-table)

(-> "COKE" ; 10-K stock with 2 listed classes and lots of notes
    xbrl-shares/share-summary-ticker
    ticker-ui
    print-table)


(-> "BRK-B" ; 10-K stock with 2 listed classes
    xbrl-shares/share-summary-ticker
    shares-ui
    print-table)

(-> "BRK-B" ; 10-K stock with 2 listed classes and lots of notes
    xbrl-shares/share-summary-ticker
    ticker-ui
    print-table)

(-> "test/html/COKE.html"
    (xbrl-shares/load-filing-facts)
    (xbrl-shares/share-summary)
    ;ticker-ui
    ;print-table
    )