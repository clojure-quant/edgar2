(ns demo.xbrl-shares-one
  (:require
   [edgar2.filing.xbrl.xbrl-shares :as xbrl-shares]
   [clojure.pprint :refer [print-table]]))

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

;; live test for one ticker

(-> "ABT"
    xbrl-shares/share-summary-ticker
    shares-ui
    print-table)

(-> "ABT"
    xbrl-shares/share-summary-ticker
    ticker-ui
    print-table)

;; store for unit test

(defn store-test [ticker]
  (let [url (xbrl-shares/ticker->url-file ticker)
        content (slurp url)]
    (spit (str "test/html/" ticker ".html") content)))

(store-test "TDAC")

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
