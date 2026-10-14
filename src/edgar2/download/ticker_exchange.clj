(ns edgar2.download.ticker-exchange
  "SEC company_tickers_exchange.json → ticker / CIK / name / exchange."
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [edgar.api :as e]
            [edgar.core :as edgar]
            [edgar2.download.dataset :as dataset]
            [edgar2.util.ticker-info :as ticker-info])
  (:import [java.time LocalDate]))

(def identity-header
  (or (System/getenv "EDGAR_IDENTITY")
      "clojure-quant edgar2 research@clojure-quant.org"))

(def exchange-url
  "https://www.sec.gov/files/company_tickers_exchange.json")

(def exchanges-path "data/ticker-exchange.edn")

(defn with-category
  [{:keys [ticker cik name exchange]}]
  {:ticker ticker
   :cik cik
   :name name
   :category (ticker-info/name-category name)
   :exchange exchange})

(defn fetch-exchange-rows
  "SEC company_tickers_exchange.json → seq of {:ticker :cik :name :category :exchange}."
  []
  (let [payload (edgar/edgar-get exchange-url)
        fields (mapv keyword (:fields payload))]
    (->> (:data payload)
         (map (fn [row]
                (let [m (zipmap fields row)]
                  (with-category
                    {:ticker (str/upper-case (str (:ticker m)))
                     :cik (ticker-info/pad-cik (:cik m))
                     :name (:name m)
                     :exchange (:exchange m)}))))
         (sort-by :ticker)
         vec)))

(defn listed-exchange?
  [row]
  (not (str/blank? (str (:exchange row)))))

(defn select-exchange-rows
  "Every listed ticker, including non-primary share classes.
  Rows with no listing venue are dropped."
  [rows]
  (filterv listed-exchange? rows))

(defn fetch-exchanges
  "ticker → exchange from company_tickers_exchange.json (one request)."
  []
  (into {} (map (juxt :ticker :exchange)
                (select-exchange-rows (fetch-exchange-rows)))))

(defn save-exchanges!
  "Write data/ticker-exchange.edn. :download-date defaults to today."
  ([rows] (save-exchanges! rows (str (LocalDate/now))))
  ([rows download-date]
   (dataset/write-dataset! exchanges-path
                           {:download-date download-date}
                           rows)))

(defn load-exchanges
  "Rows from data/ticker-exchange.edn, or a fresh SEC download if missing."
  []
  (if (.exists (io/file exchanges-path))
    (dataset/read-rows exchanges-path)
    (vec (select-exchange-rows (fetch-exchange-rows)))))

(defn download-exchanges
  "Download company_tickers_exchange.json and write data/ticker-exchange.edn.
  Every listed ticker is kept, including non-primary share classes.
  Rows with no exchange are skipped.

  Usage: clj -X:download-exchanges"
  ([] (download-exchanges {}))
  ([_]
   (e/init! identity-header)
   (let [all (fetch-exchange-rows)
         rows (select-exchange-rows all)
         by-ex (frequencies (map :exchange rows))]
     (save-exchanges! rows)
     (println (format "SEC ticker exchanges: %d symbols → %d listed (from %s)"
                      (count all) (count rows) exchange-url))
     (println "venues" (sort-by val > by-ex))
     (println "categories" (sort-by val > (frequencies (map :category rows))))
     (println (format "Wrote %s" exchanges-path))
     (println)
     (pprint/print-table [:ticker :cik :name :exchange] (take 25 rows))
     (println (format "... %d more (see %s)"
                      (max 0 (- (count rows) 25)) exchanges-path))
     rows)))
