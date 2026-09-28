(ns edgar2.download.ticker-exchange
  "SEC company_tickers_exchange.json → ticker / CIK / name / exchange."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [edgar.api :as e]
            [edgar.core :as edgar]
            [edgar2.download.tickers :as tickers]))

(def identity-header
  (or (System/getenv "EDGAR_IDENTITY")
      "clojure-quant edgar2 research@clojure-quant.org"))

(def exchange-url
  "https://www.sec.gov/files/company_tickers_exchange.json")

(def exchanges-path "data/ticker-exchange.edn")

(defn fetch-exchange-rows
  "SEC company_tickers_exchange.json → seq of {:ticker :cik :name :exchange}."
  []
  (let [payload (edgar/edgar-get exchange-url)
        fields (mapv keyword (:fields payload))]
    (->> (:data payload)
         (map (fn [row]
                (let [m (zipmap fields row)]
                  {:ticker (str/upper-case (str (:ticker m)))
                   :cik (tickers/pad-cik (:cik m))
                   :name (:name m)
                   :exchange (:exchange m)})))
         (sort-by :ticker)
         vec)))

(defn listed-exchange?
  [row]
  (not (str/blank? (str (:exchange row)))))

(defn common-ticker?
  "False for NYSE class / preferred / unit / warrant tickers (BRK-A, SCE-PG, KCA-UN)."
  [row]
  (not (str/includes? (str (:ticker row)) "-")))

(defn select-exchange-rows
  "One main unhyphenated ticker per CIK, dropping rows with no listing venue."
  [rows]
  (->> rows
       tickers/filter-primary-filers
       (filter listed-exchange?)
       ;(filter common-ticker?)
       vec))

(defn fetch-exchanges
  "ticker → exchange from company_tickers_exchange.json (one request)."
  []
  (into {} (map (juxt :ticker :exchange)
                (select-exchange-rows (fetch-exchange-rows)))))

(defn save-exchanges!
  [rows]
  (.mkdirs (io/file "data"))
  (let [tmp (str exchanges-path ".tmp")]
    (spit tmp (with-out-str (pprint/pprint (vec rows))))
    (.renameTo (io/file tmp) (io/file exchanges-path))))

(defn load-exchanges
  "Rows from data/ticker-exchange.edn, or a fresh SEC download if missing."
  []
  (if (.exists (io/file exchanges-path))
    (edn/read-string (slurp exchanges-path))
    (vec (select-exchange-rows (fetch-exchange-rows)))))

(defn download-exchanges
  "Download company_tickers_exchange.json and write data/ticker-exchange.edn
  (one main ticker per CIK; skip hyphenated class/preferred/units and no exchange).

  Usage: clj -X:download-exchanges"
  ([] (download-exchanges {}))
  ([_]
   (e/init! identity-header)
   (let [all (fetch-exchange-rows)
         rows (select-exchange-rows all)
         by-ex (frequencies (map :exchange rows))]
     (save-exchanges! rows)
     (println (format "SEC ticker exchanges: %d symbols → %d listed primary (from %s)"
                      (count all) (count rows) exchange-url))
     (println "venues" (sort-by val > by-ex))
     (println (format "Wrote %s" exchanges-path))
     (println)
     (pprint/print-table [:ticker :cik :name :exchange] (take 25 rows))
     (println (format "... %d more (see %s)"
                      (max 0 (- (count rows) 25)) exchanges-path))
     rows)))
