(ns edgar2.web.fsds
  "Ticker lookup for data/universe-fsds.edn (SIC and sector)."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [edgar2.download.tickers :as tickers]))

(def path "data/universe-fsds.edn")

(def by-cik
  (delay
    (if (.exists (io/file path))
      (into {}
            (keep (fn [row]
                    (when-let [cik (some-> (:cik row) str str/trim not-empty)]
                      [cik row])))
            (edn/read-string (slurp path)))
      {})))

(def cik-by-ticker
  (delay
    (into {}
          (keep (fn [{:keys [ticker cik]}]
                  (when (and ticker cik)
                    [(str/upper-case (str ticker)) (str cik)])))
          (tickers/load-tickers))))

(defn company
  "FSDS row for `ticker`, or nil. The row is stored by CIK."
  [ticker]
  (when-let [cik (get @cik-by-ticker (str/upper-case (str ticker)))]
    (get @by-cik cik)))
