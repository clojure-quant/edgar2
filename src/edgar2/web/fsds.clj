(ns edgar2.web.fsds
  "Ticker lookup for data/universe-fsds.edn (SIC and sector)."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [edgar2.util.ticker-info :as ticker-info]))

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

(defn company
  "FSDS row for `ticker`, or nil. The row is stored by CIK."
  [ticker]
  (when-let [cik (ticker-info/ticker->cik ticker)]
    (get @by-cik cik)))
