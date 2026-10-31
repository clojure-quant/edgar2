(ns edgar2.util.filing
  "Shared filing helpers."
  (:require [clojure.string :as str]
            [edgar.api :as e]))

(def filing-dir
  "Downloaded filing documents."
  "data/filings")

(def ^:private annual-report-forms
  #{"10-K" "20-F" "40-F"})

(defn pick-latest-annual
  "First 10-K, 20-F, or 40-F in `filings`. `e/filings` is newest-first,
  so that row is the latest annual report across those forms."
  [filings]
  (first (filter #(annual-report-forms (:form %)) filings)))

(defn latest-annual-filing
  "Newest 10-K, 20-F, or 40-F for `cik`. A later filing date wins across
  forms. Amendments are excluded."
  [cik]
  (pick-latest-annual (e/filings cik)))

(defn parse-period
  ":annual or :quarterly. Missing or unknown values are annual."
  [period]
  (let [s (str/lower-case
           (cond
             (nil? period) "annual"
             (keyword? period) (name period)
             :else (str period)))]
    (if (#{"quarterly" "q"} s) :quarterly :annual)))
