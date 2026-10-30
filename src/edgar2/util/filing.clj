(ns edgar2.util.filing
  "Shared filing helpers."
  (:require [clojure.string :as str]
            [edgar.api :as e]))

(def filing-dir
  "Downloaded filing documents."
  "data/filings")

(def ^:private annual-form-names
  ["10-K" "20-F" "40-F"])

(defn annual-form
  "10-K, or 20-F / 40-F when that is the annual report."
  [cik]
  (or (some (fn [form]
              (when (seq (e/filings cik :form form :limit 1))
                form))
            annual-form-names)
      "10-K"))

(defn parse-period
  ":annual or :quarterly. Missing or unknown values are annual."
  [period]
  (let [s (str/lower-case
           (cond
             (nil? period) "annual"
             (keyword? period) (name period)
             :else (str period)))]
    (if (#{"quarterly" "q"} s) :quarterly :annual)))
