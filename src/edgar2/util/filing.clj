(ns edgar2.util.filing
  "Shared filing helpers."
  (:require [clojure.string :as str]
            [edgar.api :as e]))

(def filing-dir
  "Downloaded filing documents."
  "data/filings")

(def ^:private annual-report-forms
  #{"10-K" "20-F" "40-F"})

(defn- annual-form?
  "True for an annual report or its amendment. A filer refiles an untagged
  10-K as `10-K/A` solely to furnish Inline XBRL."
  [form]
  (contains? annual-report-forms
             (str/upper-case (str/replace (str form) #"/A$" ""))))

(defn- xbrl?
  "True when the submission is flagged as carrying XBRL."
  [filing]
  (boolean (some #(and (number? %) (pos? %))
                 [(:isXBRL filing) (:isInlineXBRL filing)])))

(defn pick-annual-filings
  "Annual reports in preference order: the newest report period, with an
  XBRL-bearing amendment before the original.

  A filer that first files an untagged annual report, then refiles it as
  `10-K/A` (or 20-F/A, 40-F/A) solely to furnish Inline XBRL, keeps the
  share count only in that amendment. The original follows as a fallback
  for a stub amendment that carries no usable count. An amendment of an
  older period never displaces a newer report."
  [filings]
  (let [annual (vec (filter #(annual-form? (:form %)) filings))]
    (when (seq annual)
      (if-let [period (->> annual (keep :reportDate) sort last)]
        (let [same (filter #(= period (:reportDate %)) annual)]
          (concat (filter xbrl? same)
                  (remove xbrl? same)))
        [(first annual)]))))

(defn pick-latest-annual
  "First of `pick-annual-filings`: the newest 10-K, 20-F, or 40-F, with an
  XBRL-bearing amendment of that period ahead of the original."
  [filings]
  (first (pick-annual-filings filings)))

(defn annual-filing-candidates
  "Annual filings for `cik` in preference order. See `pick-annual-filings`."
  [cik]
  (pick-annual-filings (e/filings cik :include-amends? true)))

(defn latest-annual-filing
  "Newest 10-K, 20-F, or 40-F for `cik`, including amendments. A later
  filing date wins across forms; see `pick-annual-filings` for the
  amendment rule."
  [cik]
  (first (annual-filing-candidates cik)))

(defn parse-period
  ":annual or :quarterly. Missing or unknown values are annual."
  [period]
  (let [s (str/lower-case
           (cond
             (nil? period) "annual"
             (keyword? period) (name period)
             :else (str period)))]
    (if (#{"quarterly" "q"} s) :quarterly :annual)))
