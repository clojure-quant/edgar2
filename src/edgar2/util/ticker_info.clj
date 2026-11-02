(ns edgar2.util.ticker-info
  "Static ticker and company-name helpers: CIK padding, categories, and ETFs."
  (:require [clojure.string :as str]))

(defn pad-cik
  "10-digit CIK string, or nil when `cik` is blank / non-numeric."
  [cik]
  (let [s (str/trim (str cik))]
    (when (re-matches #"\d+" s)
      (format "%010d" (Long/parseLong s)))))

(def ^:private category-words
  "Fund, Trust, and Preferred. The rightmost match in the name wins, so
  \"Preferred Income Fund\" is a Fund and \"Funds Trust\" is a Trust."
  [["Fund" #"(?i)\bFunds?\b"]
   ["Trust" #"(?i)\bTrust\b"]
   ["Preferred" #"(?i)\bPreferred\b"]])

(defn name-category
  "ADR, ETF, Trust, Fund, or Preferred from the SEC registrant name.
  Nil when the name says none of those.

  `/ADR` (also `/ ADR` and `/ADR/`) is an ADR even when the issuer is a trust.
  The word ETF is an ETF, including \"Trust ETF\" and \"ETF Trust\"."
  [name]
  (let [s (str name)]
    (cond
      (re-find #"(?i)/\s*ADR\b" s) "ADR"
      (re-find #"(?i)\bETF\b" s) "ETF"
      :else
      (->> category-words
           (keep (fn [[label re]]
                   (let [m (re-matcher re s)]
                     (when (.find m)
                       [(.start m) label]))))
           (sort-by first)
           last
           second))))

(defn common-ticker?
  "False for NYSE class / preferred / unit / warrant tickers (BRK-A, SCE-PG, KCA-UN)."
  [row]
  (not (str/includes? (str (:ticker row)) "-")))

(defn filter-primary-filers
  "Keep the main ticker for each CIK: first row in SEC listing order."
  [filers]
  (->> filers
       (group-by :cik)
       vals
       (map first)
       (sort-by :ticker)))

(defn etf-name?
  "True when the entity name is an ETF / ETF Trust / Trust ETF (word-boundary)."
  [name]
  (let [s (str/trim (str name))]
    (boolean
     (or (re-find #"(?i)\bETF\s+Trust\b" s)
         (re-find #"(?i)\bTrust\s+ETF\b" s)
         (re-find #"(?i)\bExchange[\s-]+Traded\s+(?:Fund|Trust)\b" s)
         (re-find #"(?i)\bETFS\b" s)
         (re-find #"(?i)\bETF\s*(?:II|III)?\s*[.,]?\s*$" s)))))
