(ns edgar2.util.ticker-info
  "Listed-ticker helpers shared by the exchange file and the facts universe."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [edgar2.download.dataset :as dataset]))

(def exchange-rows-path "data/ticker-exchange.edn")

(defn pad-cik
  [cik]
  (format "%010d" (Long/parseLong (str cik))))

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

(defn- read-exchange-rows
  []
  (let [f (io/file exchange-rows-path)]
    (if (.exists f)
      (dataset/read-rows f)
      [])))

(def ^:private exchange-rows
  "Every listed row. Loaded on first use."
  (delay (vec (read-exchange-rows))))

(def ^:private rows-by-cik
  (delay (group-by :cik @exchange-rows)))

(def ^:private cik-by-ticker
  (delay
    (into {}
          (keep (fn [{:keys [ticker cik]}]
                  (when (and (not (str/blank? (str ticker)))
                             (not (str/blank? (str cik))))
                    [(str/upper-case (str ticker))
                     (pad-cik cik)])))
          @exchange-rows)))

(defn ticker->cik
  "10-digit CIK for a listed ticker, or nil."
  [ticker]
  (when-let [t (some-> ticker str str/trim not-empty str/upper-case)]
    (get @cik-by-ticker t)))

(defn- listed-rows
  "Exchange rows for `cik` in SEC file order (primary first)."
  [cik]
  (->> (get @rows-by-cik (pad-cik cik))
       (filterv #(not (str/blank? (str (:ticker %)))))))

(defn tickers-for-cik
  "Every listed ticker for `cik`, primary first (SEC order). Empty when absent."
  [cik]
  (vec (distinct (map :ticker (listed-rows cik)))))

(defn primary-ticker-for-cik
  "Primary listed ticker and exchange for `cik`, or nil.
  Primary is the first symbol for that CIK in company_tickers_exchange.json
  (same choice `filter-primary-filers` makes)."
  [cik]
  (when-let [row (first (listed-rows cik))]
    {:ticker (:ticker row)
     :exchange (:exchange row)}))
