(ns edgar2.db.mapping
  "CUSIP and ticker lookups from data/eodhd-mapping.edn."
  (:require [clojure.string :as str]
            [edgar2.download.dataset :as dataset]
            [edgar2.download.eodhd-mapping :as eodhd-mapping]))

(defn- present
  "Trimmed upper-case string, or nil when blank."
  [v]
  (some-> v str str/trim not-empty str/upper-case))

(defn- without-us
  "Drop a trailing .US. Other tickers are unchanged."
  [ticker]
  (when ticker
    (if (str/ends-with? ticker ".US")
      (subs ticker 0 (- (count ticker) 3))
      ticker)))

(defn- with-us
  "Ticker as stored in the mapping file. Append .US when it is missing."
  [ticker]
  (if (str/ends-with? ticker ".US")
    ticker
    (str ticker ".US")))

(defn- conj-cusip
  "Append `cusip` unless it is already in `cusips`. File order is kept."
  [cusips cusip]
  (let [cusips (or cusips [])]
    (if (some #(= cusip %) cusips)
      cusips
      (conj cusips cusip))))

(defn- build-indexes
  "`:by-cusip` is cusip → ticker (as stored, including .US).
  `:by-ticker` is ticker → vector of cusips, in file order, duplicates removed.
  Rows missing either key are skipped."
  [rows]
  (reduce (fn [idx {:keys [ticker cusip]}]
            (let [cusip (present cusip)
                  ticker (present ticker)]
              (if (and cusip ticker)
                (-> idx
                    (update :by-cusip assoc cusip ticker)
                    (update :by-ticker update ticker conj-cusip cusip))
                idx)))
          {:by-cusip {} :by-ticker {}}
          rows))

(def ^:private indexes
  "Both lookup maps. Built on the first cusip->ticker or ticker->cusip call."
  (delay
    (build-indexes (or (dataset/read-rows eodhd-mapping/mapping-path) []))))

(defn cusip->ticker
  "Ticker for `cusip`. A trailing .US is removed.
  Nil when the CUSIP is blank or not in data/eodhd-mapping.edn.
  The file is read once and kept for the process."
  [cusip]
  (when-let [c (present cusip)]
    (without-us (get (:by-cusip @indexes) c))))

(defn ticker->cusip
  "Every CUSIP for `ticker`, in mapping-file order. Duplicates are dropped.
  A ticker without a .US suffix is looked up as ticker.US.
  Empty vector when the ticker is not in data/eodhd-mapping.edn.
  Nil when `ticker` is blank.
  The file is read once and kept for the process."
  [ticker]
  (when-let [t (present ticker)]
    (vec (get (:by-ticker @indexes) (with-us t) []))))
