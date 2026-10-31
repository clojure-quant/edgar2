(ns edgar2.db.shares
  "Share-class totals from data/share-classes.edn."
  (:require [clojure.string :as str]
            [edgar2.download.dataset :as dataset]
            [edgar2.util.ticker-info :as ticker-info]))

(def share-classes-path
  "data/share-classes.edn")

(defn- ticker-key
  [ticker]
  (some-> ticker str str/trim not-empty str/upper-case))

(defn- sum-key
  "Sum of `k` across share classes. A nil value counts as 0."
  [classes k]
  (transduce (map #(or (get % k) 0)) + 0 classes))

(defn- share-info
  "Totals across every share class, plus the filing identity.
  {:cik :filing-date :form-type :weighted :outstanding}.
  A nil class value counts as 0."
  [row]
  (let [classes (vals (:shares row))]
    {:cik (:cik row)
     :filing-date (:filing-date row)
     :form-type (:form-type row)
     :weighted (sum-key classes :weighted)
     :outstanding (sum-key classes :outstanding)}))

(defn- index-rows
  "CIK and ticker → share totals, plus the set of tickers in the file."
  [rows]
  {:by-cik (into {}
                 (keep (fn [{:keys [cik] :as row}]
                         (when-let [code (ticker-info/pad-cik cik)]
                           [code (share-info row)])))
                 rows)
   :by-ticker (into {}
                    (keep (fn [{:keys [ticker] :as row}]
                            (when-let [t (ticker-key ticker)]
                              [t (share-info row)])))
                    rows)
   :tickers (into #{}
                  (keep #(ticker-key (:ticker %)))
                  rows)})

(defn load-shares-index
  "Indexes from data/share-classes.edn. Throws when the file is missing."
  []
  (let [rows (or (dataset/read-rows share-classes-path)
                 (throw (ex-info "Missing data/share-classes.edn"
                                 {:path share-classes-path})))]
    (index-rows rows)))

(def shares-index
  "Loaded on first use. Tests rebind this with `with-redefs`."
  (delay (load-shares-index)))

(defn get-shares-cik
  "Share totals for `cik`, or nil when the CIK is absent.
  Includes :cik, :filing-date, and :form-type from the source row.
  :weighted and :outstanding sum every share class; nil counts as 0.
  The file is read once and kept for the process."
  [cik]
  (when-let [code (ticker-info/pad-cik cik)]
    (get (:by-cik @shares-index) code)))

(defn get-shares-ticker
  "Share totals for `ticker`, or nil when the ticker is absent.
  Includes :cik, :filing-date, and :form-type from the source row.
  :weighted and :outstanding sum every share class; nil counts as 0.
  The file is read once and kept for the process."
  [ticker]
  (when-let [t (ticker-key ticker)]
    (get (:by-ticker @shares-index) t)))

(defn share-tickers
  "Every ticker in data/share-classes.edn.
  The file is read once and kept for the process."
  []
  (:tickers @shares-index))
