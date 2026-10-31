(ns edgar2.download.xbrl-shares
  "Share counts from a companyfacts file, or from the latest annual filing.

  clj -X:xbrl-shares
  clj -X:xbrl-shares :ticker '\"SONY\"'
  clj -X:xbrl-shares :file '\"test/facts/COKE.json\"'"
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [edgar.api :as e]
            [edgar2.db.universe :as universe]
            [edgar2.download.dataset :as dataset]
            [edgar2.filing.xbrl :as xbrl]
            [edgar2.util :as util]
            [edgar2.util.filing :as ufiling]
            [edgar2.util.map-progress :refer [map-progress]]
            [jsonista.core :as json])
  (:import [java.time LocalDate]))

(def default-file "test/facts/COKE.json")

(defn load-facts
  "Companyfacts map from a JSON file on disk."
  [path]
  (with-open [in (io/input-stream (io/file (str path)))]
    (json/read-value in json/keyword-keys-object-mapper)))

(def ^:private class-axis
  :us-gaap:StatementClassOfStockAxis)

(defn- map-get
  "Value for `k` when map keys may be keywords or strings.

  A sorted map throws if probed with the other key type, which is how
  `edgar2.filing.xbrl/parse` stores units and dimensions."
  [m k]
  (when m
    (let [s (if (keyword? k) (name k) (str k))]
      (some (fn [[mk v]]
              (when (= s (if (keyword? mk) (name mk) (str mk)))
                v))
            m))))

(defn- class-member
  "Value of the class-of-stock axis, or nil when the row has no class."
  [row]
  (map-get (:dimensions row) class-axis))

(defn- class-label
  "\"Class B\" for a ClassB member. No axis, or any other member, is \"Common\"."
  [member]
  (let [local (let [s (str member)
                    i (str/last-index-of s ":")]
                (if i (subs s (inc i)) s))]
    (if-let [[_ letter] (re-find #"Class([A-Z])Member$" local)]
      (str "Class " letter)
      "Common")))

(defn- share-rows
  [concept]
  (or (map-get (:units concept) :shares)
      []))

(defn- latest-val
  "Value on the greatest :end. A later :filed breaks a tie."
  [rows]
  (when (seq rows)
    (:val (last (sort-by (juxt #(or (:end %) "") #(or (:filed %) "")) rows)))))

(defn- rows-by-class
  [concept]
  (group-by #(class-label (class-member %)) (share-rows concept)))

(defn- weighted-concept
  "Basic weighted-average shares.

  US-GAAP outstanding, then US-GAAP issued when the outstanding tag is
  absent, then IFRS weighted-average shares."
  [facts]
  (or (get-in facts [:facts :us-gaap :WeightedAverageNumberOfSharesOutstandingBasic])
      (get-in facts [:facts :us-gaap :WeightedAverageNumberOfSharesIssuedBasic])
      (get-in facts [:facts :ifrs-full :WeightedAverageShares])))

(defn- outstanding-concept
  "Shares outstanding.

  `dei:EntityCommonStockSharesOutstanding` when it has a share fact,
  otherwise `us-gaap:SharesOutstanding`. An empty DEI concept does not
  block the fallback."
  [facts]
  (let [dei (get-in facts [:facts :dei :EntityCommonStockSharesOutstanding])]
    (if (seq (share-rows dei))
      dei
      (get-in facts [:facts :us-gaap :SharesOutstanding]))))

(defn share-summary
  "Latest weighted-average and shares outstanding by common-stock class.

  `facts` is a companyfacts map. Outstanding shares are
  `:dei/EntityCommonStockSharesOutstanding`, then
  `:us-gaap/SharesOutstanding` when that tag has no share facts.
  Weighted shares are
  `:us-gaap/WeightedAverageNumberOfSharesOutstandingBasic`, then
  `:us-gaap/WeightedAverageNumberOfSharesIssuedBasic` when that tag is
  absent, then `:ifrs-full/WeightedAverageShares`.
  A row with no `:us-gaap:StatementClassOfStockAxis` is \"Common\". A
  ClassA or ClassB member on that axis is \"Class A\" or \"Class B\"; any
  other member is \"Common\". Each figure is the observation with the
  greatest `:end`."
  [facts]
  (let [outstanding (rows-by-class (outstanding-concept facts))
        weighted (rows-by-class (weighted-concept facts))
        classes (->> (concat (keys outstanding) (keys weighted))
                     distinct
                     (sort-by #(if (= % "Common") " " %)))]
    (into {}
          (map (fn [class]
                 [class {:weighted (latest-val (get weighted class))
                         :outstanding (latest-val (get outstanding class))}]))
          classes)))

(defn- ticker-name
  [ticker]
  (when (some? ticker)
    (let [s (str/upper-case
             (cond
               (string? ticker) ticker
               (keyword? ticker) (name ticker)
               :else (str ticker)))]
      (when-not (str/blank? s) s))))

(defn- instance-name
  "Extracted XBRL instance for a primary document.
  `form40f.htm` is `form40f_htm.xml`."
  [primary]
  (when-let [[_ stem] (re-find #"(?i)^(.+)\.html?$" (str primary))]
    (str stem "_htm.xml")))

(defn- instance-text
  "Extracted instance for `filing`, or nil when SEC has no such document.
  Its contexts and units resolve cover facts that were left without them."
  [filing]
  (when-let [name (instance-name (:primaryDocument filing))]
    (try
      (e/filing-document filing name)
      (catch clojure.lang.ExceptionInfo e
        (if (= 404 (:status (ex-data e)))
          nil
          (throw e))))))

(defn share-summary-cik
  "Latest annual filing for `cik`, parsed with `edgar2.filing.xbrl/parse-all`.

  The filing is the newest 10-K, 20-F, or 40-F. When the cover links an
  audited-statement exhibit (`_d2` or `ex99d2`), that HTML is parsed with
  the cover so its facts can use the cover's contexts. The extracted
  instance (`form40f_htm.xml`) is included when SEC has one, so a cover
  fact can use a context that the inline HTML left out.
  Returns `{:cik cik :shares <share-summary>}`."
  [cik]
  (util/ensure-identity!)
  (let [filing (ufiling/latest-annual-filing cik)]
    (when-not filing
      (throw (ex-info "no annual filing" {:cik cik})))
    (let [html (e/html filing)
          exhibit (xbrl/statement-exhibit-name html)
          texts (->> [html
                      (when exhibit (e/filing-document filing exhibit))
                      (instance-text filing)]
                     (keep identity)
                     vec)
          parsed (xbrl/parse-all texts {:accn (:accessionNumber filing)
                                        :filed (:filingDate filing)
                                        :form (:form filing)})]
      {:cik cik
       :shares (share-summary parsed)})))

(defn share-summary-ticker
  "CIK for `ticker`, then `share-summary-cik`, with `:ticker` added."
  [ticker]
  (util/ensure-identity!)
  (let [ticker (ticker-name ticker)
        cik (util/resolve-filer2 ticker)]
    (when-not cik
      (throw (ex-info "no CIK for ticker" {:ticker ticker})))
    (assoc (share-summary-cik cik) :ticker ticker)))

(defn- print-concept
  [facts taxonomy concept]
  (println)
  (println (str (name taxonomy) ":" (name concept)))
  (pprint/pprint (get-in facts [:facts taxonomy concept])))

(defn- run-file
  [file]
  (let [path (or file default-file)
        facts (load-facts path)]
    (println (:entityName facts) " cik " (:cik facts))
    (println "source " path)
    (print-concept facts :dei :EntityCommonStockSharesOutstanding)
    (print-concept facts :us-gaap :WeightedAverageNumberOfSharesOutstandingBasic)
    (println)
    (println "share-summary")
    (pprint/pprint (share-summary facts))
    nil))

(def share-classes-path
  "Written by `run` when no ticker is passed."
  "data/share-classes.edn")

(defn- summary-for-universe-row
  "Share summary for one universe-facts row. A failure is a row with :error."
  [{:keys [ticker cik]}]
  (try
    (assoc (share-summary-cik cik) :ticker ticker)
    (catch Throwable ex
      (let [msg (or (ex-message ex) (str ex))]
        (println (format "  skip %s %s (%s)" ticker cik msg))
        (flush)
        {:ticker ticker :cik cik :error msg}))))

(defn run-universe
  "Share summary for every ticker in data/universe-facts.edn.

  Progress prints every 100 tickers. The file is written after the last one."
  []
  (util/ensure-identity!)
  (e/enable-disk-cache! :dir "data/edgar-cache")
  (let [rows (->> (universe/load-universe)
                  (filter #(not (str/blank? (str (:ticker %)))))
                  vec)
        n (count rows)]
    (println (format "share classes  %d tickers  → %s" n share-classes-path))
    (flush)
    (let [out (map-progress
               (fn [i total result]
                 (format "  %d/%d  %s" i total (:ticker result)))
               summary-for-universe-row
               rows)]
      (dataset/write-dataset! share-classes-path
                              {:download-date (str (LocalDate/now))}
                              out)
      (println (format "Wrote %s  (%d tickers)" share-classes-path (count out)))
      out)))

(defn run
  "With `:ticker`, print `share-summary-ticker`. With `:file`, load that
  companyfacts JSON. With neither, summarize every universe ticker into
  data/share-classes.edn."
  ([] (run {}))
  ([{:keys [file ticker]}]
   (cond
     (ticker-name ticker)
     (let [summary (share-summary-ticker ticker)]
       (pprint/pprint summary)
       summary)

     file
     (run-file file)

     :else
     (run-universe))))
