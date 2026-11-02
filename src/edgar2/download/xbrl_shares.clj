(ns edgar2.download.xbrl-shares
  "Share counts from a companyfacts file, or from the latest annual filing.

  clj -X:xbrl-shares
  clj -X:xbrl-shares :ticker '\"SONY\"'
  clj -X:xbrl-shares :file '\"test/facts/COKE.json\"'"
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [edgar2.db.universe :as universe]
            [edgar2.download.dataset :as dataset]
            [edgar2.filing.xbrl.xbrl-shares-v1 :as shares-v1]
            [edgar2.util :as util]
            [edgar2.util.core :as ucore]
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

(defn- ticker-name
  [ticker]
  (when (some? ticker)
    (let [s (str/upper-case
             (cond
               (string? ticker) ticker
               (keyword? ticker) (name ticker)
               :else (str ticker)))]
      (when-not (str/blank? s) s))))

(defn- shares-known?
  "True when a share summary carries a positive count in any class."
  [summary]
  (boolean (some (fn [class] (some #(and (number? %) (pos? %)) (vals class)))
                 (vals summary))))

(defn share-summary-cik
  "Latest annual filing for `cik`, parsed with `edgar2.filing.xbrl/parse-all`.

  The filing is the newest 10-K, 20-F, or 40-F of the newest report period.
  An amendment that furnishes XBRL is tried first, because a filer can
  refile an untagged annual report solely to add Inline XBRL; when that
  amendment carries no usable share count (a stub `10-K/A` with only
  placeholders), the original filing of the same period is tried next.
  When the cover links an audited-statement exhibit (`_d2`, `ex99d2`, or an
  `ex99-2` variant), that HTML is parsed with the cover so its facts can
  use the cover's contexts. The extracted instance (`form40f_htm.xml`) is
  included when SEC has one, and a pre-inline filing's EX-101 instance from
  the index when it does not, so a cover fact can use a context that the
  inline HTML left out.
  Each share class also has the cover `:ticker` and `:exchange`. A listed
  note or bond is not a share class and is left out. When subsidiaries are
  co-registrants (`dei:LegalEntityAxis`), only the parent's share counts are
  used and `:entities` holds how many entities report.
  Returns `{:cik :filing-date :form-type :shares}`, plus `:entities` when
  the filing carries an entity axis, and `:xbrl false` when the filing has
  no XBRL at all, the way a 40-F with an untagged cover does."
  [cik]
  (ucore/ensure-identity!)
  (let [candidates (ufiling/annual-filing-candidates cik)]
    (when-not (seq candidates)
      (throw (ex-info "no annual filing" {:cik cik})))
    (loop [[filing & more] candidates]
      (let [result (shares-v1/filing-share-summary cik filing)]
        (if (and (not (shares-known? (:shares result))) (seq more))
          (recur more)
          result)))))

(defn share-summary-ticker
  "CIK for `ticker`, then `share-summary-cik`, with `:ticker` added."
  [ticker]
  (ucore/ensure-identity!)
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
    (pprint/pprint (shares-v1/share-summary facts))
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
  (ucore/ensure-identity!)
  (ucore/enable-cache)
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