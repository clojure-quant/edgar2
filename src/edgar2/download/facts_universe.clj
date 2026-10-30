(ns edgar2.download.facts-universe
  "Companyfacts extract joined to the primary listed ticker by CIK."
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [edgar.api :as e]
            [edgar.xbrl :as xbrl]
            [edgar2.financials.concepts :as concepts]
            [edgar2.financials.shares-outstanding :as shares]
            [edgar2.download.dataset :as dataset]
            [edgar2.download.facts :as dl]
            [edgar2.util.ticker-info :as ticker-info]
            [jsonista.core :as json])
  (:import [java.util.zip ZipFile]))

(def universe-path "data/universe-facts.edn")

(def revenue-tags
  (:data (concepts/get-concept-id :revenue)))

(defn pad-cik
  "10-digit CIK, or nil when blank / non-numeric."
  [cik]
  (ticker-info/pad-cik cik))

(defn unit-name
  "Keep USD/shares as USD/shares; jsonista turns that key into a namespaced keyword."
  [unit]
  (cond
    (string? unit) unit
    (and (keyword? unit) (namespace unit)) (str (namespace unit) "/" (name unit))
    (keyword? unit) (name unit)
    :else (str unit)))

(defn concept-observations
  "Fact rows with :unit taken from the companyfacts units key (USD, JPY, COP, USD/shares)."
  [facts taxonomy tag]
  (mapcat (fn [[unit obs]]
            (let [u (unit-name unit)]
              (map #(assoc % :unit u) obs)))
          (get-in facts [taxonomy tag :units])))

(defn most-recent-obs
  "Latest observation by :filed (then :end) across the given [taxonomy tag] pairs."
  [facts tag-pairs]
  (some->> tag-pairs
           (mapcat (fn [[tax tag]] (concept-observations facts tax tag)))
           seq
           (sort-by #(str (:filed %) "|" (:end %)))
           last))

(defn shares-outstanding-obs
  "Share count from annual and quarterly report tables.

  `dss` is a companyfacts dataset (`edgar.xbrl/facts->dataset`).
  Both frequencies run `report-table-full` over the
  `:shares-outstanding` tags, then `calc-concept` adds
  `:shares-outstanding`. The quarterly row is used when it was filed
  after the annual row, unless it is more than 100 times smaller.
  Includes `:val`, `:end`, `:filed`, `:period`,
  and `:q-or-y`."
  [dss]
  (shares/shares-outstanding-obs dss))

(defn shares-outstanding
  "Shares outstanding for `dss`, a companyfacts dataset.

  See `shares-outstanding-obs`."
  [dss]
  (:val (shares-outstanding-obs dss)))

(defn reporting-standard
  "us-gaap, ifrs-full, and/or ffd (filing-fee disclosure)."
  [facts]
  (let [stds (->> [:us-gaap :ifrs-full :ffd]
                  (filter #(contains? facts %))
                  (map name))]
    (when (seq stds)
      (str/join "," stds))))

(defn compact-company-facts
  [data]
  (let [facts (:facts data)
        dss (try (xbrl/facts->dataset data) (catch Exception _ nil))
        rev (most-recent-obs facts revenue-tags)]
    (cond-> {:cik (when (:cik data) (pad-cik (:cik data)))
             :entityName (:entityName data)
             :shares-outstanding (when dss (shares-outstanding dss))
             :revenue (:val rev)
             :reporting-standard (reporting-standard facts)}
      (:unit rev) (assoc :report-currency (:unit rev))
      (:form rev) (assoc :revenue-form (:form rev))
      (:fp rev) (assoc :revenue-fp (:fp rev))
      (contains? facts :ffd) (assoc :note "filing-fee disclosure"))))

(defn with-listing
  [row]
  (if-let [listing (ticker-info/primary-ticker-for-cik (:cik row))]
    (assoc (merge row listing)
           :tickers (ticker-info/tickers-for-cik (:cik row)))
    row))

(defn empty-facts?
  "True when companyfacts.json had no taxonomies (facts: {})."
  [{:keys [reporting-standard revenue shares-outstanding note]}]
  (and (nil? reporting-standard)
       (nil? revenue)
       (nil? shares-outstanding)
       (nil? note)))

(defn keep-fact?
  "Drop ETF / Trust ETF names, filing-fee-only rows with no revenue,
  and CIKs whose companyfacts file is empty."
  [{:keys [entityName revenue note] :as row}]
  (not (or (empty-facts? row)
           (and (nil? revenue) (ticker-info/etf-name? entityName))
           (and (nil? revenue) (= note "filing-fee disclosure")))))

(defn zip-json-names
  [zip-path]
  (with-open [zf (ZipFile. (str zip-path))]
    (->> (.entries zf)
         enumeration-seq
         (map #(.getName %))
         (filter #(and (str/ends-with? % ".json")
                       (not (str/ends-with? % "/"))))
         vec)))

(defn parse-companyfacts-zip
  [zip-path limit]
  (let [names (cond->> (zip-json-names zip-path)
                limit (take (long limit))
                true vec)
        total (count names)]
    (println (format "Parsing %d JSON files from %s" total zip-path))
    (flush)
    (with-open [zf (ZipFile. (str zip-path))]
      (loop [i 0
             acc (transient [])
             names names]
        (if-let [name (first names)]
          (let [entry (.getEntry zf name)
                row (try
                      (with-open [in (.getInputStream zf entry)]
                        (compact-company-facts
                         (json/read-value in json/keyword-keys-object-mapper)))
                      (catch Exception ex
                        {:cik name :error (.getMessage ex)}))
                i' (inc i)]
            (when (or (zero? (mod i' 500)) (= i' total))
              (println (format "  parsed %d/%d  %s"
                               i' total
                               (or (:entityName row) (:cik row) name)))
              (flush))
            (recur i' (conj! acc row) (next names)))
          (vec (persistent! acc)))))))

(defn save-universe!
  "Write data/universe-facts.edn. :download-date is the companyfacts zip day."
  ([rows] (save-universe! rows (dl/companyfacts-date)))
  ([rows download-date]
   (dataset/write-dataset! universe-path
                           (cond-> {}
                             download-date (assoc :download-date download-date))
                           rows)))

(defn universe
  "Download companyfacts.zip (with progress), join the primary listed
  ticker and exchange by CIK, and write data/universe-facts.edn.
  :tickers is every listed symbol for that CIK. Unlisted CIKs are omitted.

  Reuses a zip already on disk. Pass :force true to download again.
  :limit N parses only the first N JSON files (zip is still the full archive).

  Usage: clj -X:universe-facts
         clj -X:universe-facts :force true
         clj -X:universe-facts :limit 20"
  ([] (universe {}))
  ([{:keys [force limit]}]
   (e/init! dl/identity-header)
   (let [zip-path (dl/ensure-zip! {:force force})
         parsed (parse-companyfacts-zip zip-path limit)
         kept (filter keep-fact? parsed)
         dropped (- (count parsed) (count kept))
         listed (->> kept
                     (map with-listing)
                     (filter #(not (str/blank? (str (:ticker %)))))
                     (sort-by :cik)
                     vec)
         unlisted (- (count kept) (count listed))
         with-shares (count (filter :shares-outstanding listed))
         with-rev (count (filter :revenue listed))
         by-unit (frequencies (keep :report-currency listed))
         by-form (frequencies (keep :revenue-form listed))]
     (save-universe! listed)
     (println)
     (pprint/print-table
      [:ticker :exchange :cik :entityName :revenue :report-currency :revenue-form]
      (take 12 listed))
     (println (format "Wrote %s  (%d listed; dropped %d empty/ETF/filing-fee, %d unlisted; shares=%d  revenue=%d)"
                      universe-path (count listed) dropped unlisted with-shares with-rev))
     (println "report-currency" (sort-by val > by-unit))
     (println "revenue-form" (sort-by val > by-form))
     listed)))
