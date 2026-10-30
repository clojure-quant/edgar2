(ns edgar2.download.facts-universe
  "Companyfacts extract joined to the primary listed ticker by CIK."
  (:require [clojure.pprint :as pprint]
            [clojure.string :as str]
            [edgar.api :as e]
            [edgar.xbrl :as xbrl]
            [edgar2.financials.concepts :as concepts]
            [edgar2.financials.facts-ds :as facts-ds]
            [edgar2.financials.facts-zip :as facts-zip]
            [edgar2.financials.shares-outstanding :as shares]
            [edgar2.download.dataset :as dataset]
            [edgar2.download.facts :as dl]
            [edgar2.util.map-progress :refer [map-progress]]
            [edgar2.util.ticker-info :as ticker-info]
            [tech.v3.dataset :as ds])
  (:import [java.time LocalDate]))

(def universe-path "data/universe-facts.edn")

(def revenue-tags
  (:data (concepts/get-concept-id :revenue)))

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

(def outdated-months
  "An annual report is outdated when it was filed more than this many
  months before the companyfacts zip date."
  15)

(defn companyfacts-as-of
  "Publication day of data/companyfacts.zip."
  []
  (if-let [s (dl/companyfacts-date)]
    (LocalDate/parse s)
    (throw (ex-info "Missing companyfacts date" {:path dl/companyfacts-date-path}))))

(defn latest-annual
  "Most recently filed 10-K, 20-F, 40-F, or amendment on `dss`, or nil.
  The row includes :filed-date as a LocalDate."
  [dss]
  (when (and dss (pos? (ds/row-count dss)))
    (reduce (fn [best row]
              (let [filed (when (facts-ds/annual-forms (str (:form row)))
                            (facts-ds/parse-date (:filed row)))]
                (cond
                  (nil? filed) best
                  (nil? best) (assoc row :filed-date filed)
                  (.isAfter ^LocalDate filed (:filed-date best)) (assoc row :filed-date filed)
                  :else best)))
            nil
            (ds/rows dss {:nil-missing? true}))))

(defn annual-outdated?
  "True when `filed` is strictly more than 15 months before `as-of`."
  [filed as-of]
  (boolean
   (and (instance? LocalDate filed)
        (instance? LocalDate as-of)
        (.isBefore ^LocalDate filed
                   (.minusMonths ^LocalDate as-of (long outdated-months))))))

(defn annual-filing
  "Latest annual filing on `dss`. :annual-outdated is false when `dss`
  has no 10-K, 20-F, or 40-F."
  [dss as-of]
  (if-let [annual (latest-annual dss)]
    {:date-last-filed (str (:filed-date annual))
     :form-last-filed (str (:form annual))
     :accn-last-filed (str (:accn annual))
     :annual-outdated (annual-outdated? (:filed-date annual) as-of)}
    {:date-last-filed nil
     :form-last-filed nil
     :accn-last-filed nil
     :annual-outdated false}))

(defn compact-company-facts
  [data as-of]
  (let [facts (:facts data)
        dss (try (xbrl/facts->dataset data) (catch Exception _ nil))
        rev (most-recent-obs facts revenue-tags)]
    (merge (cond-> {:cik (when (:cik data) (ticker-info/pad-cik (:cik data)))
                    :entityName (:entityName data)
                    :shares-outstanding (when dss (shares-outstanding dss))
                    :revenue (:val rev)
                    :reporting-standard (reporting-standard facts)}
             (:unit rev) (assoc :report-currency (:unit rev))
             (:form rev) (assoc :revenue-form (:form rev))
             (:fp rev) (assoc :revenue-fp (:fp rev))
             (contains? facts :ffd) (assoc :note "filing-fee disclosure"))
           (annual-filing dss as-of))))

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

(defn parse-companyfacts-zip
  "Companyfacts rows from data/companyfacts.zip.
  `limit` parses only the first N CIKs.

  Each row includes :date-last-filed, :form-last-filed, and
  :accn-last-filed from the latest 10-K, 20-F, or 40-F, and
  :annual-outdated when that filing is more than 15 months before
  the companyfacts zip date."
  [limit]
  (let [as-of (companyfacts-as-of)]
    (with-open [zf (facts-zip/open-facts-zip)]
      (let [ciks (cond->> (facts-zip/cik-list zf)
                   limit (take (long limit))
                   true vec)
            total (count ciks)]
        (println (format "Parsing %d JSON files from %s" total dl/companyfacts-zip-path))
        (flush)
        (map-progress
         (fn [i total row]
           (format "  parsed %d/%d  %s"
                   i total
                   (or (:entityName row) (:cik row))))
         (fn [cik]
           (try
             (compact-company-facts (facts-zip/read-cik-json zf cik) as-of)
             (catch Exception ex
               {:cik cik :error (.getMessage ex)})))
         ciks)))))

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
  :date-last-filed, :form-last-filed, and :accn-last-filed are the latest
  10-K, 20-F, or 40-F. :annual-outdated is true when that report was filed
  more than 15 months before the companyfacts zip date.

  Reuses a zip already on disk. Pass :force true to download again.
  :limit N parses only the first N CIKs (zip is still the full archive).

  Usage: clj -X:universe-facts
         clj -X:universe-facts :force true
         clj -X:universe-facts :limit 20"
  ([] (universe {}))
  ([{:keys [force limit]}]
   (e/init! dl/identity-header)
   (dl/ensure-zip! {:force force})
   (let [parsed (parse-companyfacts-zip limit)
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
         outdated (count (filter :annual-outdated listed))
         by-unit (frequencies (keep :report-currency listed))
         by-form (frequencies (keep :revenue-form listed))]
     (save-universe! listed)
     (println)
     (pprint/print-table
      [:ticker :exchange :cik :entityName :revenue :report-currency :revenue-form
       :date-last-filed :form-last-filed :annual-outdated]
      (take 12 listed))
     (println (format "Wrote %s  (%d listed; dropped %d empty/ETF/filing-fee, %d unlisted; shares=%d  revenue=%d  annual-outdated=%d)"
                      universe-path (count listed) dropped unlisted with-shares with-rev outdated))
     (println "report-currency" (sort-by val > by-unit))
     (println "revenue-form" (sort-by val > by-form))
     listed)))
