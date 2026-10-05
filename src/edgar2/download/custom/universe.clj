(ns edgar2.download.custom.universe
  "Companyfacts extract joined to listed ticker / exchange by CIK."
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [edgar.api :as e]
            [edgar2.download.facts :as dl]
            [edgar2.download.ticker-exchange :as exchanges]
            [jsonista.core :as json])
  (:import [java.util.zip ZipFile]))

(def universe-path "data/universe.edn")

(def share-tags
  [[:dei :EntityCommonStockSharesOutstanding]
   [:us-gaap :CommonStockSharesOutstanding]
   [:ifrs-full :NumberOfSharesOutstanding]])

(def revenue-tags
  [[:us-gaap :RevenueFromContractWithCustomerExcludingAssessedTax]
   [:us-gaap :Revenues]
   [:us-gaap :SalesRevenueNet]
   [:us-gaap :RevenueFromContractWithCustomerIncludingAssessedTax]
   [:ifrs-full :Revenue]
   [:ifrs-full :RevenueFromContractsWithCustomers]])

(defn pad-cik
  [cik]
  (format "%010d" (Long/parseLong (str cik))))

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

(defn most-recent-val
  [facts tag-pairs]
  (:val (most-recent-obs facts tag-pairs)))

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
        rev (most-recent-obs facts revenue-tags)]
    (cond-> {:cik (when (:cik data) (pad-cik (:cik data)))
             :entityName (:entityName data)
             :shares-outstanding (most-recent-val facts share-tags)
             :revenue (:val rev)
             :reporting-standard (reporting-standard facts)}
      (:unit rev) (assoc :revenue-unit (:unit rev))
      (:form rev) (assoc :revenue-form (:form rev))
      (:fp rev) (assoc :revenue-fp (:fp rev))
      (contains? facts :ffd) (assoc :note "filing-fee disclosure"))))

(defn listing-by-cik
  "CIK → {:ticker :exchange} from data/ticker-exchange.edn."
  []
  (into {}
        (keep (fn [{:keys [cik ticker exchange]}]
                (when cik
                  [cik {:ticker ticker :exchange exchange}]))
              (exchanges/load-exchanges))))

(defn with-listing
  [by-cik row]
  (if-let [listing (get by-cik (:cik row))]
    (merge row listing)
    row))

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
           (and (nil? revenue) (etf-name? entityName))
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
  [rows]
  (.mkdirs (io/file "data"))
  (let [tmp (str universe-path ".tmp")]
    (spit tmp (with-out-str (pprint/pprint (vec rows))))
    (.renameTo (io/file tmp) (io/file universe-path))))

(defn universe
  "Download companyfacts.zip (with progress), join listed ticker/exchange by CIK,
  and write data/universe.edn (unlisted CIKs omitted).

  Reuses a zip already on disk. Pass :force true to download again.
  :limit N parses only the first N JSON files (zip is still the full archive).

  Usage: clj -X:universe
         clj -X:universe :force true
         clj -X:universe :limit 20"
  ([] (universe {}))
  ([{:keys [force limit]}]
   (e/init! dl/identity-header)
   (let [zip-path (dl/ensure-zip! {:force force})
         by-cik (listing-by-cik)
         parsed (parse-companyfacts-zip zip-path limit)
         kept (filter keep-fact? parsed)
         dropped (- (count parsed) (count kept))
         listed (->> kept
                     (map #(with-listing by-cik %))
                     (filter #(not (str/blank? (str (:ticker %)))))
                     (sort-by :cik)
                     vec)
         unlisted (- (count kept) (count listed))
         with-shares (count (filter :shares-outstanding listed))
         with-rev (count (filter :revenue listed))
         by-unit (frequencies (keep :revenue-unit listed))
         by-form (frequencies (keep :revenue-form listed))]
     (save-universe! listed)
     (println)
     (pprint/print-table
      [:ticker :exchange :cik :entityName :revenue :revenue-unit :revenue-form]
      (take 12 listed))
     (println (format "Wrote %s  (%d listed; dropped %d empty/ETF/filing-fee, %d unlisted; shares=%d  revenue=%d)"
                      universe-path (count listed) dropped unlisted with-shares with-rev))
     (println "revenue-unit" (sort-by val > by-unit))
     (println "revenue-form" (sort-by val > by-form))
     listed)))
