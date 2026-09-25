(ns edgar2.facts.parse
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [edgar.api :as e]
            [edgar2.facts.download :as dl]
            [jsonista.core :as json])
  (:import [java.util.zip ZipFile]))

(def facts-path "data/facts.edn")

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

(defn concept-observations
  [facts taxonomy tag]
  (mapcat val (get-in facts [taxonomy tag :units])))

(defn most-recent-val
  "Latest :val by :filed (then :end) across the given [taxonomy tag] pairs."
  [facts tag-pairs]
  (some->> tag-pairs
           (mapcat (fn [[tax tag]] (concept-observations facts tax tag)))
           seq
           (sort-by #(str (:filed %) "|" (:end %)))
           last
           :val))

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
  (let [facts (:facts data)]
    (cond-> {:cik (when (:cik data) (pad-cik (:cik data)))
             :entityName (:entityName data)
             :shares-outstanding (most-recent-val facts share-tags)
             :revenue (most-recent-val facts revenue-tags)
             :reporting-standard (reporting-standard facts)}
      (contains? facts :ffd)
      (assoc :note "filing-fee disclosure"))))

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

(defn save-facts!
  [rows]
  (.mkdirs (io/file "data"))
  (let [tmp (str facts-path ".tmp")]
    (spit tmp (with-out-str (pprint/pprint (vec rows))))
    (.renameTo (io/file tmp) (io/file facts-path))))

(defn facts-all
  "Download companyfacts.zip (with progress) and write data/facts.edn.

  Reuses a zip already on disk. Pass :force true to download again.
  :limit N parses only the first N JSON files (zip is still the full archive).

  Usage: clj -X:facts-all
         clj -X:facts-all :force true
         clj -X:facts-all :limit 20"
  ([] (facts-all {}))
  ([{:keys [force limit]}]
   (e/init! dl/identity-header)
   (let [zip-path (dl/ensure-zip! {:force force})
         parsed (parse-companyfacts-zip zip-path limit)
         dropped (count (remove keep-fact? parsed))
         rows (->> parsed
                   (filter keep-fact?)
                   (sort-by :cik)
                   vec)
         with-shares (count (filter :shares-outstanding rows))
         with-rev (count (filter :revenue rows))]
     (save-facts! rows)
     (println)
     (pprint/print-table
      [:cik :entityName :shares-outstanding :revenue :reporting-standard]
      (take 12 (remove #(str/blank? (str (:cik %))) rows)))
     (println (format "Wrote %s  (%d entities; dropped %d empty/ETF/filing-fee; shares=%d  revenue=%d)"
                      facts-path (count rows) dropped with-shares with-rev))
     rows)))
