(ns edgar2.download.fsds-universe
  "Analyze FSDS quarter zips into a bulk 10-K filer overview."
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [edgar.api :as e]
            [edgar2.download.dataset :as dataset]
            [edgar2.download.fsds :as dl]
            [edgar2.report.sector :as sector])
  (:import [java.io BufferedReader InputStreamReader]
           [java.nio.charset StandardCharsets]
           [java.time LocalDate]
           [java.util.zip ZipFile]))

(def filer-info-path "data/universe-fsds.edn")

(def revenue-tags
  ["RevenueFromContractWithCustomerExcludingAssessedTax"
   "Revenues"
   "SalesRevenueNet"
   "SalesRevenueGoodsNet"
   "RevenueFromContractWithCustomerIncludingAssessedTax"
   "RevenuesNetOfInterestExpense"
   "InterestAndDividendIncomeOperating"
   "PremiumsEarnedNet"
   "RealEstateRevenueNet"])

(def overview-tags (set revenue-tags))

(def afs-labels
  {"LAF" "Large accelerated filer"
   "1-LAF" "Large accelerated filer"
   "ACC" "Accelerated filer"
   "2-ACC" "Accelerated filer"
   "SRA" "Smaller reporting accelerated filer"
   "3-SRA" "Smaller reporting accelerated filer"
   "NON" "Non-accelerated filer"
   "4-NON" "Non-accelerated filer"
   "SML" "Smaller reporting company"
   "5-SML" "Smaller reporting company"})

(defn quarter-label
  "2026q3 from a zip path, or nil."
  [path]
  (second (re-find #"(\d{4}q[1-4])\.zip" (str path))))

(defn save-filer-info!
  "Write data/universe-fsds.edn with :download-quarter and :download-date."
  ([rows] (save-filer-info! rows {}))
  ([rows {:keys [download-quarter download-date]}]
   (dataset/write-dataset! filer-info-path
                           (cond-> {}
                             download-quarter (assoc :download-quarter download-quarter)
                             download-date (assoc :download-date (str download-date)))
                           rows)))

(defn fmt-revenue
  [v]
  (if v
    (format "%.0fM" (/ (double v) 1.0e6))
    "-"))

(defn parse-num
  [s]
  (when (and s (not (str/blank? (str s))))
    (try (Double/parseDouble (str s))
         (catch Exception _ nil))))

(defn yyyymmdd->iso
  [s]
  (let [d (re-find #"\d{8}" (str s))]
    (when d
      (str (subs d 0 4) "-" (subs d 4 6) "-" (subs d 6 8)))))

(defn pad-cik
  [cik]
  (format "%010d" (Long/parseLong (str cik))))

(defn sic-code
  [s]
  (when-let [n (parse-num s)]
    (format "%04d" (long n))))

(defn category-label
  [afs]
  (when (and afs (not (str/blank? (str afs))))
    (or (afs-labels (str/upper-case (str afs)))
        (afs-labels (str/upper-case (last (str/split (str afs) #"-"))))
        (str afs))))

(defn reduce-zip-tsv
  [zip-path entry-name rf init]
  (with-open [zf (ZipFile. (str zip-path))]
    (let [entry (.getEntry zf entry-name)]
      (when-not entry
        (throw (ex-info (str entry-name " missing in " zip-path)
                        {:zip zip-path :entry entry-name})))
      (with-open [r (BufferedReader.
                     (InputStreamReader. (.getInputStream zf entry)
                                         StandardCharsets/UTF_8))]
        (let [headers (mapv keyword (str/split (str/trimr (.readLine r)) #"\t"))]
          (loop [acc init]
            (if-let [line (.readLine r)]
              (recur (rf acc (zipmap headers (str/split (str/trimr line) #"\t" -1))))
              acc)))))))

(defn newer-sub?
  [a b]
  (pos? (compare (str (:filed a)) (str (:filed b)))))

(def annual-forms #{"10-K"})

(defn collect-annual-subs
  "Latest 10-K per CIK from FSDS SUB tables. Returns {cik sub}."
  [zip-paths]
  (reduce
   (fn [by-cik zip-path]
     (println (format "  parse SUB %s" (.getName (io/file zip-path))))
     (flush)
     (reduce-zip-tsv
      zip-path "sub.txt"
      (fn [acc row]
        (let [form (str (:form row))]
          (if (or (not (annual-forms form))
                  (str/blank? (str (:cik row))))
            acc
            (let [cik (pad-cik (:cik row))
                  sub {:cik cik
                       :name (str/trim (str (:name row)))
                       :adsh (:adsh row)
                       :sic (sic-code (:sic row))
                       :afs (:afs row)
                       :period (str (:period row))
                       :filed (str (:filed row))}]
              (if (or (nil? (get acc cik))
                      (newer-sub? sub (get acc cik)))
                (assoc acc cik sub)
                acc)))))
      by-cik))
   {}
   zip-paths))

(defn better-fact
  [prev row period]
  (let [ddate (str (:ddate row))
        qtrs (or (some-> (:qtrs row) parse-num long) -1)
        val (parse-num (:value row))
        cand {:val val :ddate ddate :qtrs qtrs}
        period (str period)]
    (cond
      (nil? val) prev
      (nil? prev) cand
      (and (= ddate period) (not= (:ddate prev) period)) cand
      (and (= ddate period) (= (:ddate prev) period) (> qtrs (:qtrs prev))) cand
      (and (not= (:ddate prev) period) (pos? (compare ddate (:ddate prev)))) cand
      :else prev)))

(defn collect-overview-facts
  "Revenue facts for the latest 10-K accessions. Segmented facts are skipped."
  [zip-paths tenk-by-cik]
  (let [period-by-adsh (into {} (map (juxt :adsh :period) (vals tenk-by-cik)))
        tenk-adsh (set (map :adsh (vals tenk-by-cik)))]
    (reduce
     (fn [facts zip-path]
       (println (format "  parse NUM %s" (.getName (io/file zip-path))))
       (flush)
       (reduce-zip-tsv
        zip-path "num.txt"
        (fn [acc row]
          (let [adsh (:adsh row)]
            (if (or (not (tenk-adsh adsh))
                    (not (overview-tags (:tag row)))
                    (not (str/blank? (str (:coreg row))))
                    (not (str/blank? (str (:segments row)))))
              acc
              (update-in acc [adsh (:tag row)]
                         better-fact row (period-by-adsh adsh)))))
        facts))
     {}
     zip-paths)))

(defn fact-val
  [facts adsh tags qtrs]
  (some (fn [tag]
          (let [f (get-in facts [adsh tag])]
            (when (and f (or (nil? qtrs) (= qtrs (:qtrs f))))
              (:val f))))
        tags))

(defn fact-val*
  [facts adsh tags qtrs]
  (or (fact-val facts adsh tags qtrs)
      (fact-val facts adsh tags nil)))

(defn build-filer-info-row
  [sub facts]
  (let [adsh (:adsh sub)
        sic (:sic sub)]
    (merge {:cik (:cik sub)
            :name (:name sub)
            :sic sic
            :category (category-label (:afs sub))
            :last-10k-date (yyyymmdd->iso (:filed sub))
            :last-10k-period (yyyymmdd->iso (:period sub))
            :last-10k-accession adsh
            :last-10k-revenue (fact-val* facts adsh revenue-tags 4)}
           (sector/get-sector-data sic))))

(defn filer-info
  "Build an overview of every CIK with a 10-K in the recent FSDS quarters.
  Sector names come from resources/sic.edn.

  Usage: clj -X:download-universe-fsds
         clj -X:download-universe-fsds :limit 10"
  ([] (filer-info {}))
  ([{:keys [limit]}]
   (e/init! dl/identity-header)
   (e/enable-disk-cache! :dir "data/edgar-cache")
   (println (format "filer-info  overview from FSDS bulk files  → %s" filer-info-path))
   (println "Downloading recent FSDS quarters (cached after the first run)…")
   (let [zips (dl/ensure-quarters!)]
     (when (empty? zips)
       (throw (ex-info "No FSDS quarter zips available" {:dir dl/fsds-dir})))
     (println (format "Using %d quarter zip(s)" (count zips)))
     (let [tenk (collect-annual-subs zips)
           subs (vec (cond->> (sort-by (juxt :name :cik) (vals tenk))
                       limit (take (long limit))))
           n (count subs)
           facts (collect-overview-facts zips tenk)
           rows (mapv #(build-filer-info-row % facts) subs)]
       (println (format "filer-info  %d filers with a 10-K" n))
       (doseq [[i row] (map-indexed vector rows)]
         (println (format "%d/%d  %s  %s  10-K=%s  period=%s  rev=%s  sic=%s  %s"
                          (inc i) n
                          (:cik row) (:name row)
                          (or (:last-10k-date row) "-")
                          (or (:last-10k-period row) "-")
                          (fmt-revenue (:last-10k-revenue row))
                          (or (:sic row) "-")
                          (or (:industry row) "-")))
         (flush))
       (let [quarter (some quarter-label zips)]
         (save-filer-info! rows {:download-quarter quarter
                                 :download-date (str (LocalDate/now))})
         (println (format "Wrote %s  (%d filers, %d with a sector, quarter %s)"
                          filer-info-path
                          (count rows)
                          (count (filter :sector rows))
                          (or quarter "-"))))
       (when (.exists (io/file "data/filer-info.log.edn"))
         (io/delete-file "data/filer-info.log.edn" true))
       rows))))
