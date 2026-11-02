(ns edgar2.download.fsds-universe
  "Analyze FSDS quarter zips into a bulk annual-report filer overview."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [edgar2.financials.concepts :as concepts]
            [edgar2.util.core :as ucore]
            [edgar2.download.dataset :as dataset]
            [edgar2.download.fsds :as dl]
            [edgar2.db.sector :as sector])
  (:import [java.io BufferedReader InputStreamReader]
           [java.nio.charset StandardCharsets]
           [java.time LocalDate]
           [java.util.zip ZipFile]))

(def filer-info-path "data/universe-fsds.edn")

(def revenue-tags
  "Local tag names from the canonical :revenue concept (FSDS has no taxonomy)."
  (mapv (comp name second) (:data (concepts/get-concept-id :revenue))))

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

(def annual-forms
  #{"10-K" "10-K/A" "20-F" "20-F/A" "40-F" "40-F/A"})

(defn annual-sub?
  [row]
  (let [form (str/trim (str (:form row)))]
    (and (contains? annual-forms form)
         (not (str/blank? (str/trim (str (:cik row)))))
         (not (str/blank? (str/trim (str (:adsh row))))))))

(defn sub-from-row
  [row]
  {:cik (pad-cik (str/trim (str (:cik row))))
   :name (str/trim (str (:name row)))
   :adsh (str/trim (str (:adsh row)))
   :form (str/trim (str (:form row)))
   :sic (sic-code (:sic row))
   :afs (:afs row)
   :period (str/trim (str (:period row)))
   :filed (str/trim (str (:filed row)))})

(defn collect-annual-subs
  "All annual report candidates (10-K/20-F/40-F and amendments) per CIK
  from the FSDS SUB tables. Returns {cik [sub ...]}."
  [zip-paths]
  (reduce
   (fn [by-cik zip-path]
     (println (format "  parse SUB %s" (.getName (io/file zip-path))))
     (flush)
     (reduce-zip-tsv
      zip-path "sub.txt"
      (fn [acc row]
        (if (annual-sub? row)
          (let [sub (sub-from-row row)]
            (update acc (:cik sub) (fnil conj []) sub))
          acc))
      by-cik))
   {}
   zip-paths))

(defn- period-key
  "Ordering key for a sub: its report period, or the filed date when the
  period is blank. Both are YYYYMMDD strings so they sort lexicographically."
  [sub]
  (let [period (str/trim (str (:period sub)))]
    (if (str/blank? period)
      (str/trim (str (:filed sub)))
      period)))

(defn- amendment?
  [sub]
  (str/ends-with? (str/upper-case (str (:form sub))) "/A"))

(defn newer-sub?
  "True when `a` should be selected ahead of `b`: newest report period first,
  then newest filed date, then the amendment on a tied filed date."
  [a b]
  (let [pa (period-key a)
        pb (period-key b)]
    (cond
      (not= pa pb) (pos? (compare pa pb))
      (not= (str (:filed a)) (str (:filed b)))
      (pos? (compare (str (:filed a)) (str (:filed b))))
      :else (and (amendment? a) (not (amendment? b))))))

(defn- sub-order
  [a b]
  (cond
    (newer-sub? a b) -1
    (newer-sub? b a) 1
    :else 0))

(defn select-annual-subs
  "Turn {cik [candidate ...]} into {cik selected-sub}. The selected sub is the
  newest period, with same-period accessions kept in `:annual-adshs` (selected
  first, then newest filed) so revenue can fall back to the original filing."
  [subs-by-cik]
  (into {}
        (map (fn [[cik subs]]
               (let [selected (reduce (fn [best sub]
                                        (if (newer-sub? sub best) sub best))
                                      subs)
                     key (period-key selected)
                     same-period (filter #(= key (period-key %)) subs)
                     ordered (sort sub-order same-period)]
                 [cik (assoc selected :annual-adshs
                             (vec (distinct (map :adsh ordered))))])))
        subs-by-cik))

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
  "Revenue facts for the latest annual accessions. Segmented facts are skipped."
  [zip-paths subs-by-cik]
  (let [annual-adsh (set (mapcat :annual-adshs (vals subs-by-cik)))
        period-by-adsh (into {}
                             (for [sub (vals subs-by-cik)
                                   adsh (:annual-adshs sub)]
                               [adsh (:period sub)]))]
    (reduce
     (fn [facts zip-path]
       (println (format "  parse NUM %s" (.getName (io/file zip-path))))
       (flush)
       (reduce-zip-tsv
        zip-path "num.txt"
        (fn [acc row]
          (let [adsh (:adsh row)]
            (if (or (not (annual-adsh adsh))
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

(defn annual-revenue
  "First non-nil revenue across `adshs` in preference order."
  [facts adshs]
  (some #(fact-val* facts % revenue-tags 4) adshs))

(defn build-filer-info-row
  [sub facts]
  (let [sic (:sic sub)
        adshs (or (seq (:annual-adshs sub)) [(:adsh sub)])]
    (merge {:cik (:cik sub)
            :name (:name sub)
            :sic sic
            :category (category-label (:afs sub))
            :last-annual-date (yyyymmdd->iso (:filed sub))
            :last-annual-period (yyyymmdd->iso (:period sub))
            :last-annual-form (:form sub)
            :last-annual-accession (:adsh sub)
            :last-annual-revenue (annual-revenue facts adshs)}
           (sector/get-sector-data sic))))

(defn filer-info
  "Build an overview of every CIK with a 10-K, 20-F, or 40-F in the recent
  FSDS quarters. Sector names come from resources/sic.edn.

  Usage: clj -X:download-universe-fsds
         clj -X:download-universe-fsds :limit 10"
  ([] (filer-info {}))
  ([{:keys [limit]}]
   (ucore/ensure-identity!)
   (ucore/enable-cache)
   (println (format "filer-info  overview from FSDS bulk files  → %s" filer-info-path))
   (println "Downloading recent FSDS quarters (cached after the first run)…")
   (let [zips (dl/ensure-quarters!)]
     (when (empty? zips)
       (throw (ex-info "No FSDS quarter zips available" {:dir dl/fsds-dir})))
     (println (format "Using %d quarter zip(s)" (count zips)))
     (let [annual (select-annual-subs (collect-annual-subs zips))
           subs (vec (cond->> (sort-by (juxt :name :cik) (vals annual))
                       limit (take (long limit))))
           n (count subs)
           facts (collect-overview-facts zips annual)
           rows (mapv #(build-filer-info-row % facts) subs)]
       (println (format "filer-info  %d filers with an annual report" n))
       (doseq [[i row] (map-indexed vector rows)]
         (println (format "%d/%d  %s  %s  form=%s  filed=%s  period=%s  rev=%s  sic=%s  %s"
                          (inc i) n
                          (:cik row) (:name row)
                          (or (:last-annual-form row) "-")
                          (or (:last-annual-date row) "-")
                          (or (:last-annual-period row) "-")
                          (fmt-revenue (:last-annual-revenue row))
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
