(ns edgar2.institutional
  "13F holdings from the local EDGAR index and daily feeds.

  edgar2.filing.quarterly-index reads each master.zip. Those rows are
  reduced to data/13f-index.edn, the list of 13F-HR and 13F-HR/A filings
  to process.

  Each filing is then read from the daily archive via edgar2.filing.day.
  A day that is not downloaded is skipped. Results go to data/holdings.edn.

  Holdings keep the CUSIP from the information table. Share lines
  (sshPrnamtType SH, no put or call) are summed by CUSIP. :total-value is
  the sum of the value column, in dollars (filings before 2023-01-03
  reported thousands).

  Usage:
    (print-filers)
    clj -X:institutional"
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [clojure.xml :as xml]
            [edgar2.filing.day :as day]
            [edgar2.filing.quarterly-index :as qi])
  (:import [java.io ByteArrayInputStream]))

(def index-path "data/13f-index.edn")
(def holdings-path "data/holdings.edn")

(def holdings-forms #{"13F-HR" "13F-HR/A"})

(defn- field
  [header tag]
  (some-> (re-find (re-pattern (str "<" tag ">\\s*([^\\r\\n<]+)")) header)
          second
          str/trim
          not-empty))

(defn- iso-day
  [yyyymmdd]
  (when (and yyyymmdd (= 8 (count yyyymmdd)))
    (str (subs yyyymmdd 0 4) "-" (subs yyyymmdd 4 6) "-" (subs yyyymmdd 6 8))))

(defn- report-period
  "Cover `periodOfReport` is MM-DD-YYYY. Store YYYY-MM-DD."
  [s]
  (cond
    (re-matches #"\d{4}-\d{2}-\d{2}" s) s
    (re-matches #"\d{2}-\d{2}-\d{4}" s)
    (let [[m d y] (str/split s #"-")]
      (str y "-" m "-" d))
    :else s))

(defn- tag-name
  [node-tag tag]
  (or (= tag node-tag)
      (when (keyword? node-tag)
        (= (name tag) (last (str/split (name node-tag) #":"))))))

(defn- find-tag
  [node tag]
  (when (map? node)
    (if (tag-name (:tag node) tag)
      node
      (some #(find-tag % tag) (:content node)))))

(defn- find-tags
  [node tag]
  (when (map? node)
    (let [here (if (tag-name (:tag node) tag) [node] [])]
      (into here (mapcat #(find-tags % tag) (:content node))))))

(defn- tag-text
  [node tag]
  (some-> (find-tag node tag) :content first str str/trim not-empty))

(defn- child-text
  [node tag]
  (some->> (:content node)
           (filter #(and (map? %) (tag-name (:tag %) tag)))
           first
           :content
           first
           str
           str/trim
           not-empty))

(defn- parse-long-safe
  [s]
  (when (not-empty (str/trim (or s "")))
    (try (parse-long (str/replace s "," ""))
         (catch Exception _ nil))))

(defn- parse-xml
  [s]
  (let [xml (cond
              (str/includes? s "<XML>")
              (subs s
                    (+ (str/index-of s "<XML>") 5)
                    (or (str/last-index-of s "</XML>") (count s)))
              :else s)
        xml (str/trim xml)]
    (when (str/starts-with? xml "<")
      (xml/parse (ByteArrayInputStream. (.getBytes xml "UTF-8"))))))

(defn- documents
  [submission]
  (loop [from 0 acc []]
    (if-let [i (str/index-of submission "<DOCUMENT>" from)]
      (if-let [j (str/index-of submission "</DOCUMENT>" i)]
        (recur (+ j 11) (conj acc (subs submission i j)))
        acc)
      acc)))

(defn- doc-type
  [doc]
  (some-> (re-find #"<TYPE>\s*([^\r\n<]+)" doc) second str/trim))

(defn- doc-text
  [doc]
  (when-let [i (str/index-of doc "<TEXT>")]
    (when-let [j (str/last-index-of doc "</TEXT>")]
      (when (> j i)
        (subs doc (+ i 6) j)))))

(defn- info-table-root
  [submission]
  (some (fn [doc]
          (when (and (= "INFORMATION TABLE" (doc-type doc))
                     (doc-text doc))
            (parse-xml (doc-text doc))))
        (documents submission)))

(defn- cover-root
  [submission]
  (some (fn [doc]
          (let [text (doc-text doc)]
            (when (and text
                       (not= "INFORMATION TABLE" (doc-type doc))
                       (str/includes? text "filingManager"))
              (parse-xml text))))
        (documents submission)))

(defn- holding-rows
  [root]
  (if-not root
    []
    (mapv (fn [entry]
          (let [amt (find-tag entry :shrsOrPrnAmt)]
            {:cusip (tag-text entry :cusip)
             :issuer (tag-text entry :nameOfIssuer)
             :class (tag-text entry :titleOfClass)
             :value (parse-long-safe (tag-text entry :value))
             :shares (parse-long-safe (child-text amt :sshPrnamt))
             :shares-type (child-text amt :sshPrnamtType)
             :put-call (tag-text entry :putCall)}))
        (find-tags root :infoTable))))

(defn- usd
  [n filing-date]
  (cond
    (nil? n) nil
    (and filing-date (neg? (compare filing-date "2023-01-03"))) (* n 1000)
    :else n))

(defn- stock-holdings
  [rows filing-date]
  (->> rows
       (filter (fn [{:keys [put-call shares-type shares cusip]}]
                 (and (not-empty cusip)
                      (str/blank? (str put-call))
                      (= "SH" (some-> shares-type str/upper-case))
                      (and shares (pos? shares)))))
       (group-by :cusip)
       (map (fn [[cusip group]]
              {:cusip cusip
               :issuer (some :issuer group)
               :class (some :class group)
               :qty (reduce + 0 (keep :shares group))
               :value (usd (reduce + 0 (keep :value group)) filing-date)}))
       (sort-by :cusip)
       vec))

(defn get-holdings
  "Essence of one 13F submission string.

  `filing` is {:submission \"<SUBMISSION>...\"}. Returns nil when the text
  has no information table.

  {:filer 1067983
   :filer-name \"BERKSHIRE HATHAWAY INC\"
   :filing-date \"2026-08-14\"
   :report-period \"2025-12-31\"
   :total-count 405
   :total-value 299253556246
   :holdings [{:cusip \"037833100\"
               :issuer \"APPLE INC\"
               :class \"COM\"
               :qty 123455
               :value 25000000}]}"
  [{:keys [submission]}]
  (when submission
    (let [head (subs submission 0 (or (str/index-of submission "<DOCUMENT>")
                                      (count submission)))
          filed (iso-day (field head "FILING-DATE"))
          rows (holding-rows (info-table-root submission))
          cover (cover-root submission)]
      (when (seq rows)
        {:filer (parse-long (or (field head "CIK") "0"))
         :filer-name (or (when cover (tag-text (find-tag cover :filingManager) :name))
                         (field head "CONFORMED-NAME"))
         :filing-date filed
         :report-period (some-> cover (tag-text :periodOfReport) report-period)
         :total-count (some-> cover (tag-text :tableEntryTotal) parse-long-safe)
         :total-value (usd (reduce + 0 (keep :value rows)) filed)
         :holdings (stock-holdings rows filed)}))))

(defn- write-edn!
  [path data]
  (let [file (io/file path)
        tmp (str path ".tmp")]
    (some-> (.getParentFile file) .mkdirs)
    (spit tmp (binding [*print-length* nil
                        *print-level* nil]
                (with-out-str (pprint/pprint data))))
    (.renameTo (io/file tmp) file)
    path))

(defn- accession
  [filename]
  (second (re-find #"(\d{10}-\d{2}-\d{6})" (str filename))))

(defn- index-row
  [row]
  (when-let [acc (accession (:filename row))]
    {:cik (:cik row)
     :name (:company-name row)
     :form (:form row)
     :filing-date (:date-filed row)
     :accession acc}))

(defn write-index!
  "Write data/13f-index.edn from every quarterly master.zip on disk.

  {:filings [{:cik :name :form :filing-date :accession} ...]}"
  []
  (let [quarters (qi/on-disk)]
    (when (empty? quarters)
      (throw (ex-info "No quarterly master.zip under data/full-index"
                      {:dir qi/*index-dir*})))
    (let [filings (->> quarters
                       (mapcat (fn [quarter]
                                 (println (format "  index %d QTR%d"
                                                  (:year quarter)
                                                  (:quarter quarter)))
                                 (flush)
                                 (mapcat #(qi/query-form quarter %) holdings-forms)))
                       (keep index-row)
                       (group-by :accession)
                       (map (comp first val))
                       (sort-by (juxt :filing-date :accession))
                       vec)
          data {:filings filings}]
      (write-edn! index-path data)
      (println (format "Wrote %s  (%d filings)" index-path (count filings)))
      (flush)
      data)))

(defn- load-day
  [archive rows]
  (let [bodies (day/read-accessions archive (map :accession rows))]
    (println (format "    read %d/%d" (count bodies) (count rows)))
    (flush)
    (keep (fn [row]
            (if-let [text (get bodies (:accession row))]
              (try
                (get-holdings {:submission text})
                (catch Exception ex
                  (println (format "    skip %s %s" (:accession row) (.getMessage ex)))
                  nil))
              (do (println (format "    missing %s" (:accession row)))
                  nil)))
          rows)))

(defn load-holdings
  "Holdings for index filings whose daily archive is already on disk."
  [filings]
  (let [by-day (group-by :filing-date filings)
        days (sort (keys by-day))
        present (filter day/archive-file days)
        skipped (remove day/archive-file days)]
    (println (format "%d filings in the index, %d days on disk, %d days skipped"
                     (count filings)
                     (count present)
                     (count skipped)))
    (flush)
    (->> present
         (mapcat (fn [day]
                   (let [rows (get by-day day)
                         archive (day/archive-file day)]
                     (println (format "  %s  %d filings" (.getName archive) (count rows)))
                     (flush)
                     (load-day archive rows))))
         (sort-by (juxt :filing-date :filer))
         vec)))

(defn write-holdings!
  [filers]
  (write-edn! holdings-path {:filers filers})
  (println (format "Wrote %s  (%d filers)" holdings-path (count filers)))
  (flush)
  filers)

(defn- money
  [n]
  (if n (format "$%,d" (long n)) ""))

(defn- print-table
  [filers]
  (let [rows (->> filers
                  (map (fn [h]
                         {:filer (:filer h)
                          :name (:filer-name h)
                          :holdings (count (:holdings h))
                          :value (:total-value h)}))
                  (sort-by #(or (:value %) -1) #(compare %2 %1))
                  vec)
        total (reduce + 0 (keep :value rows))]
    (println (format "%d filers   %s" (count rows) (money total)))
    (pprint/print-table
     [:filer :name :holdings :value]
     (map #(update % :value money) rows))
    nil))

(defn print-filers
  "Build data/13f-index.edn, write data/holdings.edn from the daily
  archives that are already downloaded, then print the filer table."
  ([] (print-filers nil))
  ([_]
   (let [index (write-index!)
         filers (load-holdings (:filings index))]
     (write-holdings! filers)
     (print-table filers))))
