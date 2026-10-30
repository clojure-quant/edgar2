(ns edgar2.filing.quarterly-index
  "Rows of one quarterly EDGAR master index.

  `quarter` is {:year 2026 :quarter 3}. The zip is
  {dir}/{year}/QTR{n}/master.zip and the entry is master.idx.
  Bind *index-dir* to read a different tree. Bytes come from edgar2.util.zip.

  Each row is {:cik :company-name :form :date-filed :filename}
  from CIK|Company Name|Form Type|Date Filed|Filename."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [edgar2.util.zip :as zip]))

(def ^:dynamic *index-dir*
  "Directory that holds {year}/QTR{n}/master.zip."
  "data/full-index")

(defn- master-file
  [{:keys [year quarter]}]
  (io/file *index-dir* (str year) (str "QTR" quarter) "master.zip"))

(defn- parse-line
  [line]
  (let [parts (str/split line #"\|")]
    (when (and (= 5 (count parts))
               (re-matches #"\d{4}-\d{2}-\d{2}" (str/trim (nth parts 3))))
      {:cik (str/trim (nth parts 0))
       :company-name (str/trim (nth parts 1))
       :form (str/trim (nth parts 2))
       :date-filed (str/trim (nth parts 3))
       :filename (str/trim (nth parts 4))})))

(defn all
  "Every data row in the quarter's master.idx."
  [quarter]
  (with-open [z (zip/open-zip-file (master-file quarter))]
    (into []
          (keep parse-line)
          (str/split-lines (zip/read-file-content z "master.idx")))))

(defn- cik-key
  [cik]
  (let [s (str/replace (str cik) #"^0+" "")]
    (if (str/blank? s) "0" s)))

(defn query-cik
  "Rows for `cik`. A string or a number; leading zeros are ignored."
  [quarter cik]
  (let [want (cik-key cik)]
    (filterv #(= want (cik-key (:cik %))) (all quarter))))

(defn query-form
  "Rows whose :form is exactly `form`. \"13F-HR\" leaves out \"13F-HR/A\"."
  [quarter form]
  (filterv #(= form (:form %)) (all quarter)))

(defn on-disk
  "Quarters that have a master.zip, oldest first.
  When opts has :year, only that year."
  [{:keys [year]}]
  (->> (file-seq (io/file *index-dir*))
       (filter #(and (.isFile %) (= "master.zip" (.getName %))))
       (keep (fn [f]
               (let [q (.getName (.getParentFile f))
                     y (.getName (.getParentFile (.getParentFile f)))
                     year-n (parse-long y)]
                 (when (and (re-matches #"QTR[1-4]" q)
                            (re-matches #"\d{4}" y)
                            (or (nil? year) (= (long year) year-n)))
                   {:year year-n
                    :quarter (parse-long (subs q 3))}))))
       (sort-by (juxt :year :quarter))
       vec))
