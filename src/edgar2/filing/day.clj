(ns edgar2.filing.day
  "One EDGAR daily feed: {dest}/{year}/QTR{n}/{yyyymmdd}.nc.tar.gz.

  Members are stored as ./{accession}.nc. Archive bytes are read through
  edgar2.util.tar."
  (:require
   [clojure.java.io :as io]
   [edgar2.util.tar :as tar]
   [edgar2.config :refer [day-path]]))


(defn archive-file
  "Finished daily feed for a YYYY-MM-DD filing date, or nil.
  Incomplete `.part` files are left out."
  [filing-date]
  (let [y (subs filing-date 0 4)
        m (parse-long (subs filing-date 5 7))
        d (subs filing-date 8 10)
        q (inc (quot (dec m) 3))
        f (io/file day-path y (str "QTR" q)
                   (str y (subs filing-date 5 7) d ".nc.tar.gz"))]
    (when (.isFile f) f)))

(defn- member-name
  [accession]
  (str "./" accession ".nc"))

(defn read-accessions
  "Submission text for each accession present in `archive`.
  Returns {accession text}. One pass."
  [archive accessions]
  (let [accs (vec accessions)
        bodies (tar/read-files (tar/open-tar-file archive)
                               (map member-name accs))]
    (into {}
          (keep (fn [acc]
                  (when-let [text (get bodies (member-name acc))]
                    [acc text])))
          accs)))
