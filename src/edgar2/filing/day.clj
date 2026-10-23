(ns edgar2.filing.day
  "One EDGAR daily feed: {dest}/{year}/QTR{n}/{yyyymmdd}.nc.tar.gz.

  Members are stored as ./{accession}.nc. Archive bytes are read through
  edgar2.filing.tar."
  (:require [clojure.java.io :as io]
            [edgar2.filing.tar :as tar]))

(def dest
  "/run/media/florian/26ad683d-44f4-42d8-bbd9-0a5d0b792fe0/sec")

(defn archive-file
  "Finished daily feed for a YYYY-MM-DD filing date, or nil.
  Incomplete `.part` files are left out."
  [filing-date]
  (let [y (subs filing-date 0 4)
        m (parse-long (subs filing-date 5 7))
        d (subs filing-date 8 10)
        q (inc (quot (dec m) 3))
        f (io/file dest y (str "QTR" q)
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
