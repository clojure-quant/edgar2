(ns edgar2.util.zip
  "Random-access reader for a .zip archive.

  (open-zip-file filename) returns a java.util.zip.ZipFile. Close it with
  with-open. get-file-info lists entry names. read-file-content returns one
  entry as a UTF-8 string."
  (:require [clojure.java.io :as io])
  (:import [java.util.zip ZipFile]))

(defn open-zip-file
  "Open `filename` (path string or File). Caller closes the ZipFile."
  [filename]
  (ZipFile. (io/file filename)))

(defn get-file-info
  "Entry names in `zip-reader`, directories omitted, sorted."
  [^ZipFile zip-reader]
  (->> (enumeration-seq (.entries zip-reader))
       (remove #(.isDirectory ^java.util.zip.ZipEntry %))
       (map #(.getName ^java.util.zip.ZipEntry %))
       sort
       vec))

(defn read-file-content
  "UTF-8 text of entry `filename`. Throws when the entry is absent."
  [^ZipFile zip-reader filename]
  (let [entry (or (.getEntry zip-reader (str filename))
                  (throw (ex-info "Zip entry not found"
                                  {:zip (.getName zip-reader)
                                   :name filename})))]
    (with-open [in (.getInputStream zip-reader entry)]
      (slurp in :encoding "UTF-8"))))
