(ns edgar2.download.dataset
  "EDN datasets stored as {:download-date ... :rows [...]}.

  Readers still accept a bare vector from older files."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as str]))

(defn rows
  "Row vector from a dataset value. A bare sequence is the rows themselves."
  [data]
  (cond
    (nil? data) []
    (map? data) (vec (:rows data))
    (sequential? data) (vec data)
    :else []))

(defn read-rows
  "Rows from `path`, or nil when the file is missing."
  [path]
  (let [f (io/file path)]
    (when (.exists f)
      (rows (edn/read-string (slurp f))))))

(defn write-dataset!
  "Write `rows` with header keys from `meta` (inserted before :rows)."
  [path meta rows]
  (let [file (io/file path)
        tmp (str path ".tmp")
        data (into (array-map)
                   (concat (seq meta) [[:rows (vec rows)]]))]
    (some-> (.getParentFile file) .mkdirs)
    (spit tmp (with-out-str (pprint/pprint data)))
    (.renameTo (io/file tmp) file)
    path))

(defn peek-meta
  "Header keys without reading :rows. Nil when the file is missing.
  A legacy bare vector comes back as {}."
  [path]
  (let [f (io/file path)]
    (when (.exists f)
      (let [text (with-open [r (io/reader f)]
                   (let [buf (char-array 600)
                         n (.read r buf)]
                     (if (neg? n) "" (String. buf 0 n))))
            trimmed (str/triml text)]
        (if (str/starts-with? trimmed "[")
          {}
          (into {}
                (keep (fn [[k re]]
                        (when-let [v (second (re-find re text))]
                          [k v])))
                {:download-date #":download-date \"([^\"]+)\""
                 :download-quarter #":download-quarter \"([^\"]+)\""}))))))
