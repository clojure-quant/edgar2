(ns edgar2.filing.tar
  "Reader for a .tar.gz archive.

  A gzip stream cannot be rewound, so each call scans from the start.
  open-tar-file returns {:file file}. get-file-info lists member names.
  read-file-content returns one member. read-files takes many members in
  one pass.

  Member text is ISO-8859-1. Names match exactly."
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.util.zip GZIPInputStream]))

(def ^:private discard-buf (byte-array (* 1024 1024)))

(defn open-tar-file
  "Handle for `filename` (path string or File)."
  [filename]
  (let [f (io/file filename)]
    (when-not (.isFile f)
      (throw (ex-info "Tar archive not found" {:file (str f)})))
    {:file f}))

(defn- read-fully
  "Exactly `n` bytes, or an empty array when `n` is zero. Nil at EOF."
  [^java.io.InputStream in n]
  (cond
    (zero? n) (byte-array 0)
    (pos? n)
    (let [buf (byte-array n)
          got (.readNBytes in buf 0 n)]
      (when (= got n) buf))))

(defn- discard
  [^java.io.InputStream in n]
  (loop [left n]
    (when (pos? left)
      (let [got (.read in discard-buf 0 (int (min left (alength discard-buf))))]
        (if (neg? got)
          (throw (ex-info "Unexpected end of tar archive" {}))
          (recur (- left got)))))))

(defn- cstr
  [^bytes buf off len]
  (let [end (loop [i off]
              (cond
                (>= i (+ off len)) (+ off len)
                (zero? (aget buf i)) i
                :else (recur (inc i))))]
    (-> (String. buf off (- end off) "US-ASCII")
        str/trim)))

(defn- octal
  [^bytes buf off len]
  (let [s (cstr buf off len)]
    (if (str/blank? s)
      0
      (Long/parseLong s 8))))

(defn- tar-name
  [^bytes header]
  (let [name (cstr header 0 100)]
    (if (= "ustar" (cstr header 257 5))
      (let [prefix (cstr header 345 155)]
        (if (str/blank? prefix) name (str prefix "/" name)))
      name)))

(defn- pad-512
  [size]
  (let [m (mod size 512)]
    (if (zero? m) 0 (- 512 m))))

(defn- bytes->string
  [^bytes buf]
  (String. buf java.nio.charset.StandardCharsets/ISO_8859_1))

(defn- archive
  [tar-reader]
  (or (:file tar-reader)
      (throw (ex-info "Expected a tar handle from open-tar-file" {}))))

(defn- scan
  "One pass. `wanted` is a set of member names, or nil to collect every
  regular-file name. Returns {name text} or a vector of names."
  [tar-reader wanted]
  (with-open [in (GZIPInputStream. (io/input-stream (archive tar-reader)))]
    (loop [long-name nil found {} names []]
      (if (and wanted (= (count found) (count wanted)))
        found
        (if-let [header (read-fully in 512)]
          (if (zero? (aget header 0))
            (if wanted found names)
            (let [flag (char (bit-and (aget header 156) 0xff))
                  size (octal header 124 12)
                  name (or long-name (tar-name header))]
              (cond
                (= flag \L)
                (let [raw (read-fully in size)]
                  (discard in (pad-512 size))
                  (recur (some-> raw bytes->string str/trim) found names))

                (contains? #{\0 \u0000} flag)
                (let [take? (and wanted (contains? wanted name))
                      body (when take? (read-fully in size))]
                  (when-not take? (discard in size))
                  (discard in (pad-512 size))
                  (recur nil
                         (cond-> found body (assoc name (bytes->string body)))
                         (cond-> names (nil? wanted) (conj name))))

                :else
                (do (discard in size)
                    (discard in (pad-512 size))
                    (recur nil found names)))))
          (if wanted found names))))))

(defn get-file-info
  "Regular-file member names, in archive order."
  [tar-reader]
  (vec (scan tar-reader nil)))

(defn read-files
  "One pass. `names` match member names exactly. Returns {name text} for
  the members that were present. Stops once every requested name is found."
  [tar-reader names]
  (scan tar-reader (set names)))

(defn read-file-content
  "Text of member `filename`, or nil when it is absent."
  [tar-reader filename]
  (get (read-files tar-reader [filename]) filename))
