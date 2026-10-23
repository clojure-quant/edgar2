(ns edgar2.filing.day-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [edgar2.filing.day :as day])
  (:import [java.util.zip GZIPOutputStream]))

(defn- put-bytes
  [^bytes dest off ^bytes src]
  (System/arraycopy src 0 dest off (alength src)))

(defn- member-header
  [name size]
  (let [buf (byte-array 512)
        ascii #(.getBytes ^String % "US-ASCII")]
    (put-bytes buf 0 (ascii name))
    (put-bytes buf 100 (ascii "0000644\0"))
    (put-bytes buf 124 (ascii (format "%011o\0" size)))
    (aset buf 156 (unchecked-byte (int \0)))
    (put-bytes buf 257 (ascii "ustar\0"))
    (put-bytes buf 263 (ascii "00"))
    buf))

(defn- write-tar-gz!
  [path members]
  (with-open [out (GZIPOutputStream. (io/output-stream path))]
    (doseq [[name text] members]
      (let [body (.getBytes ^String text "ISO-8859-1")
            n (alength body)
            pad-n (let [m (mod n 512)] (if (zero? m) 0 (- 512 m)))]
        (.write out (member-header name n))
        (.write out body)
        (.write out (byte-array pad-n))))
    (.write out (byte-array 1024))))

(deftest read-accessions-uses-dot-slash-member-name
  (let [dir (io/file (System/getProperty "java.io.tmpdir") "edgar2-day-test")
        path (io/file dir "20260102.nc.tar.gz")
        acc "0001315339-26-000001"]
    (.mkdirs dir)
    (write-tar-gz! path [[(str "./" acc ".nc") "<SUBMISSION>one"]
                         ["./0000000000-26-000002.nc" "<SUBMISSION>two"]])
    (is (= {acc "<SUBMISSION>one"}
           (day/read-accessions path [acc "0000000099-26-000009"])))
    (is (nil? (day/archive-file "1999-01-01")))))
