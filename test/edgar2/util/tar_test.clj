(ns edgar2.util.tar-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [edgar2.util.tar :as tar])
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

(deftest lists-and-reads-members
  (let [dir (io/file (System/getProperty "java.io.tmpdir") "edgar2-tar-test")
        path (io/file dir "sample.tar.gz")
        reader (delay (tar/open-tar-file path))]
    (.mkdirs dir)
    (write-tar-gz! path [["a.txt" "alpha"] ["b.txt" "beta"] ["c.txt" "gamma"]])
    (let [t @reader]
      (is (= ["a.txt" "b.txt" "c.txt"] (tar/get-file-info t)))
      (is (= "beta" (tar/read-file-content t "b.txt")))
      (is (nil? (tar/read-file-content t "missing.txt")))
      (is (= {"a.txt" "alpha" "c.txt" "gamma"}
             (tar/read-files t ["c.txt" "a.txt" "nope.txt"]))))))
