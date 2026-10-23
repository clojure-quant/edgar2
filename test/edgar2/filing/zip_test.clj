(ns edgar2.filing.zip-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [edgar2.filing.zip :as zip])
  (:import [java.util.zip ZipEntry ZipOutputStream]))

(defn- write-zip!
  [path entries]
  (with-open [out (ZipOutputStream. (io/output-stream path))]
    (doseq [[name text] entries]
      (.putNextEntry out (ZipEntry. name))
      (.write out (.getBytes ^String text "UTF-8"))
      (.closeEntry out))))

(deftest lists-and-reads-entries
  (let [dir (io/file (System/getProperty "java.io.tmpdir") "edgar2-zip-test")
        path (io/file dir "sample.zip")]
    (.mkdirs dir)
    (write-zip! path [["b.txt" "second"] ["a.txt" "first"]])
    (with-open [z (zip/open-zip-file path)]
      (is (= ["a.txt" "b.txt"] (zip/get-file-info z)))
      (is (= "first" (zip/read-file-content z "a.txt")))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"Zip entry not found"
                            (zip/read-file-content z "missing.txt"))))))
