(ns edgar2.filing.quarterly-index-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [edgar2.filing.quarterly-index :as qi])
  (:import [java.util.zip ZipEntry ZipOutputStream]))

(def master-idx
  (str "Description: Master Index of EDGAR Dissemination Feed\n"
       "CIK|Company Name|Form Type|Date Filed|Filename\n"
       "--------------------------------------------------------------------------------\n"
       "1315339|FIRST FINANCIAL|13F-HR|2026-01-02|edgar/data/1315339/0001315339-26-000001.txt\n"
       "1000045|OTHER CO|10-K|2026-01-05|edgar/data/1000045/0001000045-26-000001.txt\n"
       "0001315339|FIRST FINANCIAL|13F-HR/A|2026-02-02|edgar/data/1315339/0001315339-26-000099.txt\n"))

(defn- write-zip!
  [path]
  (with-open [out (ZipOutputStream. (io/output-stream path))]
    (.putNextEntry out (ZipEntry. "master.idx"))
    (.write out (.getBytes master-idx "UTF-8"))
    (.closeEntry out)))

(defn- delete-recursively
  [f]
  (when (.exists f)
    (when (.isDirectory f)
      (doseq [child (.listFiles f)]
        (delete-recursively child)))
    (.delete f)))

(deftest queries-master-idx
  (let [root (io/file (System/getProperty "java.io.tmpdir") "edgar2-qidx-test")
        zip (io/file root "2026" "QTR1" "master.zip")
        older (io/file root "2025" "QTR4" "master.zip")
        quarter {:year 2026 :quarter 1}]
    (delete-recursively root)
    (.mkdirs (.getParentFile zip))
    (.mkdirs (.getParentFile older))
    (write-zip! zip)
    (write-zip! older)
    (binding [qi/*index-dir* (str root)]
      (is (= [{:year 2025 :quarter 4}
              {:year 2026 :quarter 1}]
             (qi/on-disk {})))
      (is (= [{:year 2026 :quarter 1}] (qi/on-disk {:year 2026})))
      (is (= [{:year 2025 :quarter 4}] (qi/on-disk {:year 2025})))
      (is (= 3 (count (qi/all quarter))))
      (is (= ["13F-HR"] (map :form (qi/query-form quarter "13F-HR"))))
      (is (= ["13F-HR" "13F-HR/A"]
             (map :form (qi/query-cik quarter 1315339))))
      (is (= ["13F-HR" "13F-HR/A"]
             (map :form (qi/query-cik quarter "0001315339"))))
      (is (= {:cik "1315339"
              :company-name "FIRST FINANCIAL"
              :form "13F-HR"
              :date-filed "2026-01-02"
              :filename "edgar/data/1315339/0001315339-26-000001.txt"}
             (first (qi/query-form quarter "13F-HR")))))))
