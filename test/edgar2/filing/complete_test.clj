(ns edgar2.filing.complete-test
  (:require [clojure.test :refer [deftest is]]
            [edgar2.filing.complete :as complete]))

(def ^:private cover "<XML>cover</XML>")
(def ^:private table "<XML>table</XML>")

(def ^:private expected-header
  {:cik 1315339
   :name "First Financial Bank - Trust Division"
   :filing-date "2026-01-02"
   :form "13F-HR"
   :accession "0001315339-26-000001"})

(def ^:private expected-files
  [{:seq 1 :filename "primary_doc.xml" :type "13F-HR"}
   {:seq 2 :filename "form13fInfoTable.xml" :type "INFORMATION TABLE"}])

(defn- doc
  [type seq filename body]
  (str "<DOCUMENT>\n<TYPE>" type
       "\n<SEQUENCE>" seq
       "\n<FILENAME>" filename
       "\n<TEXT>" body "</TEXT>\n</DOCUMENT>\n"))

(def ^:private documents
  (str (doc "13F-HR" "1" "primary_doc.xml" cover)
       (doc "INFORMATION TABLE" "2" "form13fInfoTable.xml" table)))

(def ^:private txt
  (str "<SEC-DOCUMENT>0001315339-26-000001.txt : 20260102\n"
       "<SEC-HEADER>0001315339-26-000001.hdr.sgml : 20260102\n"
       "<ACCEPTANCE-DATETIME>20260102145133\n"
       "ACCESSION NUMBER:\t\t0001315339-26-000001\n"
       "CONFORMED SUBMISSION TYPE:\t13F-HR\n"
       "PUBLIC DOCUMENT COUNT:\t\t2\n"
       "CONFORMED PERIOD OF REPORT:\t20251231\n"
       "FILED AS OF DATE:\t\t20260102\n"
       "FILER:\n"
       "\tCOMPANY DATA:\n"
       "\t\tCOMPANY CONFORMED NAME:\t\t\tFirst Financial Bank - Trust Division\n"
       "\t\tCENTRAL INDEX KEY:\t\t\t0001315339\n"
       "</SEC-HEADER>\n"
       documents))

(def ^:private nc
  (str "<SUBMISSION>\r\n"
       "<ACCESSION-NUMBER>0001315339-26-000001\r\n"
       "<FORM-TYPE>NOPE\r\n"
       "<TYPE>13F-HR\r\n"
       "<PUBLIC-DOCUMENT-COUNT>2\r\n"
       "<PERIOD>20251231\r\n"
       "<FILING-DATE>20260102\r\n"
       "<FILER>\r\n"
       "<COMPANY-DATA>\r\n"
       "<CONFORMED-NAME>First Financial Bank - Trust Division\r\n"
       "<CIK>0001315339\r\n"
       "</COMPANY-DATA>\r\n"
       "</FILER>\r\n"
       "<DOCUMENT>\r\n"
       "<TYPE>13F-HR\r\n"
       "<SEQUENCE>1\r\n"
       "<FILENAME>primary_doc.xml\r\n"
       "<TEXT>" cover "</TEXT>\r\n"
       "</DOCUMENT>\r\n"
       "<DOCUMENT>\r\n"
       "<TYPE>INFORMATION TABLE\r\n"
       "<SEQUENCE>2\r\n"
       "<FILENAME>form13fInfoTable.xml\r\n"
       "<TEXT>" table "</TEXT>\r\n"
       "</DOCUMENT>\r\n"
       "</SUBMISSION>\r\n"))

(defn- table-of
  [structure]
  (mapv #(select-keys % [:seq :filename :type]) (:files structure)))

(deftest txt-and-nc-share-header-and-files
  (let [from-txt (complete/file-structure txt)
        from-nc (complete/file-structure nc)]
    (is (= expected-header (:header from-txt)))
    (is (= expected-header (:header from-nc)))
    (is (= expected-files (table-of from-txt)))
    (is (= expected-files (table-of from-nc)))
    (is (every? #(not (contains? % :content)) (:files from-txt)))
    (is (= cover (complete/load-file from-txt ".xml" "13F-HR")))
    (is (= table (complete/load-file from-nc ".xml" "INFORMATION TABLE")))
    (is (nil? (complete/load-file from-txt ".html" "13F-HR")))
    (is (nil? (complete/load-file from-txt ".xml" "13F-HR/A")))))

(deftest html-and-xml-of-the-same-type-stay-separate
  (let [text (str "<CIK>0001557017</CIK>\n"
                  "<FILING-DATE>20250213</FILING-DATE>\n"
                  (doc "13F-HR" "1" "primary_doc.html" "<html>hi</html>")
                  (doc "13F-HR" "1" "primary_doc.XML" cover))
        structure (complete/file-structure text)]
    (is (= 1557017 (:cik (:header structure))))
    (is (= "2025-02-13" (:filing-date (:header structure))))
    (is (= [{:seq 1 :filename "primary_doc.html" :type "13F-HR"}
            {:seq 1 :filename "primary_doc.XML" :type "13F-HR"}]
           (table-of structure)))
    (is (= cover (complete/load-file structure ".xml" "13F-HR")))
    (is (= "<html>hi</html>" (complete/load-file structure ".html" "13F-HR")))))
