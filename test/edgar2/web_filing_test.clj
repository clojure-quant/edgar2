(ns edgar2.web-filing-test
  "Agent-filed documents are stored under the issuer CIK. The accession
  prefix is the submitter (0001193125 is Donnelley). A 503 on that index
  must not hide a filing the issuer path can serve."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [edgar.filing :as ef]
            [edgar2.filing :as filing]
            [edgar2.web.filing :as web]))

(def aiv-cik "0000922864")
(def aiv-accession "0001193125-26-338545")
(def aiv-doc "aiv-20260630.htm")

(defn- index-for
  [form doc]
  {:formType form
   :filingDate "2026-08-07"
   :files [{:sequence "1" :name doc :type form :description form :size "1"}]})

(defn- http-ex
  [status]
  (ex-info (str "HTTP " status " from SEC API")
           {:type :edgar.core/http-error :status status}))

(defn- rm-r
  [^java.io.File f]
  (when (.isDirectory f)
    (doseq [c (.listFiles f)]
      (rm-r c)))
  (.delete f))

(deftest issuer-index-is-tried-before-submitter
  (let [calls (atom [])]
    (with-redefs [ef/filing-index
                  (fn [f]
                    (swap! calls conj (:cik f))
                    (if (= "0001193125" (:cik f))
                      (throw (http-ex 503))
                      (index-for "10-Q" aiv-doc)))]
      (let [found (#'web/load-filing aiv-cik aiv-accession)]
        (is (= [aiv-cik] @calls))
        (is (= {:cik aiv-cik
                :accessionNumber aiv-accession
                :form "10-Q"
                :primaryDocument aiv-doc
                :filingDate "2026-08-07"}
               found))))))

(deftest submitter-index-is-the-fallback
  (let [calls (atom [])]
    (with-redefs [ef/filing-index
                  (fn [f]
                    (swap! calls conj (:cik f))
                    (if (= aiv-cik (:cik f))
                      (throw (http-ex 404))
                      (index-for "10-Q" aiv-doc)))]
      (let [found (#'web/load-filing aiv-cik aiv-accession)]
        (is (= [aiv-cik "0001193125"] @calls))
        (is (= "0001193125" (:cik found)))
        (is (= aiv-doc (:primaryDocument found)))))))

(deftest issuer-503-still-uses-submitter-index
  (with-redefs [ef/filing-index
                (fn [f]
                  (if (= aiv-cik (:cik f))
                    (throw (http-ex 503))
                    (index-for "10-Q" aiv-doc)))]
    (is (= "10-Q" (:form (#'web/load-filing aiv-cik aiv-accession))))))

(deftest both-indexes-503-is-not-a-missing-filing
  (with-redefs [ef/filing-index (fn [_] (throw (http-ex 503)))]
    (let [e (try
              (#'web/load-filing aiv-cik aiv-accession)
              (catch clojure.lang.ExceptionInfo e e))]
      (is (instance? clojure.lang.ExceptionInfo e))
      (is (= 503 (:status (ex-data e)))))))

(deftest both-indexes-missing-returns-nil
  (with-redefs [ef/filing-index (fn [_] (throw (http-ex 404)))]
    (is (nil? (#'web/load-filing aiv-cik aiv-accession)))))

(deftest same-cik-looks-up-once
  (let [calls (atom 0)]
    (with-redefs [ef/filing-index
                  (fn [_]
                    (swap! calls inc)
                    (index-for "10-K" "a.htm"))]
      (#'web/load-filing "0000320193" "0000320193-23-000106")
      (is (= 1 @calls)))))

(deftest cached-document-skips-sec
  (let [dir (io/file (System/getProperty "java.io.tmpdir")
                     (str "edgar2-filing-" (System/nanoTime)))
        file (io/file dir "10-Q" aiv-cik aiv-accession aiv-doc)
        calls (atom 0)]
    (.mkdirs (.getParentFile file))
    (spit file "ok")
    (try
      (with-redefs [filing/filing-dir (.getPath dir)
                    ef/filing-index (fn [_]
                                      (swap! calls inc)
                                      (throw (http-ex 503)))]
        (is (= file (web/filing-doc-file aiv-cik aiv-accession aiv-doc)))
        (is (zero? @calls)))
      (finally (rm-r dir)))))
