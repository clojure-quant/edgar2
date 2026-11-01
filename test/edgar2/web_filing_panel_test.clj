(ns edgar2.web-filing-panel-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [edgar2.filing.xbrl-loader :as xbrl-loader]
            [edgar2.web.filing-panel :as panel]))

(def cover
  "<html><body>
   <ix:nonNumeric name=\"dei:DocumentType\">10-K</ix:nonNumeric>
   <ix:nonNumeric name=\"us-gaap:Assets\">1</ix:nonNumeric>
   <ix:nonNumeric name=\"dei:DocumentType\">10-K</ix:nonNumeric>
   <ix:nonNumeric name=\"dei:EntityRegistrantName\"></ix:nonNumeric>
   </body></html>")

(defn- rm-r
  [^java.io.File f]
  (when (.isDirectory f)
    (doseq [c (.listFiles f)]
      (rm-r c)))
  (.delete f))

(defn- temp-file
  [content]
  (let [dir (io/file (System/getProperty "java.io.tmpdir")
                     (str "edgar2-panel-" (System/nanoTime)))
        file (io/file dir "doc.htm")]
    (.mkdirs dir)
    (spit file content)
    [dir file]))

(deftest dei-rows-keeps-dei-facts-in-document-order
  (let [rows (panel/dei-rows [{:text cover :where "cover"}])]
    (is (= 3 (count rows)))
    (is (= [:DocumentType :DocumentType :EntityRegistrantName]
           (mapv :concept rows)))
    (is (every? #(= #{:taxonomy :concept :value} (set (keys %))) rows))
    (is (every? #(= :dei (:taxonomy %)) rows))))

(deftest rows-html-escapes-markup
  (let [html (panel/rows-html [{:taxonomy :dei
                                :concept :DocumentType
                                :value "<script>alert(1)</script>"}])]
    (is (str/includes? html "&lt;script&gt;alert(1)&lt;/script&gt;"))
    (is (not (str/includes? html "<script>")))))

(deftest rows-html-renders-blank-values-as-an-em-dash
  (is (str/includes? (panel/rows-html [{:taxonomy :dei
                                        :concept :EntityRegistrantName
                                        :value nil}])
                     "\u2014")))

(deftest page-html-shows-the-empty-message
  (let [[dir file] (temp-file "<html><body><p>plain</p></body></html>")]
    (try
      (let [html (panel/page-html file)]
        (is (str/includes? html "XBRL DEI facts (0)"))
        (is (str/includes? html "No DEI facts in this document.")))
      (finally (rm-r dir)))))

(deftest page-html-reports-a-parse-failure
  (let [[dir file] (temp-file "<html><body></body></html>")]
    (try
      (with-redefs [xbrl-loader/load-documents
                    (fn [_] (throw (ex-info "boom <bad>" {})))]
        (let [html (panel/page-html file)]
          (is (str/includes? html "Could not read XBRL facts: boom &lt;bad&gt;"))
          (is (not (str/includes? html "boom <bad>")))))
      (finally (rm-r dir)))))

(deftest rows-are-cached-by-path-mtime-and-length
  (let [[dir file] (temp-file "<html><body></body></html>")
        calls (atom 0)]
    (try
      (with-redefs [panel/dei-rows (fn [_]
                                     (swap! calls inc)
                                     [])]
        (panel/page-html file)
        (panel/page-html file)
        (is (= 1 @calls)))
      (finally (rm-r dir)))))
