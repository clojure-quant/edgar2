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

(def ^:private long-value
  "A non-numeric fact value that is definitely longer than fifty characters.")

(defn- long-cover
  [value]
  (str "<html><body>"
       "<ix:nonNumeric name=\"dei:DocumentDescription\">" value "</ix:nonNumeric>"
       "</body></html>"))

(def ^:private context-cover
  "A duration context with an explicit member, an instant context, and a
  fact whose context does not resolve."
  (str "<html><body>"
       "<xbrli:context id=\"duration\">"
       "<xbrli:entity>"
       "<xbrli:identifier scheme=\"http://www.sec.gov/CIK\">0000000001</xbrli:identifier>"
       "<xbrli:segment>"
       "<xbrldi:explicitMember dimension=\"us-gaap:StatementBusinessSegmentsAxis\">"
       "us-gaap:RetailMember</xbrldi:explicitMember>"
       "</xbrli:segment>"
       "</xbrli:entity>"
       "<xbrli:period><xbrli:startDate>2025-01-01</xbrli:startDate>"
       "<xbrli:endDate>2025-12-31</xbrli:endDate></xbrli:period>"
       "</xbrli:context>"
       "<xbrli:context id=\"instant\">"
       "<xbrli:period><xbrli:instant>2026-01-30</xbrli:instant></xbrli:period>"
       "</xbrli:context>"
       "<xbrli:unit id=\"usd\"><xbrli:measure>iso4217:USD</xbrli:measure></xbrli:unit>"
       "<ix:nonFraction name=\"us-gaap:Revenue\" contextRef=\"duration\""
       " unitRef=\"usd\">10</ix:nonFraction>"
       "<ix:nonFraction name=\"us-gaap:Assets\" contextRef=\"instant\""
       " unitRef=\"usd\">20</ix:nonFraction>"
       "<ix:nonFraction name=\"us-gaap:Liabilities\" contextRef=\"missing\""
       " unitRef=\"usd\">30</ix:nonFraction>"
       "</body></html>"))

(deftest fact-rows-joins-period-and-dimension
  (let [rows (panel/fact-rows [{:text context-cover :where "cover"}])
        by-concept (into {} (map (juxt :concept identity)) rows)]
    (is (= {:start "2025-01-01" :end "2025-12-31"}
           (:period (:Revenue by-concept))))
    (is (= {:us-gaap/StatementBusinessSegmentsAxis :us-gaap/RetailMember}
           (:dimension (:Revenue by-concept))))
    (is (= "0000000001" (:cik (:Revenue by-concept))))
    (is (= {:instant "2026-01-30"} (:period (:Assets by-concept))))
    (is (nil? (:cik (:Assets by-concept))))
    (is (nil? (:dimension (:Assets by-concept))))
    (is (nil? (:period (:Liabilities by-concept))))
    (is (nil? (:cik (:Liabilities by-concept))))
    (is (nil? (:dimension (:Liabilities by-concept))))))

(deftest fact-rows-keeps-every-fact-in-document-order
  (let [rows (panel/fact-rows [{:text cover :where "cover"}])]
    (is (= 4 (count rows)))
    (is (= [:DocumentType :Assets :DocumentType :EntityRegistrantName]
           (mapv :concept rows)))
    (is (= [:dei :us-gaap :dei :dei] (mapv :taxonomy rows)))
    (is (every? #(= #{:taxonomy :concept :value :period :cik :dimension}
                    (set (keys %)))
                rows))
    (is (every? #(nil? (:cik %)) rows))))

(deftest facts->taxonomies-puts-dei-first-and-skips-nil
  (is (= [:dei :srt :us-gaap]
         (panel/facts->taxonomies
          [{:taxonomy :us-gaap}
           {:taxonomy :dei}
           {:taxonomy nil}
           {:taxonomy :srt}
           {:taxonomy :dei}]))))

(deftest rows-for-taxonomy-filters-and-rejects-nil
  (let [rows [{:taxonomy :dei :concept :DocumentType :value "10-K"}
              {:taxonomy :us-gaap :concept :Assets :value "1"}]]
    (is (= [:us-gaap]
           (mapv :taxonomy (panel/rows-for-taxonomy rows :us-gaap))))
    (is (= [] (panel/rows-for-taxonomy rows nil)))))

(deftest rows-html-escapes-markup
  (let [html (panel/rows-html [{:taxonomy :dei
                                :concept :DocumentType
                                :value "<script>alert(1)</script>"}])]
    (is (str/includes? html "&lt;script&gt;alert(1)&lt;/script&gt;"))
    (is (not (str/includes? html "<script>")))))

(deftest rows-html-renders-duration-and-dimension
  (let [html (panel/rows-html [{:taxonomy :us-gaap
                                :concept :Revenue
                                :value 10
                                :period {:start "2025-01-01" :end "2025-12-31"}
                                :dimension {:us-gaap/Axis :us-gaap/Member}}])]
    (is (str/includes? html "<td class=\"period\">2025-01-01-2025-12-31</td>"))
    (is (str/includes? html "title=\"us-gaap:Axis = us-gaap:Member\""))
    (is (str/includes? html ">:us-gaap/Member</span>"))))

(deftest rows-html-uses-em-dashes-for-absent-period-and-dimension
  (let [html (panel/rows-html [{:taxonomy :dei
                                :concept :DocumentType
                                :value "10-K"}])]
    (is (= 2 (count (re-seq #"\u2014" html))))
    (is (str/includes? html "<td class=\"cik\"></td>"))
    (is (not (str/includes? html "title=")))))

(deftest rows-html-caps-and-escapes-a-long-dimension
  (let [member (keyword "b" "<script>alert(1)</script>tail")
        html (panel/rows-html [{:taxonomy :us-gaap
                                :concept :Assets
                                :value 1
                                :dimension {:a/Axis member}}])]
    (is (str/includes? html ">:b/&lt;script&gt;alert(1)&lt;</span>"))
    (is (str/includes? html "b:&lt;script&gt;alert(1)&lt;/script&gt;tail"))
    (is (not (str/includes? html "<script>")))))

(deftest rows-html-renders-a-present-cik
  (let [html (panel/rows-html [{:taxonomy :dei
                                :concept :DocumentType
                                :value "10-K"
                                :cik "0000000001"}])]
    (is (str/includes? html "<td class=\"cik\">0000000001</td>"))))

(deftest rows-html-escapes-the-cik
  (let [html (panel/rows-html [{:taxonomy :dei
                                :concept :DocumentType
                                :value "10-K"
                                :cik "<script>alert(1)</script>"}])]
    (is (str/includes? html
                       "<td class=\"cik\">&lt;script&gt;alert(1)&lt;/script&gt;</td>"))
    (is (not (str/includes? html "<script>")))))

(deftest rows-html-renders-blank-values-as-an-em-dash
  (is (str/includes? (panel/rows-html [{:taxonomy :dei
                                        :concept :EntityRegistrantName
                                        :value nil}])
                     "\u2014")))

(deftest rows-html-truncates-long-string-values
  (let [html (panel/rows-html [{:taxonomy :dei
                                :concept :DocumentDescription
                                :value long-value}])]
    (is (str/includes? html
                       (str ">" (subs long-value 0 50) "...</span>")))
    (is (str/includes? html
                       (str "<span class=\"fact-full\" hidden>"
                            long-value "</span>")))
    (is (str/includes? html "class=\"fact-more\""))
    (is (str/includes? html "aria-expanded=\"false\""))))

(deftest rows-html-truncation-happens-after-trimming
  (let [value (str "  " (apply str (repeat 51 "y")) "  ")
        html (panel/rows-html [{:taxonomy :dei
                                :concept :DocumentDescription
                                :value value}])]
    (is (str/includes? html (str ">" (apply str (repeat 50 "y")) "...</span>")))))

(deftest rows-html-keeps-values-of-fifty-characters-or-fewer
  (doseq [value [(apply str (repeat 50 "x"))
                 (apply str (repeat 49 "x"))]]
    (let [html (panel/rows-html [{:taxonomy :dei
                                  :concept :DocumentDescription
                                  :value value}])]
      (is (not (str/includes? html "fact-more")))
      (is (not (str/includes? html "...")))
      (is (str/includes? html value)))))

(deftest rows-html-does-not-truncate-non-string-values
  (let [value 999999999999999999999999999999999999999999999999999999999999N
        html (panel/rows-html [{:taxonomy :us-gaap
                                :concept :Assets
                                :value value}])]
    (is (not (str/includes? html "fact-more")))
    (is (str/includes? html (str value)))))

(deftest rows-html-escapes-both-parts-of-a-long-value
  (let [value (str "<script>alert(1)</script>" (apply str (repeat 40 "x")))
        html (panel/rows-html [{:taxonomy :dei
                                :concept :DocumentDescription
                                :value value}])]
    (is (not (str/includes? html "<script>")))
    (is (= 2 (count (re-seq #"&lt;script&gt;" html))))
    (is (str/includes? html "&lt;/script&gt;"))))

(deftest page-html-shows-period-and-dimension-cells
  (let [[dir file] (temp-file context-cover)]
    (try
      (let [html (panel/page-html file)]
        (is (str/includes? html
                           (str "<th>Taxonomy</th><th>Concept</th><th>Period</th>"
                                "<th>Value</th><th>CIK</th><th>Dimension</th>")))
        (is (str/includes? html ">2025-01-01-2025-12-31</td>"))
        (is (str/includes? html ">2026-01-30</td>"))
        (is (str/includes? html "<td class=\"cik\">0000000001</td>"))
        (is (= 1 (count (re-seq #"class=\"dimension\"" html))))
        (is (str/includes?
             html
             "title=\"us-gaap:StatementBusinessSegmentsAxis = us-gaap:RetailMember\""))
        (is (str/includes? html ">:us-gaap/RetailMembe</span>")))
      (finally (rm-r dir)))))

(deftest page-html-defaults-to-dei-and-lists-every-taxonomy
  (let [[dir file] (temp-file cover)]
    (try
      (let [html (panel/page-html file)]
        (is (str/includes? html "<select"))
        (is (str/includes? html "<option value=\"dei\" selected>dei</option>"))
        (is (str/includes? html "<option value=\"us-gaap\">us-gaap</option>"))
        (is (str/includes? html "XBRL facts (3)"))
        (is (str/includes? html "DocumentType"))
        (is (not (str/includes? html "Assets"))))
      (finally (rm-r dir)))))

(deftest page-html-includes-the-value-popup
  (let [[dir file] (temp-file (long-cover long-value))]
    (try
      (let [html (panel/page-html file)]
        (is (str/includes? html "popup.id='edgar2-fact-popup'"))
        (is (str/includes? html "popup.className='fact-popup'"))
        (is (str/includes? html ".fact-popup{"))
        (is (str/includes? html ".fact-more{"))
        (is (str/includes? html "class=\"fact-more\""))
        (is (str/includes? html (str (subs long-value 0 50) "..."))))
      (finally (rm-r dir)))))

(deftest page-html-filters-to-the-requested-taxonomy
  (let [[dir file] (temp-file cover)]
    (try
      (let [html (panel/page-html file "us-gaap")]
        (is (str/includes? html "<option value=\"us-gaap\" selected>us-gaap</option>"))
        (is (str/includes? html "XBRL facts (1)"))
        (is (str/includes? html "Assets"))
        (is (not (str/includes? html "DocumentType"))))
      (finally (rm-r dir)))))

(deftest page-html-falls-back-from-an-unknown-taxonomy
  (let [[dir file] (temp-file cover)]
    (try
      (let [html (panel/page-html file "nope")]
        (is (str/includes? html "<option value=\"dei\" selected>dei</option>"))
        (is (str/includes? html "XBRL facts (3)"))
        (is (str/includes? html "DocumentType")))
      (finally (rm-r dir)))))

(deftest page-html-shows-the-empty-message
  (let [[dir file] (temp-file "<html><body><p>plain</p></body></html>")]
    (try
      (let [html (panel/page-html file)]
        (is (str/includes? html "XBRL facts (0)"))
        (is (str/includes? html "No XBRL facts in this document."))
        (is (not (str/includes? html "<select"))))
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
      (with-redefs [panel/fact-rows (fn [_]
                                      (swap! calls inc)
                                      [])]
        (panel/page-html file)
        (panel/page-html file "us-gaap")
        (is (= 1 @calls)))
      (finally (rm-r dir)))))
