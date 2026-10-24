(ns edgar2.institutional-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [edgar2.institutional :as institutional]))

(def ^:private apple "037833100")
(def ^:private other "594918104")

(defn- submission
  [cusip-rows]
  (str "<SUBMISSION>\n"
       "<CIK>0001557017</CIK>\n"
       "<FILING-DATE>20250213</FILING-DATE>\n"
       "<DOCUMENT>\n<TYPE>13F-HR\n<SEQUENCE>1\n<FILENAME>primary_doc.xml\n<TEXT>\n<XML>\n"
       "<edgarSubmission><formData><coverPage>"
       "<periodOfReport>12-31-2024</periodOfReport>"
       "<reportType>13F HOLDINGS REPORT</reportType>"
       "<form13FFileNumber>028-17203</form13FFileNumber>"
       "<filingManager><name>Capula Management Ltd</name></filingManager>"
       "<otherManagersInfo><otherManager>"
       "<cik>0001592828</cik><form13FFileNumber>028-17729</form13FFileNumber>"
       "<name>Empowered Funds, LLC</name>"
       "</otherManager></otherManagersInfo>"
       "</coverPage><summaryPage><otherManagers2Info>"
       "<otherManager2><sequenceNumber>1</sequenceNumber><otherManager>"
       "<cik>0001633019</cik><form13FFileNumber>028-17204</form13FFileNumber>"
       "<name>Capula Investment US LP</name></otherManager></otherManager2>"
       "<otherManager2><sequenceNumber>2</sequenceNumber><otherManager>"
       "<cik>0001351450</cik><form13FFileNumber>028-17757</form13FFileNumber>"
       "<name>Capula Investment Management LLP</name></otherManager></otherManager2>"
       "</otherManagers2Info></summaryPage></formData></edgarSubmission>"
       "\n</XML>\n</TEXT>\n</DOCUMENT>\n"
       "<DOCUMENT>\n<TYPE>INFORMATION TABLE\n<SEQUENCE>2\n<FILENAME>form13fInfoTable.xml\n<TEXT>\n<XML>\n<informationTable>"
       (str/join "" cusip-rows)
       "</informationTable>\n</XML>\n</TEXT>\n</DOCUMENT>\n</SUBMISSION>"))

(defn- row
  [issuer cusip value shares discretion other]
  (str "<infoTable><nameOfIssuer>" issuer "</nameOfIssuer>"
       "<titleOfClass>COM</titleOfClass><cusip>" cusip "</cusip>"
       "<value>" value "</value><shrsOrPrnAmt><sshPrnamt>" shares "</sshPrnamt>"
       "<sshPrnamtType>SH</sshPrnamtType></shrsOrPrnAmt>"
       "<investmentDiscretion>" discretion "</investmentDiscretion>"
       (when other (str "<otherManager>" other "</otherManager>"))
       "<votingAuthority><Sole>" shares "</Sole><Shared>0</Shared><None>0</None></votingAuthority>"
       "</infoTable>"))

(defn- line
  [qty value managers]
  {:cusip apple
   :issuer "APPLE INC"
   :class "COM"
   :qty qty
   :value value
   :discretion (if (seq managers) "DFND" "SOLE")
   :other-managers managers})

(defn- filing
  [cik name holdings included]
  {:filer cik
   :filer-name name
   :filing-date "2025-02-13"
   :report-period "2024-12-31"
   :file-number (str "028-" cik)
   :included-managers included
   :holdings holdings})

(deftest column-7-stays-on-the-filing-manager
  (let [parsed (institutional/get-holdings
                {:submission (submission [(row "APPLE INC" apple 10 100 "SOLE" nil)
                                          (row "APPLE INC" apple 25 50 "DFND" "1")
                                          (row "MICROSOFT CORP" other 7 7 "OTR" "2,1")])})
        by-key (group-by (juxt :cusip :other-managers) (:holdings parsed))]
    (is (= 1557017 (:filer parsed)))
    (is (= "2024-12-31" (:report-period parsed)))
    (is (= "028-17203" (:file-number parsed)))
    (is (= {:cik 1633019
            :name "Capula Investment US LP"
            :file-number "028-17204"}
           (get (:included-managers parsed) 1)))
    (is (= [{:cik 1592828 :name "Empowered Funds, LLC" :file-number "028-17729"}]
           (:reported-by parsed)))
    (is (= 100 (:qty (first (get by-key [apple []])))))
    (is (= 50 (:qty (first (get by-key [apple [1]])))))
    (is (= [1 2] (:other-managers (first (get by-key [other [1 2]])))))
    (is (= 7 (:qty (first (get by-key [other [1 2]])))))))

(deftest prune-drops-only-the-overlap
  (let [reporter (filing 1557017 "Capula Management Ltd"
                         [(line 100 10 [])
                          (line 50 25 [1])]
                         {1 {:cik 1633019
                             :name "Capula Investment US LP"
                             :file-number "028-17204"}})
        included (filing 1633019 "Capula Investment US LP"
                         [(line 40 20 [])]
                         {})
        alone (filing 1557017 "Capula Management Ltd"
                      [(line 50 25 [1])]
                      {1 {:cik 1633019 :name "Capula Investment US LP" :file-number "028-1"}})
        {:keys [filers several-filers cusips-reduced value-removed]}
        (institutional/prune-holdings [reporter included])]
    (is (= 1 several-filers))
    (is (= 1 cusips-reduced))
    (is (= 20 value-removed))
    (is (= 110 (:qty (first (:holdings (first (filter #(= 1557017 (:filer %)) filers)))))))
    (is (= 15 (:value (first (:holdings (first (filter #(= 1557017 (:filer %)) filers)))))))
    (is (= 40 (:qty (first (:holdings (first (filter #(= 1633019 (:filer %)) filers)))))))
    (let [kept (institutional/prune-holdings [alone])]
      (is (zero? (:cusips-reduced kept)))
      (is (zero? (:value-removed kept)))
      (is (= 50 (:qty (first (:holdings (first (:filers kept))))))))))

(deftest two-lines-on-one-filing-are-one-claim
  (let [a (filing 1 "A"
                  [(line 30 30 [1])
                   (assoc (line 20 20 [1]) :discretion "OTR")]
                  {1 {:cik 20 :name "B" :file-number "028-20"}})
        b (filing 20 "B" [(line 40 40 [])] {})
        result (institutional/prune-holdings [a b])
        book (first (filter #(= 1 (:filer %)) (:filers result)))]
    (is (= 10 (:qty (first (:holdings book)))))
    (is (= 10 (:value (first (:holdings book)))))
    (is (= 40 (:value-removed result)))
    (is (= 1 (:cusips-reduced result)))))

(deftest prune-keeps-the-largest-claim
  (let [manager {1 {:cik 20 :name "B" :file-number "028-20"}}
        a (filing 1 "A" [(line 50 25 [1])] manager)
        c (filing 3 "C" [(line 30 9 [1])] manager)
        b (filing 20 "B" [(line 40 20 [])] {})
        {:keys [filers cusips-reduced value-removed]}
        (institutional/prune-holdings [a c b])
        qty (fn [cik]
              (:qty (first (:holdings (first (filter #(= cik (:filer %)) filers))))))]
    (is (= 1 cusips-reduced))
    (is (= 29 value-removed))
    (is (= 10 (qty 1)))
    (is (nil? (qty 3)))
    (is (= 40 (qty 20)))))
