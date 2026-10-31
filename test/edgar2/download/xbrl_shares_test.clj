(ns edgar2.download.xbrl-shares-test
  (:require [clojure.test :refer [deftest is]]
            [edgar2.download.xbrl-shares :as shares]
            [edgar2.filing.xbrl :as xbrl]))

(defn- shares-fact
  [val]
  {:units {:shares [{:end "2025-12-31" :val val :filed "2026-03-10"}]}})

(deftest issued-basic-is-weighted-when-outstanding-is-absent
  (is (= {"Common" {:weighted 26038125 :outstanding nil}}
         (shares/share-summary
          {:facts {:us-gaap {:WeightedAverageNumberOfSharesIssuedBasic
                             (shares-fact 26038125)}}}))))

(deftest cover-outstanding-uses-instance-context
  (let [cover "<html><body>
               <ix:nonFraction name=\"dei:EntityCommonStockSharesOutstanding\" contextRef=\"C_20260430\" unitRef=\"Share\" decimals=\"INF\">351,018,130</ix:nonFraction>
               </body></html>"
        instance "<xbrl>
                  <context id=\"C_20260430\"><period><instant>2026-04-30</instant></period></context>
                  <context id=\"C_20250501to20260430\"><period><startDate>2025-05-01</startDate><endDate>2026-04-30</endDate></period></context>
                  <context id=\"C_20240501to20250430\"><period><startDate>2024-05-01</startDate><endDate>2025-04-30</endDate></period></context>
                  <unit id=\"Share\"><measure>shares</measure></unit>
                  <ifrs-full:WeightedAverageShares contextRef=\"C_20250501to20260430\" unitRef=\"Share\" decimals=\"INF\">338886497</ifrs-full:WeightedAverageShares>
                  <ifrs-full:WeightedAverageShares contextRef=\"C_20240501to20250430\" unitRef=\"Share\" decimals=\"INF\">267326705</ifrs-full:WeightedAverageShares>
                  </xbrl>"]
    (is (= {"Common" {:weighted 338886497, :outstanding 351018130}}
           (shares/share-summary (xbrl/parse-all [cover instance]))))))

(deftest outstanding-basic-wins-when-issued-is-also-present
  (is (= {"Common" {:weighted 100 :outstanding nil}}
         (shares/share-summary
          {:facts {:us-gaap {:WeightedAverageNumberOfSharesOutstandingBasic
                             (shares-fact 100)
                             :WeightedAverageNumberOfSharesIssuedBasic
                             (shares-fact 26038125)}}}))))

(deftest shares-outstanding-fills-when-dei-has-no-facts
  (is (= {"Common" {:weighted nil :outstanding 379236005}}
         (shares/share-summary
          {:facts {:dei {:EntityCommonStockSharesOutstanding {:units {}}}
                   :us-gaap {:SharesOutstanding (shares-fact 379236005)}}}))))

(def tap-cover
  "<html><body>
   <ix:nonNumeric name=\"dei:EntityCentralIndexKey\">0000024545</ix:nonNumeric>
   <ix:nonNumeric name=\"dei:DocumentType\">10-K</ix:nonNumeric>
   <ix:nonNumeric name=\"dei:DocumentFiscalYearFocus\">2025</ix:nonNumeric>
   <ix:nonNumeric name=\"dei:DocumentFiscalPeriodFocus\">FY</ix:nonNumeric>
   <xbrli:context id=\"c-1\">
     <xbrli:entity><xbrli:identifier scheme=\"http://www.sec.gov/CIK\">0000024545</xbrli:identifier></xbrli:entity>
     <xbrli:period><xbrli:startDate>2025-01-01</xbrli:startDate><xbrli:endDate>2025-12-31</xbrli:endDate></xbrli:period>
   </xbrli:context>
   <xbrli:context id=\"c-3\">
     <xbrli:entity><xbrli:identifier scheme=\"http://www.sec.gov/CIK\">0000024545</xbrli:identifier>
       <xbrli:segment>
         <xbrldi:explicitMember dimension=\"us-gaap:StatementClassOfStockAxis\">us-gaap:CommonClassAMember</xbrldi:explicitMember>
         <xbrldi:explicitMember dimension=\"dei:EntityListingsExchangeAxis\">exch:XNYS</xbrldi:explicitMember>
       </xbrli:segment>
     </xbrli:entity>
     <xbrli:period><xbrli:startDate>2025-01-01</xbrli:startDate><xbrli:endDate>2025-12-31</xbrli:endDate></xbrli:period>
   </xbrli:context>
   <xbrli:context id=\"c-4\">
     <xbrli:entity><xbrli:identifier scheme=\"http://www.sec.gov/CIK\">0000024545</xbrli:identifier>
       <xbrli:segment>
         <xbrldi:explicitMember dimension=\"us-gaap:StatementClassOfStockAxis\">us-gaap:CommonClassBMember</xbrldi:explicitMember>
         <xbrldi:explicitMember dimension=\"dei:EntityListingsExchangeAxis\">exch:XNYS</xbrldi:explicitMember>
       </xbrli:segment>
     </xbrli:entity>
     <xbrli:period><xbrli:startDate>2025-01-01</xbrli:startDate><xbrli:endDate>2025-12-31</xbrli:endDate></xbrli:period>
   </xbrli:context>
   <xbrli:context id=\"c-5\">
     <xbrli:entity><xbrli:identifier scheme=\"http://www.sec.gov/CIK\">0000024545</xbrli:identifier>
       <xbrli:segment>
         <xbrldi:explicitMember dimension=\"us-gaap:DebtInstrumentAxis\">tap:A3.800SeniorNotesDue2032Member</xbrldi:explicitMember>
         <xbrldi:explicitMember dimension=\"dei:EntityListingsExchangeAxis\">exch:XNYS</xbrldi:explicitMember>
       </xbrli:segment>
     </xbrli:entity>
     <xbrli:period><xbrli:startDate>2025-01-01</xbrli:startDate><xbrli:endDate>2025-12-31</xbrli:endDate></xbrli:period>
   </xbrli:context>
   <xbrli:context id=\"c-7\">
     <xbrli:entity><xbrli:identifier scheme=\"http://www.sec.gov/CIK\">0000024545</xbrli:identifier>
       <xbrli:segment>
         <xbrldi:explicitMember dimension=\"us-gaap:StatementClassOfStockAxis\">us-gaap:CommonClassAMember</xbrldi:explicitMember>
       </xbrli:segment>
     </xbrli:entity>
     <xbrli:period><xbrli:instant>2026-02-11</xbrli:instant></xbrli:period>
   </xbrli:context>
   <xbrli:context id=\"c-8\">
     <xbrli:entity><xbrli:identifier scheme=\"http://www.sec.gov/CIK\">0000024545</xbrli:identifier>
       <xbrli:segment>
         <xbrldi:explicitMember dimension=\"us-gaap:StatementClassOfStockAxis\">us-gaap:CommonClassBMember</xbrldi:explicitMember>
       </xbrli:segment>
     </xbrli:entity>
     <xbrli:period><xbrli:instant>2026-02-11</xbrli:instant></xbrli:period>
   </xbrli:context>
   <xbrli:unit id=\"shares\"><xbrli:measure>xbrli:shares</xbrli:measure></xbrli:unit>
   <ix:nonNumeric contextRef=\"c-3\" name=\"dei:Security12bTitle\">Class A Common Stock, $0.01 par value</ix:nonNumeric>
   <ix:nonNumeric contextRef=\"c-3\" name=\"dei:TradingSymbol\">TAP.A</ix:nonNumeric>
   <ix:nonNumeric contextRef=\"c-3\" name=\"dei:SecurityExchangeName\">New York Stock Exchange</ix:nonNumeric>
   <ix:nonNumeric contextRef=\"c-4\" name=\"dei:Security12bTitle\">Class B Common Stock, $0.01 par value</ix:nonNumeric>
   <ix:nonNumeric contextRef=\"c-4\" name=\"dei:TradingSymbol\">TAP</ix:nonNumeric>
   <ix:nonNumeric contextRef=\"c-4\" name=\"dei:SecurityExchangeName\">New York Stock Exchange</ix:nonNumeric>
   <ix:nonNumeric contextRef=\"c-5\" name=\"dei:Security12bTitle\">3.800% Senior Notes due 2032</ix:nonNumeric>
   <ix:nonNumeric contextRef=\"c-5\" name=\"dei:TradingSymbol\">TAP 32</ix:nonNumeric>
   <ix:nonNumeric contextRef=\"c-5\" name=\"dei:SecurityExchangeName\">New York Stock Exchange</ix:nonNumeric>
   <ix:nonFraction name=\"us-gaap:WeightedAverageNumberOfSharesOutstandingBasic\" contextRef=\"c-1\" unitRef=\"shares\" scale=\"0\">199100000</ix:nonFraction>
   <ix:nonFraction name=\"dei:EntityCommonStockSharesOutstanding\" contextRef=\"c-7\" unitRef=\"shares\" scale=\"0\">2,563,034</ix:nonFraction>
   <ix:nonFraction name=\"dei:EntityCommonStockSharesOutstanding\" contextRef=\"c-8\" unitRef=\"shares\" scale=\"0\">175,592,622</ix:nonFraction>
   </body></html>")

(deftest cover-lists-ticker-and-exchange-for-each-share-class
  (is (= {"Common" {:weighted 199100000, :outstanding nil}
          "Class A" {:weighted nil
                     :outstanding 2563034
                     :ticker "TAP.A"
                     :exchange "New York Stock Exchange"}
          "Class B" {:weighted nil
                     :outstanding 175592622
                     :ticker "TAP"
                     :exchange "New York Stock Exchange"}}
         (shares/share-summary (xbrl/parse tap-cover))))
  (is (not-any? #(= "TAP 32" (:ticker %))
                (vals (shares/share-summary (xbrl/parse tap-cover))))))

(deftest common-listing-keeps-ticker-when-a-note-has-no-debt-axis
  (let [html "<html><body>
              <ix:nonNumeric name=\"dei:DocumentType\">10-K</ix:nonNumeric>
              <ix:nonNumeric name=\"dei:DocumentFiscalYearFocus\">2025</ix:nonNumeric>
              <ix:nonNumeric name=\"dei:DocumentFiscalPeriodFocus\">FY</ix:nonNumeric>
              <xbrli:context id=\"note\">
                <xbrli:entity><xbrli:identifier scheme=\"http://www.sec.gov/CIK\">0000000001</xbrli:identifier>
                  <xbrli:segment>
                    <xbrldi:explicitMember dimension=\"dei:EntityListingsExchangeAxis\">exch:XNYS</xbrldi:explicitMember>
                  </xbrli:segment>
                </xbrli:entity>
                <xbrli:period><xbrli:startDate>2025-01-01</xbrli:startDate><xbrli:endDate>2025-12-31</xbrli:endDate></xbrli:period>
              </xbrli:context>
              <xbrli:context id=\"common\">
                <xbrli:entity><xbrli:identifier scheme=\"http://www.sec.gov/CIK\">0000000001</xbrli:identifier>
                  <xbrli:segment>
                    <xbrldi:explicitMember dimension=\"dei:EntityListingsExchangeAxis\">exch:XNAS</xbrldi:explicitMember>
                  </xbrli:segment>
                </xbrli:entity>
                <xbrli:period><xbrli:startDate>2025-01-01</xbrli:startDate><xbrli:endDate>2025-12-31</xbrli:endDate></xbrli:period>
              </xbrli:context>
              <xbrli:context id=\"count\">
                <xbrli:period><xbrli:instant>2026-02-11</xbrli:instant></xbrli:period>
              </xbrli:context>
              <xbrli:unit id=\"shares\"><xbrli:measure>xbrli:shares</xbrli:measure></xbrli:unit>
              <ix:nonNumeric contextRef=\"note\" name=\"dei:Security12bTitle\">3.800% Senior Notes due 2032</ix:nonNumeric>
              <ix:nonNumeric contextRef=\"note\" name=\"dei:TradingSymbol\">TAP 32</ix:nonNumeric>
              <ix:nonNumeric contextRef=\"note\" name=\"dei:SecurityExchangeName\">New York Stock Exchange</ix:nonNumeric>
              <ix:nonNumeric contextRef=\"common\" name=\"dei:Security12bTitle\">Common Stock</ix:nonNumeric>
              <ix:nonNumeric contextRef=\"common\" name=\"dei:TradingSymbol\">BSET</ix:nonNumeric>
              <ix:nonNumeric contextRef=\"common\" name=\"dei:SecurityExchangeName\">Nasdaq</ix:nonNumeric>
              <ix:nonFraction name=\"dei:EntityCommonStockSharesOutstanding\" contextRef=\"count\" unitRef=\"shares\" scale=\"0\">8654370</ix:nonFraction>
              </body></html>"]
    (is (= {"Common" {:weighted nil
                      :outstanding 8654370
                      :ticker "BSET"
                      :exchange "Nasdaq"}}
           (shares/share-summary (xbrl/parse html))))))

(deftest dei-outstanding-wins-over-shares-outstanding
  (is (= {"Common" {:weighted nil :outstanding 100}}
         (shares/share-summary
          {:facts {:dei {:EntityCommonStockSharesOutstanding (shares-fact 100)}
                   :us-gaap {:SharesOutstanding
                             {:units {:shares [{:end "2026-09-04"
                                                :val 379236005
                                                :filed "2026-09-04"}]}}}}}))))

(defn- entity-context
  [id member period]
  (str "<xbrli:context id=\"" id "\"><xbrli:entity>"
       "<xbrli:identifier scheme=\"http://www.sec.gov/CIK\">0000107263</xbrli:identifier>"
       (when member
         (str "<xbrli:segment><xbrldi:explicitMember dimension=\"dei:LegalEntityAxis\">"
              member "</xbrldi:explicitMember></xbrli:segment>"))
       "</xbrli:entity>" period "</xbrli:context>"))

(def ^:private instant-2026 "<xbrli:period><xbrli:instant>2026-02-19</xbrli:instant></xbrli:period>")

(def ^:private year-2025 "<xbrli:period><xbrli:startDate>2025-01-01</xbrli:startDate><xbrli:endDate>2025-12-31</xbrli:endDate></xbrli:period>")

(defn- combined-cover
  "Parent plus two subsidiary registrants, the way a combined 10-K tags them."
  [parent-member]
  (str "<html><body>"
       "<ix:nonNumeric name=\"dei:EntityRegistrantName\">The Williams Companies, Inc.</ix:nonNumeric>"
       (entity-context "parent-out" parent-member year-2025)
       (entity-context "parent-instant" parent-member instant-2026)
       (entity-context "transco" "wmb:TranscontinentalGasPipeLineCompanyLLCMember" instant-2026)
       (entity-context "northwest" "wmb:NorthwestPipelineLLCMember" instant-2026)
       "<xbrli:unit id=\"shares\"><xbrli:measure>xbrli:shares</xbrli:measure></xbrli:unit>"
       "<ix:nonFraction name=\"us-gaap:WeightedAverageNumberOfSharesOutstandingBasic\" contextRef=\"parent-out\" unitRef=\"shares\" scale=\"6\">1,221</ix:nonFraction>"
       "<ix:nonFraction name=\"dei:EntityCommonStockSharesOutstanding\" contextRef=\"parent-instant\" unitRef=\"shares\">1,221,563,111</ix:nonFraction>"
       "<ix:nonFraction name=\"dei:EntityCommonStockSharesOutstanding\" contextRef=\"transco\" unitRef=\"shares\" format=\"ixt:fixed-zero\">None</ix:nonFraction>"
       "<ix:nonFraction name=\"dei:EntityCommonStockSharesOutstanding\" contextRef=\"northwest\" unitRef=\"shares\" format=\"ixt:fixed-zero\">None</ix:nonFraction>"
       "</body></html>"))

(deftest parent-wins-when-subsidiaries-also-report
  (let [facts (xbrl/parse (combined-cover nil))]
    (is (= 3 (shares/reporting-entities facts)))
    (is (= {"Common" {:weighted 1221000000, :outstanding 1221563111}}
           (shares/share-summary facts)))))

(deftest parent-member-is-recognized-when-every-row-has-an-entity-axis
  (let [facts (xbrl/parse (combined-cover "wmb:WilliamsCompaniesIncMember"))]
    (is (= 3 (shares/reporting-entities facts)))
    (is (= {"Common" {:weighted 1221000000, :outstanding 1221563111}}
           (shares/share-summary facts)))))

(deftest one-entity-has-no-entity-count
  (is (nil? (shares/reporting-entities
             {:facts {:dei {:EntityCommonStockSharesOutstanding (shares-fact 100)}}}))))

