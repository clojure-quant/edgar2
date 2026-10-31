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

(deftest nonvoting-common-keeps-its-own-class
  (let [html "<html><body>
              <ix:nonNumeric name=\"dei:EntityRegistrantName\">HORMEL FOODS CORPORATION</ix:nonNumeric>
              <xbrli:context id=\"voting\">
                <xbrli:entity><xbrli:identifier scheme=\"http://www.sec.gov/CIK\">0000048465</xbrli:identifier>
                  <xbrli:segment>
                    <xbrldi:explicitMember dimension=\"us-gaap:StatementClassOfStockAxis\">us-gaap:CommonStockMember</xbrldi:explicitMember>
                  </xbrli:segment>
                </xbrli:entity>
                <xbrli:period><xbrli:instant>2025-11-30</xbrli:instant></xbrli:period>
              </xbrli:context>
              <xbrli:context id=\"nonvoting\">
                <xbrli:entity><xbrli:identifier scheme=\"http://www.sec.gov/CIK\">0000048465</xbrli:identifier>
                  <xbrli:segment>
                    <xbrldi:explicitMember dimension=\"us-gaap:StatementClassOfStockAxis\">us-gaap:NonvotingCommonStockMember</xbrldi:explicitMember>
                  </xbrli:segment>
                </xbrli:entity>
                <xbrli:period><xbrli:instant>2025-11-30</xbrli:instant></xbrli:period>
              </xbrli:context>
              <xbrli:unit id=\"shares\"><xbrli:measure>xbrli:shares</xbrli:measure></xbrli:unit>
              <ix:nonFraction name=\"dei:EntityCommonStockSharesOutstanding\" contextRef=\"voting\" unitRef=\"shares\" decimals=\"0\">550,107,295</ix:nonFraction>
              <ix:nonFraction name=\"dei:EntityCommonStockSharesOutstanding\" contextRef=\"nonvoting\" unitRef=\"shares\" decimals=\"INF\">0</ix:nonFraction>
              </body></html>"]
    (is (= {"Common" {:weighted nil, :outstanding 550107295}
            "Common Nonvoting" {:weighted nil, :outstanding 0}}
           (shares/share-summary (xbrl/parse html))))))

(defn- ifrs-class-context
  [id member period]
  (str "<xbrli:context id=\"" id "\"><xbrli:entity>"
       "<xbrli:identifier scheme=\"http://www.sec.gov/CIK\">0000717826</xbrli:identifier>"
       "<xbrli:segment><xbrldi:explicitMember dimension=\"ifrs-full:ClassesOfShareCapitalAxis\">"
       member "</xbrldi:explicitMember></xbrli:segment>"
       "</xbrli:entity>" period "</xbrli:context>"))

(def ^:private instant-2025 "<xbrli:period><xbrli:instant>2025-12-31</xbrli:instant></xbrli:period>")

(deftest ifrs-class-axis-splits-ordinary-share-classes
  (let [html (str "<html><body>"
                  "<ix:nonNumeric name=\"dei:EntityRegistrantName\">ERICSSON LM TELEPHONE CO</ix:nonNumeric>"
                  "<xbrli:context id=\"fy\">"
                  "<xbrli:entity><xbrli:identifier scheme=\"http://www.sec.gov/CIK\">0000717826</xbrli:identifier></xbrli:entity>"
                  "<xbrli:period><xbrli:startDate>2025-01-01</xbrli:startDate><xbrli:endDate>2025-12-31</xbrli:endDate></xbrli:period>"
                  "</xbrli:context>"
                  (ifrs-class-context "a" "eric:OrdinarySharesAMember" instant-2025)
                  (ifrs-class-context "b" "eric:OrdinarySharesBMember" instant-2025)
                  (ifrs-class-context "c" "eric:OrdinarySharesCMember" instant-2025)
                  "<xbrli:unit id=\"shares\"><xbrli:measure>xbrli:shares</xbrli:measure></xbrli:unit>"
                  "<ix:nonNumeric contextRef=\"fy\" name=\"dei:Security12bTitle\">American Depositary Shares (each representing one B share)</ix:nonNumeric>"
                  "<ix:nonNumeric contextRef=\"fy\" name=\"dei:TradingSymbol\">ERIC</ix:nonNumeric>"
                  "<ix:nonNumeric contextRef=\"fy\" name=\"dei:SecurityExchangeName\">The NASDAQ Stock Market LLC</ix:nonNumeric>"
                  "<ix:nonFraction name=\"ifrs-full:WeightedAverageShares\" contextRef=\"fy\" unitRef=\"shares\" scale=\"6\">3,333</ix:nonFraction>"
                  "<ix:nonFraction name=\"dei:EntityCommonStockSharesOutstanding\" contextRef=\"a\" unitRef=\"shares\">261,755,983</ix:nonFraction>"
                  "<ix:nonFraction name=\"dei:EntityCommonStockSharesOutstanding\" contextRef=\"b\" unitRef=\"shares\">3,109,595,752</ix:nonFraction>"
                  "<ix:nonFraction name=\"dei:EntityCommonStockSharesOutstanding\" contextRef=\"c\" unitRef=\"shares\" decimals=\"INF\">0</ix:nonFraction>"
                  "</body></html>")]
    (is (= {"Common" {:weighted 3333000000, :outstanding nil}
            "Class A" {:weighted nil, :outstanding 261755983}
            "Class B" {:weighted nil
                       :outstanding 3109595752
                       :ticker "ERIC"
                       :exchange "The NASDAQ Stock Market LLC"}
            "Class C" {:weighted nil, :outstanding 0}}
           (shares/share-summary (xbrl/parse html))))))

(defn- ifrs-share-context
  [id dims period]
  (str "<xbrli:context id=\"" id "\"><xbrli:entity>"
       "<xbrli:identifier scheme=\"http://www.sec.gov/CIK\">0000718940</xbrli:identifier>"
       "<xbrli:segment>" dims "</xbrli:segment>"
       "</xbrli:entity>" period "</xbrli:context>"))

(deftest ifrs-number-of-shares-outstanding-fills-when-dei-is-absent
  (let [html (str "<html><body>"
                  "<ix:nonNumeric name=\"dei:EntityRegistrantName\">BCE INC.</ix:nonNumeric>"
                  "<xbrli:context id=\"fy\">"
                  "<xbrli:entity><xbrli:identifier scheme=\"http://www.sec.gov/CIK\">0000718940</xbrli:identifier></xbrli:entity>"
                  "<xbrli:period><xbrli:startDate>2025-01-01</xbrli:startDate><xbrli:endDate>2025-12-31</xbrli:endDate></xbrli:period>"
                  "</xbrli:context>"
                  (ifrs-share-context
                   "ordinary"
                   (str "<xbrldi:explicitMember dimension=\"ifrs-full:ClassesOfShareCapitalAxis\">ifrs-full:OrdinarySharesMember</xbrldi:explicitMember>"
                        "<xbrldi:explicitMember dimension=\"ifrs-full:ComponentsOfEquityAxis\">ifrs-full:IssuedCapitalMember</xbrldi:explicitMember>")
                   instant-2025)
                  (ifrs-share-context
                   "preferred"
                   "<xbrldi:explicitMember dimension=\"ifrs-full:ClassesOfShareCapitalAxis\">ifrs-full:PreferenceSharesMember</xbrldi:explicitMember>"
                   instant-2025)
                  (ifrs-share-context
                   "b"
                   "<xbrldi:explicitMember dimension=\"ifrs-full:ClassesOfShareCapitalAxis\">bce:ClassBSharesMember</xbrldi:explicitMember>"
                   instant-2025)
                  "<xbrli:unit id=\"shares\"><xbrli:measure>xbrli:shares</xbrli:measure></xbrli:unit>"
                  "<ix:nonFraction name=\"ifrs-full:WeightedAverageShares\" contextRef=\"fy\" unitRef=\"shares\" scale=\"6\">929.1</ix:nonFraction>"
                  "<ix:nonFraction name=\"ifrs-full:NumberOfSharesOutstanding\" contextRef=\"ordinary\" unitRef=\"shares\">932,525,817</ix:nonFraction>"
                  "<ix:nonFraction name=\"ifrs-full:NumberOfSharesOutstanding\" contextRef=\"preferred\" unitRef=\"shares\">131,755,879</ix:nonFraction>"
                  "<ix:nonFraction name=\"ifrs-full:NumberOfSharesOutstanding\" contextRef=\"b\" unitRef=\"shares\" decimals=\"INF\">0</ix:nonFraction>"
                  "</body></html>")]
    (is (= {"Common" {:weighted 929100000, :outstanding 932525817}
            "Class B" {:weighted nil, :outstanding 0}}
           (shares/share-summary (xbrl/parse html))))))

(deftest dei-outstanding-wins-over-ifrs-number-of-shares-outstanding
  (is (= {"Common" {:weighted nil :outstanding 100}}
         (shares/share-summary
          {:facts {:dei {:EntityCommonStockSharesOutstanding (shares-fact 100)}
                   :ifrs-full {:NumberOfSharesOutstanding (shares-fact 999)}}}))))

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

