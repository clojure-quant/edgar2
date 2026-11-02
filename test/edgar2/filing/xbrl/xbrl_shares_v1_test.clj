(ns edgar2.filing.xbrl.xbrl-shares-v1-test
  (:require [clojure.test :refer [deftest is]]
            [edgar.api :as e]
            [edgar2.filing.xbrl :as xbrl]
            [edgar2.filing.xbrl.xbrl-shares-v1 :as shares]))

(defn- shares-fact
  [val]
  {:units {:shares [{:end "2025-12-31" :val val :filed "2026-03-10"}]}})

(deftest issued-basic-is-weighted-when-outstanding-is-absent
  (is (= {"Common" {:weighted-diluted nil :weighted 26038125 :outstanding nil}}
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
    (is (= {"Common" {:weighted-diluted nil :weighted 338886497, :outstanding 351018130}}
           (shares/share-summary (xbrl/parse-all [cover instance]))))))

(deftest outstanding-basic-wins-when-issued-is-also-present
  (is (= {"Common" {:weighted-diluted nil :weighted 100 :outstanding nil}}
         (shares/share-summary
          {:facts {:us-gaap {:WeightedAverageNumberOfSharesOutstandingBasic
                             (shares-fact 100)
                             :WeightedAverageNumberOfSharesIssuedBasic
                             (shares-fact 26038125)}}}))))

(deftest shares-outstanding-fills-when-dei-has-no-facts
  (is (= {"Common" {:weighted-diluted nil :weighted nil :outstanding 379236005}}
         (shares/share-summary
          {:facts {:dei {:EntityCommonStockSharesOutstanding {:units {}}}
                   :us-gaap {:SharesOutstanding (shares-fact 379236005)}}}))))

(deftest common-stock-shares-outstanding-fills-when-dei-has-no-facts
  (is (= {"Common" {:weighted-diluted nil :weighted 42976024 :outstanding 49335551}}
         (shares/share-summary
          {:facts {:us-gaap {:WeightedAverageNumberOfSharesOutstandingBasic
                             (shares-fact 42976024)
                             :CommonStockSharesOutstanding
                             (shares-fact 49335551)}}}))))

(deftest class-label-reads-a-before-a-class-description
  (let [html "<html><body>
              <ix:nonNumeric name=\"dei:EntityRegistrantName\">WEBULL CORP</ix:nonNumeric>
              <xbrli:context id=\"a\">
                <xbrli:entity><xbrli:identifier scheme=\"http://www.sec.gov/CIK\">0001866501</xbrli:identifier>
                  <xbrli:segment><xbrldi:explicitMember dimension=\"ifrs-full:ClassesOfShareCapitalAxis\">wbx:ClassACommonShareMember</xbrldi:explicitMember></xbrli:segment>
                </xbrli:entity>
                <xbrli:period><xbrli:instant>2025-12-31</xbrli:instant></xbrli:period>
              </xbrli:context>
              <xbrli:context id=\"b\">
                <xbrli:entity><xbrli:identifier scheme=\"http://www.sec.gov/CIK\">0001866501</xbrli:identifier>
                  <xbrli:segment><xbrldi:explicitMember dimension=\"ifrs-full:ClassesOfShareCapitalAxis\">wbx:ClassBCommonShareMember</xbrldi:explicitMember></xbrli:segment>
                </xbrli:entity>
                <xbrli:period><xbrli:instant>2025-12-31</xbrli:instant></xbrli:period>
              </xbrli:context>
              <xbrli:unit id=\"shares\"><xbrli:measure>xbrli:shares</xbrli:measure></xbrli:unit>
              <ix:nonFraction name=\"dei:EntityCommonStockSharesOutstanding\" contextRef=\"a\" unitRef=\"shares\">16,778,631</ix:nonFraction>
              <ix:nonFraction name=\"dei:EntityCommonStockSharesOutstanding\" contextRef=\"b\" unitRef=\"shares\">355,040</ix:nonFraction>
              </body></html>"]
    (is (= {"Class A" {:weighted-diluted nil :weighted nil :outstanding 16778631}
            "Class B" {:weighted-diluted nil :weighted nil :outstanding 355040}}
           (shares/share-summary (xbrl/parse html))))))

(deftest voting-share-classes-stay-distinct
  (let [html "<html><body>
              <ix:nonNumeric name=\"dei:EntityRegistrantName\">TILRAY BRANDS, INC.</ix:nonNumeric>
              <xbrli:context id=\"fy\">
                <xbrli:entity><xbrli:identifier scheme=\"http://www.sec.gov/CIK\">0001754195</xbrli:identifier></xbrli:entity>
                <xbrli:period><xbrli:startDate>2025-01-01</xbrli:startDate><xbrli:endDate>2025-12-31</xbrli:endDate></xbrli:period>
              </xbrli:context>
              <xbrli:context id=\"sub\">
                <xbrli:entity><xbrli:identifier scheme=\"http://www.sec.gov/CIK\">0001754195</xbrli:identifier>
                  <xbrli:segment><xbrldi:explicitMember dimension=\"us-gaap:StatementClassOfStockAxis\">trlv:SubordinateVotingSharesMember</xbrldi:explicitMember></xbrli:segment>
                </xbrli:entity>
                <xbrli:period><xbrli:instant>2026-02-19</xbrli:instant></xbrli:period>
              </xbrli:context>
              <xbrli:context id=\"mult\">
                <xbrli:entity><xbrli:identifier scheme=\"http://www.sec.gov/CIK\">0001754195</xbrli:identifier>
                  <xbrli:segment><xbrldi:explicitMember dimension=\"us-gaap:StatementClassOfStockAxis\">trlv:MultipleVotingSharesMember</xbrldi:explicitMember></xbrli:segment>
                </xbrli:entity>
                <xbrli:period><xbrli:instant>2026-02-19</xbrli:instant></xbrli:period>
              </xbrli:context>
              <xbrli:context id=\"super\">
                <xbrli:entity><xbrli:identifier scheme=\"http://www.sec.gov/CIK\">0001754195</xbrli:identifier>
                  <xbrli:segment><xbrldi:explicitMember dimension=\"us-gaap:StatementClassOfStockAxis\">trlv:SuperVotingSharesMember</xbrldi:explicitMember></xbrli:segment>
                </xbrli:entity>
                <xbrli:period><xbrli:instant>2026-02-19</xbrli:instant></xbrli:period>
              </xbrli:context>
              <xbrli:unit id=\"shares\"><xbrli:measure>xbrli:shares</xbrli:measure></xbrli:unit>
              <ix:nonFraction name=\"us-gaap:WeightedAverageNumberOfSharesOutstandingBasic\" contextRef=\"fy\" unitRef=\"shares\">191,323,791</ix:nonFraction>
              <ix:nonFraction name=\"dei:EntityCommonStockSharesOutstanding\" contextRef=\"sub\" unitRef=\"shares\">169,080,759</ix:nonFraction>
              <ix:nonFraction name=\"dei:EntityCommonStockSharesOutstanding\" contextRef=\"mult\" unitRef=\"shares\">23,226,386</ix:nonFraction>
              <ix:nonFraction name=\"dei:EntityCommonStockSharesOutstanding\" contextRef=\"super\" unitRef=\"shares\">0</ix:nonFraction>
              </body></html>"]
    (is (= {"Common" {:weighted-diluted nil :weighted 191323791 :outstanding nil}
            "Subordinate Voting" {:weighted-diluted nil :weighted nil :outstanding 169080759}
            "Multiple Voting" {:weighted-diluted nil :weighted nil :outstanding 23226386}
            "Super Voting" {:weighted-diluted nil :weighted nil :outstanding 0}}
           (shares/share-summary (xbrl/parse html))))))

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
  (is (= {"Common" {:weighted-diluted nil :weighted 199100000, :outstanding nil}
          "Class A" {:weighted-diluted nil :weighted nil
                     :outstanding 2563034
                     :ticker "TAP.A"
                     :exchange "New York Stock Exchange"}
          "Class B" {:weighted-diluted nil :weighted nil
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
    (is (= {"Common" {:weighted-diluted nil :weighted nil
                      :outstanding 8654370
                      :ticker "BSET"
                      :exchange "Nasdaq"}}
           (shares/share-summary (xbrl/parse html))))))

(deftest dei-outstanding-wins-over-shares-outstanding
  (is (= {"Common" {:weighted-diluted nil :weighted nil :outstanding 100}}
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
    (is (= {"Common" {:weighted-diluted nil :weighted nil, :outstanding 550107295}
            "Common Nonvoting" {:weighted-diluted nil :weighted nil, :outstanding 0}}
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
    (is (= {"Common" {:weighted-diluted nil :weighted 3333000000, :outstanding nil}
            "Class A" {:weighted-diluted nil :weighted nil, :outstanding 261755983}
            "Class B" {:weighted-diluted nil :weighted nil
                       :outstanding 3109595752
                       :ticker "ERIC"
                       :exchange "The NASDAQ Stock Market LLC"}
            "Class C" {:weighted-diluted nil :weighted nil, :outstanding 0}}
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
    (is (= {"Common" {:weighted-diluted nil :weighted 929100000, :outstanding 932525817}
            "Class B" {:weighted-diluted nil :weighted nil, :outstanding 0}}
           (shares/share-summary (xbrl/parse html))))))

(deftest dei-outstanding-wins-over-ifrs-number-of-shares-outstanding
  (is (= {"Common" {:weighted-diluted nil :weighted nil :outstanding 100}}
         (shares/share-summary
          {:facts {:dei {:EntityCommonStockSharesOutstanding (shares-fact 100)}
                   :ifrs-full {:NumberOfSharesOutstanding (shares-fact 999)}}}))))

(deftest parent-wins-when-subsidiaries-also-report
  (let [facts (xbrl/parse (combined-cover nil))]
    (is (= 3 (shares/reporting-entities facts)))
    (is (= {"Common" {:weighted-diluted nil :weighted 1221000000, :outstanding 1221563111}}
           (shares/share-summary facts)))))

(deftest parent-member-is-recognized-when-every-row-has-an-entity-axis
  (let [facts (xbrl/parse (combined-cover "wmb:WilliamsCompaniesIncMember"))]
    (is (= 3 (shares/reporting-entities facts)))
    (is (= {"Common" {:weighted-diluted nil :weighted 1221000000, :outstanding 1221563111}}
           (shares/share-summary facts)))))

(deftest diluted-weighted-shares-are-reported-separately
  (is (= {"Common" {:weighted-diluted 120 :weighted 100 :outstanding nil}}
         (shares/share-summary
          {:facts {:us-gaap {:WeightedAverageNumberOfSharesOutstandingBasic
                             (shares-fact 100)
                             :WeightedAverageNumberOfDilutedSharesOutstanding
                             (shares-fact 120)}}}))))

(deftest diluted-falls-back-through-units-ifrs-and-adjustment
  (is (= 120 (:weighted-diluted
              (get (shares/share-summary
                    {:facts {:us-gaap {:WeightedAverageLimitedPartnershipUnitsOutstandingDiluted
                                       (shares-fact 120)}}})
                   "Common"))))
  (is (= 130 (:weighted-diluted
              (get (shares/share-summary
                    {:facts {:ifrs-full {:AdjustedWeightedAverageShares
                                         (shares-fact 130)}}})
                   "Common"))))
  (is (= 5 (:weighted-diluted
            (get (shares/share-summary
                  {:facts {:us-gaap {:WeightedAverageNumberDilutedSharesOutstandingAdjustment
                                     (shares-fact 5)}}})
                 "Common")))))

(deftest limited-partnership-units-fill-weighted-last
  (is (= {"Common" {:weighted-diluted nil :weighted 1000 :outstanding 1000}}
         (shares/share-summary
          {:facts {:us-gaap {:WeightedAverageLimitedPartnershipUnitsOutstanding
                             (shares-fact 1000)}}})))
  (is (= 10 (:weighted
             (get (shares/share-summary
                   {:facts {:us-gaap
                            {:WeightedAverageNumberOfSharesOutstandingBasic (shares-fact 10)
                             :WeightedAverageLimitedPartnershipUnitsOutstanding (shares-fact 999)}}})
                  "Common")))))

(deftest partnership-units-fill-outstanding-last
  (is (= 8500000 (:outstanding
                  (get (shares/share-summary
                        {:facts {:us-gaap {:LimitedPartnersCapitalAccountUnitsOutstanding
                                           (shares-fact 8500000)}}})
                       "Common"))))
  (is (= 40 (:outstanding
             (get (shares/share-summary
                   {:facts {:us-gaap {:GeneralPartnersCapitalAccountUnitsOutstanding
                                      (shares-fact 40)}}})
                  "Common"))))
  (is (= 500 (:outstanding
              (get (shares/share-summary
                    {:facts {:us-gaap {:CapitalUnitsOutstanding (shares-fact 500)}}})
                   "Common"))))
  (is (= 777 (:outstanding
              (get (shares/share-summary
                    {:facts {:us-gaap {:WeightedAverageLimitedPartnershipUnitsOutstanding
                                       (shares-fact 777)}}})
                   "Common"))))
  (is (= 100 (:outstanding
              (get (shares/share-summary
                    {:facts {:dei {:EntityCommonStockSharesOutstanding (shares-fact 100)}
                             :us-gaap {:GeneralPartnersCapitalAccountUnitsOutstanding
                                       (shares-fact 40)}}})
                   "Common")))))

(deftest limited-partner-units-win-over-general-partner-units
  (is (= 8500000
         (:outstanding
          (get (shares/share-summary
                {:facts {:us-gaap {:LimitedPartnersCapitalAccountUnitsOutstanding
                                   (shares-fact 8500000)
                                   :GeneralPartnersCapitalAccountUnitsOutstanding
                                   (shares-fact 40)}}})
               "Common")))))

(deftest zero-cover-count-does-not-block-a-statement-count
  (is (= {"Common" {:weighted-diluted nil :weighted nil :outstanding 99061523}}
         (shares/share-summary
          {:facts {:dei {:EntityCommonStockSharesOutstanding (shares-fact 0)}
                   :us-gaap {:CommonStockSharesOutstanding (shares-fact 99061523)}}}))))

(deftest zero-cover-count-does-not-block-ifrs
  (is (= 166896936
         (:outstanding
          (get (shares/share-summary
                {:facts {:dei {:EntityCommonStockSharesOutstanding (shares-fact 0)}
                         :ifrs-full {:NumberOfSharesOutstanding (shares-fact 166896936)}}})
               "Common")))))

(deftest an-all-zero-filing-still-reads-zero
  (is (= 0 (:outstanding
            (get (shares/share-summary
                  {:facts {:dei {:EntityCommonStockSharesOutstanding (shares-fact 0)}}})
                 "Common")))))

(deftest zero-weighted-basic-does-not-block-ifrs
  (is (= 100 (:weighted
              (get (shares/share-summary
                    {:facts {:us-gaap {:WeightedAverageNumberOfSharesOutstandingBasic
                                       (shares-fact 0)}
                             :ifrs-full {:WeightedAverageShares (shares-fact 100)}}})
                   "Common")))))

(deftest one-entity-has-no-entity-count
  (is (nil? (shares/reporting-entities
             {:facts {:dei {:EntityCommonStockSharesOutstanding (shares-fact 100)}}}))))

(deftest xbrl-text-detects-inline-and-instance
  (is (true? (shares/xbrl-text?
              "<ix:nonFraction name=\"dei:EntityCommonStockSharesOutstanding\" contextRef=\"c\">1</ix:nonFraction>")))
  (is (true? (shares/xbrl-text? "<xbrl xmlns=\"http://www.xbrl.org/2003/instance\"></xbrl>")))
  (is (false? (shares/xbrl-text?
               "<html><body>107,792,002 shares outstanding as of March 31, 2026</body></html>")))
  (is (false? (shares/xbrl-text? nil))))

(deftest instance-root-recognizes-an-instance-not-a-linkbase
  (is (true? (shares/instance-root?
              "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<xbrl xmlns=\"http://www.xbrl.org/2003/instance\"></xbrl>")))
  (is (true? (shares/instance-root?
              "<xbrli:xbrl xmlns:xbrli=\"http://www.xbrl.org/2003/instance\"></xbrli:xbrl>")))
  (is (false? (shares/instance-root?
               "<?xml version=\"1.0\"?><link:linkbase xmlns:link=\"http://www.xbrl.org/2003/linkbase\"></link:linkbase>")))
  (is (false? (shares/instance-root? "<html><body>no facts</body></html>"))))

(deftest indexed-instances-prefer-ex-101-ins
  (with-redefs [e/xbrl-docs
                (constantly
                 [{:type "EX-101.SCH" :name "scda-20151031.xsd"}
                  {:type "EX-101.CAL" :name "scda-20151031_cal.xml"}
                  {:type "EX-101.LAB" :name "scda-20151031_lab.xml"}
                  {:type "EX-101.INS" :name "scda-20151031.xml"}
                  {:type "EX-99.2" :name "exhibit99-2.htm"}
                  {:type "EX-99.3" :name "other.xml"}])]
    (is (= ["scda-20151031.xml" "other.xml"]
           (shares/indexed-instances {:cik "1341878"})))))

(deftest filing-xbrl-false-when-nothing-is-tagged
  (with-redefs [e/xbrl-docs (constantly ())]
    (is (false? (shares/filing-xbrl?
                 {:isXBRL 0 :isInlineXBRL 0}
                 ["<html><body>107,792,002 shares outstanding</body></html>"]
                 {})))
    (is (true? (shares/filing-xbrl? {:isXBRL 1} [] {})))
    (is (true? (shares/filing-xbrl? {:isInlineXBRL 1} [] {})))
    (is (true? (shares/filing-xbrl? {:isXBRL 0} [] {:facts {:dei {}}})))
    (is (true? (shares/filing-xbrl?
                {:isXBRL 0}
                ["<ix:nonFraction name=\"dei:EntityCentralIndexKey\">1</ix:nonFraction>"]
                {})))))

(deftest filing-xbrl-true-when-the-index-lists-an-instance
  (with-redefs [e/xbrl-docs (constantly [{:type "EX-101.INS" :name "ztek-20250331.xml"}])]
    (is (true? (shares/filing-xbrl?
                {:isXBRL 0 :isInlineXBRL 0}
                ["<html><body>no tags</body></html>"]
                {})))))

