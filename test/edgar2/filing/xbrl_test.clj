(ns edgar2.filing.xbrl-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [edgar2.filing.xbrl :as xbrl]
            [edgar2.filing.xbrl-facts :as xbrl-facts]
            [edgar2.filing.xbrl-loader :as xbrl-loader]
            [edgar2.filing.xbrl-overview :as xbrl-overview]))

(def coke-cover
  "<html><body>
   <ix:hidden>
     <ix:nonNumeric name=\"dei:EntityCentralIndexKey\">0000317540</ix:nonNumeric>
     <ix:nonNumeric name=\"dei:EntityRegistrantName\">COCA-COLA CONSOLIDATED, INC.</ix:nonNumeric>
     <ix:nonNumeric name=\"dei:DocumentType\">10-K</ix:nonNumeric>
     <ix:nonNumeric name=\"dei:DocumentFiscalYearFocus\">2025</ix:nonNumeric>
     <ix:nonNumeric name=\"dei:DocumentFiscalPeriodFocus\">FY</ix:nonNumeric>
   </ix:hidden>
   <ix:resources>
     <xbrli:context id=\"c-1\">
       <xbrli:entity><xbrli:identifier scheme=\"http://www.sec.gov/CIK\">0000317540</xbrli:identifier></xbrli:entity>
       <xbrli:period><xbrli:startDate>2025-01-01</xbrli:startDate><xbrli:endDate>2025-12-31</xbrli:endDate></xbrli:period>
     </xbrli:context>
     <xbrli:context id=\"c-3\">
       <xbrli:entity><xbrli:identifier scheme=\"http://www.sec.gov/CIK\">0000317540</xbrli:identifier>
         <xbrli:segment><xbrldi:explicitMember dimension=\"us-gaap:StatementClassOfStockAxis\">coke:CommonClassUndefinedMember</xbrldi:explicitMember></xbrli:segment>
       </xbrli:entity>
       <xbrli:period><xbrli:instant>2026-01-30</xbrli:instant></xbrli:period>
     </xbrli:context>
     <xbrli:context id=\"c-4\">
       <xbrli:entity><xbrli:identifier scheme=\"http://www.sec.gov/CIK\">0000317540</xbrli:identifier>
         <xbrli:segment><xbrldi:explicitMember dimension=\"us-gaap:StatementClassOfStockAxis\">us-gaap:CommonClassBMember</xbrldi:explicitMember></xbrli:segment>
       </xbrli:entity>
       <xbrli:period><xbrli:instant>2026-01-30</xbrli:instant></xbrli:period>
     </xbrli:context>
     <xbrli:unit id=\"shares\"><xbrli:measure>xbrli:shares</xbrli:measure></xbrli:unit>
     <xbrli:unit id=\"usd\"><xbrli:measure>iso4217:USD</xbrli:measure></xbrli:unit>
     <xbrli:unit id=\"usdPerShare\"><xbrli:divide>
       <xbrli:unitNumerator><xbrli:measure>iso4217:USD</xbrli:measure></xbrli:unitNumerator>
       <xbrli:unitDenominator><xbrli:measure>xbrli:shares</xbrli:measure></xbrli:unitDenominator>
     </xbrli:divide></xbrli:unit>
     <xbrli:unit id=\"segment\"><xbrli:measure>coke:segment</xbrli:measure></xbrli:unit>
   </ix:resources>
   <ix:nonFraction name=\"dei:EntityCommonStockSharesOutstanding\" contextRef=\"c-3\" unitRef=\"shares\" scale=\"0\" format=\"ixt:num-dot-decimal\">56,517,334</ix:nonFraction>
   <ix:nonFraction name=\"dei:EntityCommonStockSharesOutstanding\" contextRef=\"c-3\" unitRef=\"shares\" scale=\"0\">56,517,334</ix:nonFraction>
   <ix:nonFraction name=\"dei:EntityCommonStockSharesOutstanding\" contextRef=\"c-4\" unitRef=\"shares\" scale=\"0\">10,046,960</ix:nonFraction>
   <ix:nonFraction name=\"us-gaap:InterestIncomeExpenseNonoperatingNet\" contextRef=\"c-1\" unitRef=\"usd\" scale=\"3\" sign=\"-\" format=\"ixt:num-dot-decimal\">42,678</ix:nonFraction>
   <ix:nonFraction name=\"us-gaap:PensionExpense\" contextRef=\"c-1\" unitRef=\"usd\" scale=\"3\" format=\"ixt:fixed-zero\">&#8212;</ix:nonFraction>
   <ix:nonFraction name=\"us-gaap:EarningsPerShareBasic\" contextRef=\"c-1\" unitRef=\"usdPerShare\" scale=\"0\" decimals=\"2\">1.25</ix:nonFraction>
   <ix:nonFraction name=\"us-gaap:NumberOfOperatingSegments\" contextRef=\"c-1\" unitRef=\"segment\" scale=\"0\" format=\"ixt-sec:numwordsen\">two</ix:nonFraction>
   <ix:nonFraction name=\"us-gaap:CommitmentsAndContingencies\" contextRef=\"c-1\" unitRef=\"usd\" xsi:nil=\"true\"></ix:nonFraction>
   </body></html>")

(def filing-meta
  {:accn "0001628280-26-009057" :filed "2026-02-18"})

(deftest cover-page-shares-keep-each-class
  (let [parsed (xbrl-facts/parse coke-cover filing-meta)
        shares (get-in parsed [:facts :dei :EntityCommonStockSharesOutstanding :units "shares"])
        interest (get-in parsed [:facts :us-gaap :InterestIncomeExpenseNonoperatingNet :units "USD"])]
    (is (= 317540 (:cik parsed)))
    (is (= "COCA-COLA CONSOLIDATED, INC." (:entityName parsed)))
    (is (= [{:end "2026-01-30"
             :val 56517334
             :accn "0001628280-26-009057"
             :fy 2025
             :fp "FY"
             :form "10-K"
             :filed "2026-02-18"
             :dimensions {"us-gaap:StatementClassOfStockAxis"
                          "coke:CommonClassUndefinedMember"}}
            {:end "2026-01-30"
             :val 10046960
             :accn "0001628280-26-009057"
             :fy 2025
             :fp "FY"
             :form "10-K"
             :filed "2026-02-18"
             :dimensions {"us-gaap:StatementClassOfStockAxis"
                          "us-gaap:CommonClassBMember"}}]
           shares))
    (is (= [{:start "2025-01-01"
             :end "2025-12-31"
             :val -42678000
             :accn "0001628280-26-009057"
             :fy 2025
             :fp "FY"
             :form "10-K"
             :filed "2026-02-18"}]
           interest))
    (is (= 0 (get-in parsed [:facts :us-gaap :PensionExpense :units "USD" 0 :val])))
    (is (= 1.25 (get-in parsed [:facts :us-gaap :EarningsPerShareBasic :units "USD/shares" 0 :val])))
    (is (= 2 (get-in parsed [:facts :us-gaap :NumberOfOperatingSegments :units "segment" 0 :val])))
    (is (nil? (get-in parsed [:facts :us-gaap :CommitmentsAndContingencies])))))

(deftest statement-exhibit-name-is-ex99d2
  (is (= "su-20251231xex99d2.htm"
         (xbrl-loader/statement-exhibit-name
          "<a href=\"su-20251231xex99d1.htm\"></a><a href=\"su-20251231xex99d2.htm\"></a><a href=\"su-20251231xex99d10.htm\"></a>")))
  (is (nil? (xbrl-loader/statement-exhibit-name "<a href=\"su-20251231xex99d10.htm\"></a>")))
  (is (nil? (xbrl-loader/statement-exhibit-name
             "<a href=\"https://www.sec.gov/Archives/edgar/data/1273813/000110465909052888/a09-25104_1ex99d2.htm\"></a>"))))

(deftest statement-exhibit-name-matches-dashed-variants
  (is (= "exhibit99-2.htm"
         (xbrl-loader/statement-exhibit-name "<a href=\"exhibit99-1.htm\"></a><a href=\"exhibit99-2.htm\"></a>")))
  (is (= "ex-99.2.htm"
         (xbrl-loader/statement-exhibit-name "<a href=\"ex-99.2.htm\"></a>")))
  (is (= "ex99-2.htm"
         (xbrl-loader/statement-exhibit-name "<a href=\"ex99-2.htm\"></a>")))
  (is (= "su-20251231x_d2.htm"
         (xbrl-loader/statement-exhibit-name "<a href=\"su-20251231x_d2.htm\"></a>")))
  (is (nil? (xbrl-loader/statement-exhibit-name "<a href=\"form1992.htm\"></a>"))))

(deftest numeric-formats-and-scales
  (let [html "<html><body>
   <xbrli:context id=\"fy\"><xbrli:period><xbrli:startDate>2025-01-01</xbrli:startDate><xbrli:endDate>2025-12-31</xbrli:endDate></xbrli:period></xbrli:context>
   <xbrli:unit id=\"eur\"><xbrli:measure>iso4217:EUR</xbrli:measure></xbrli:unit>
   <ix:nonFraction name=\"ifrs-full:Revenue\" contextRef=\"fy\" unitRef=\"eur\" scale=\"3\" format=\"ixt:num-comma-decimal\">1.234,5</ix:nonFraction>
   <ix:nonFraction name=\"ifrs-full:Assets\" contextRef=\"fy\" unitRef=\"eur\" scale=\"0\" format=\"ixt:num-dot-decimal\">1 234.5</ix:nonFraction>
   <ix:nonFraction name=\"ifrs-full:Liabilities\" contextRef=\"fy\" unitRef=\"eur\" scale=\"0\" format=\"ixt:numspacecomma\">1 234,5</ix:nonFraction>
   <ix:nonFraction name=\"ifrs-full:Equity\" contextRef=\"fy\" unitRef=\"eur\" scale=\"0\" format=\"ixt:numdash\">&#8212;</ix:nonFraction>
   <ix:nonFraction name=\"ifrs-full:ProfitLoss\" contextRef=\"fy\" unitRef=\"eur\" scale=\"0\" format=\"ixt:zerodash\">42</ix:nonFraction>
   <ix:nonFraction name=\"ifrs-full:Goodwill\" contextRef=\"fy\" unitRef=\"eur\" scale=\"-2\" format=\"ixt:num-dot-decimal\">5</ix:nonFraction>
   <ix:nonFraction name=\"ifrs-full:IntangibleAssetsOtherThanGoodwill\" contextRef=\"fy\" unitRef=\"eur\" scale=\"6\" sign=\"-\" format=\"ixt:num-dot-decimal\">1.5</ix:nonFraction>
   </body></html>"
        parsed (xbrl-facts/parse html)
        val #(get-in parsed [:facts :ifrs-full % :units "EUR" 0 :val])]
    (is (= 1234500 (val :Revenue)))
    (is (= 1234.5 (val :Assets)))
    (is (= 1234.5 (val :Liabilities)))
    (is (= 0 (val :Equity)))
    (is (= 42 (val :ProfitLoss)))
    (is (= 0.05 (val :Goodwill)))
    (is (= -1500000 (val :IntangibleAssetsOtherThanGoodwill)))))

(deftest nested-fact-outer-takes-no-value
  (let [html "<html><body>
   <xbrli:context id=\"fy\"><xbrli:period><xbrli:startDate>2025-01-01</xbrli:startDate><xbrli:endDate>2025-12-31</xbrli:endDate></xbrli:period></xbrli:context>
   <xbrli:unit id=\"segment\"><xbrli:measure>segment</xbrli:measure></xbrli:unit>
   <ix:nonFraction name=\"us-gaap:NumberOfReportableSegments\" contextRef=\"fy\" unitRef=\"segment\" decimals=\"INF\" format=\"ixt-sec:numwordsen\"><ix:nonFraction name=\"us-gaap:NumberOfReportableSegments\" contextRef=\"fy\" unitRef=\"segment\" scale=\"0\">1</ix:nonFraction></ix:nonFraction>
   </body></html>"
        rows (:units (get-in (xbrl-facts/parse html)
                             [:facts :us-gaap :NumberOfReportableSegments]))
        facts (mapcat val rows)]
    (is (= [1] (mapv :val facts)))))

(deftest prefixed-instance-namespace-is-read
  (let [html "<html><body>
   <i:context id=\"fy\"><i:period><i:startDate>2025-01-01</i:startDate><i:endDate>2025-12-31</i:endDate></i:period></i:context>
   <i:context id=\"instant\"><i:entity><i:segment>
       <xbrldi:explicitMember dimension=\"us-gaap:StatementClassOfStockAxis\">us-gaap:CommonStockMember</xbrldi:explicitMember>
     </i:segment></i:entity>
     <i:period><i:instant>2025-12-31</i:instant></i:period></i:context>
   <i:unit id=\"usd\"><i:divide><i:unitNumerator><i:measure>iso4217:USD</i:measure></i:unitNumerator>
     <i:unitDenominator><i:measure>xbrli:shares</i:measure></i:unitDenominator></i:divide></i:unit>
   <i:unit id=\"shares\"><i:measure>xbrli:shares</i:measure></i:unit>
   <ix:nonFraction name=\"us-gaap:EarningsPerShareBasic\" contextRef=\"fy\" unitRef=\"usd\" scale=\"3\">1,234</ix:nonFraction>
   <ix:nonFraction name=\"dei:EntityCommonStockSharesOutstanding\" contextRef=\"instant\" unitRef=\"shares\">100</ix:nonFraction>
   </body></html>"
        parsed (xbrl-facts/parse html)
        shares (get-in parsed [:facts :dei :EntityCommonStockSharesOutstanding :units "shares" 0])
        eps (get-in parsed [:facts :us-gaap :EarningsPerShareBasic :units "USD/shares" 0])]
    (is (= 100 (:val shares)))
    (is (= {"us-gaap:StatementClassOfStockAxis" "us-gaap:CommonStockMember"}
           (:dimensions shares)))
    (is (= "2025-12-31" (:end shares)))
    (is (= 1234000 (:val eps)))))

(deftest prefixed-xbrl-root-is-parsed-as-xml
  (let [instance "<xbrli:xbrl xmlns:xbrli=\"http://www.xbrl.org/2003/instance\"
                              xmlns:us-gaap=\"http://fasb.org/us-gaap/2013\">
     <xbrli:context id=\"c\"><xbrli:period><xbrli:instant>2013-12-31</xbrli:instant></xbrli:period></xbrli:context>
     <xbrli:unit id=\"s\"><xbrli:measure>xbrli:shares</xbrli:measure></xbrli:unit>
     <us-gaap:CommonStockSharesOutstanding contextRef=\"c\" unitRef=\"s\">99,000,000</us-gaap:CommonStockSharesOutstanding>
   </xbrli:xbrl>"]
    (is (true? (xbrl/instance-document? instance)))
    (is (= 99000000
           (get-in (xbrl-facts/parse instance)
                   [:facts :us-gaap :CommonStockSharesOutstanding :units "shares" 0 :val])))))

(deftest instance-unit-keeps-its-divide
  (let [instance "<xbrl xmlns=\"http://www.xbrl.org/2003/instance\">
     <context id=\"c\"><period><startDate>2025-01-01</startDate><endDate>2025-12-31</endDate></period></context>
     <unit id=\"u\"><divide>
       <unitNumerator><measure>iso4217:USD</measure></unitNumerator>
       <unitDenominator><measure>xbrli:shares</measure></unitDenominator>
     </divide></unit>
     <us-gaap:EarningsPerShareBasic contextRef=\"c\" unitRef=\"u\">1.25</us-gaap:EarningsPerShareBasic>
   </xbrl>"]
    (is (= 1.25 (get-in (xbrl-facts/parse instance)
                        [:facts :us-gaap :EarningsPerShareBasic :units "USD/shares" 0 :val])))))

(deftest exhibit-fact-uses-cover-context
  (let [cover "<html><body>
               <ix:nonNumeric name=\"dei:EntityCentralIndexKey\">0000311337</ix:nonNumeric>
               <ix:nonNumeric name=\"dei:EntityRegistrantName\">Suncor Energy Inc.</ix:nonNumeric>
               <ix:nonNumeric name=\"dei:DocumentType\">40-F</ix:nonNumeric>
               <ix:nonNumeric name=\"dei:DocumentFiscalYearFocus\">2025</ix:nonNumeric>
               <ix:nonNumeric name=\"dei:DocumentFiscalPeriodFocus\">FY</ix:nonNumeric>
               <xbrli:context id=\"c-1\">
                 <xbrli:period><xbrli:startDate>2025-01-01</xbrli:startDate><xbrli:endDate>2025-12-31</xbrli:endDate></xbrli:period>
               </xbrli:context>
               <xbrli:unit id=\"cad\"><xbrli:measure>iso4217:CAD</xbrli:measure></xbrli:unit>
               </body></html>"
        exhibit "<html><body>
                 <ix:nonFraction name=\"ifrs-full:RevenueFromContractsWithCustomers\" contextRef=\"c-1\" unitRef=\"cad\" scale=\"6\">123</ix:nonFraction>
                 </body></html>"
        parsed (xbrl-facts/parse-all [cover exhibit] {:accn "0001104659-26-020411"})]
    (is (= 311337 (:cik parsed)))
    (is (= "Suncor Energy Inc." (:entityName parsed)))
    (is (= [{:start "2025-01-01"
             :end "2025-12-31"
             :val 123000000
             :accn "0001104659-26-020411"
             :fy 2025
             :fp "FY"
             :form "40-F"}]
           (get-in parsed [:facts :ifrs-full :RevenueFromContractsWithCustomers :units "CAD"])))))

(deftest instance-context-resolves-cover-outstanding
  (let [cover "<html><body>
               <ix:nonNumeric name=\"dei:EntityCentralIndexKey\">0001796073</ix:nonNumeric>
               <ix:nonNumeric name=\"dei:DocumentType\">40-F</ix:nonNumeric>
               <ix:nonNumeric name=\"dei:DocumentFiscalYearFocus\">2026</ix:nonNumeric>
               <ix:nonNumeric name=\"dei:DocumentFiscalPeriodFocus\">FY</ix:nonNumeric>
               <ix:nonFraction name=\"dei:EntityCommonStockSharesOutstanding\" contextRef=\"C_20260430\" unitRef=\"Share\" decimals=\"INF\">351,018,130</ix:nonFraction>
               </body></html>"
        instance "<xbrl>
                  <context id=\"C_20260430\"><period><instant>2026-04-30</instant></period></context>
                  <unit id=\"Share\"><measure>shares</measure></unit>
                  </xbrl>"
        parsed (xbrl-facts/parse-all [cover instance])]
    (is (= [{:end "2026-04-30"
             :val 351018130
             :fy 2026
             :fp "FY"
             :form "40-F"}]
           (get-in parsed [:facts :dei :EntityCommonStockSharesOutstanding :units "shares"])))))

(deftest number-words-scale-to-a-count
  (let [html "<html><body>
              <xbrli:context id=\"c\">
                <xbrli:period><xbrli:instant>2026-01-30</xbrli:instant></xbrli:period>
              </xbrli:context>
              <xbrli:unit id=\"pure\"><xbrli:measure>xbrli:pure</xbrli:measure></xbrli:unit>
              <ix:nonFraction name=\"us-gaap:NumberOfReportingUnits\" contextRef=\"c\" unitRef=\"pure\" format=\"ixt-sec:numwordsen\">one hundred and five</ix:nonFraction>
              </body></html>"
        parsed (xbrl-facts/parse html)]
    (is (= 105 (get-in parsed [:facts :us-gaap :NumberOfReportingUnits :units "pure" 0 :val])))))

(deftest overview-flattens-facts-contexts-dimensions
  (let [o (xbrl-overview/overview [{:text coke-cover :where "cover"}])
        facts (:facts o)
        shares (filterv #(and (= :dei (:taxonomy %))
                              (= :EntityCommonStockSharesOutstanding (:concept %))
                              (= "c-3" (:context-id %)))
                        facts)
        name (first (filter #(= :EntityRegistrantName (:concept %)) facts))
        contexts (into {} (map (juxt :context-id identity)) (:context o))]
    (is (= 2 (count shares)))
    (is (every? #(= "shares" (:unit %)) shares))
    (is (= #{56517334} (set (map :value shares))))
    (is (= [{:axis-taxonomy :us-gaap
             :axis-concept :StatementClassOfStockAxis
             :member-taxonomy :coke
             :member-concept :CommonClassUndefinedMember}]
           (:dimensions (first shares))))
    (is (= "COCA-COLA CONSOLIDATED, INC." (:value name)))
    (is (nil? (:unit name)))
    (is (= {:scheme "http://www.sec.gov/CIK"
            :identifier "0000317540"}
           (:entity (get contexts "c-3"))))
    (is (= {:instant "2026-01-30"} (:period (get contexts "c-3"))))
    (is (= {:us-gaap/StatementClassOfStockAxis :coke/CommonClassUndefinedMember}
           (:dimension (get contexts "c-3"))))
    (is (= {:start "2025-01-01" :end "2025-12-31"}
           (:period (get contexts "c-1"))))
    (is (= {} (:dimension (get contexts "c-1"))))
    (is (= 2 (count (:dimension o))))))

(def typed-cover
  "<html><body>
   <xbrli:context id=\"c-t\">
     <xbrli:entity><xbrli:identifier scheme=\"http://www.sec.gov/CIK\">0000000001</xbrli:identifier></xbrli:entity>
     <xbrli:period><xbrli:instant>2025-12-31</xbrli:instant></xbrli:period>
     <xbrli:scenario>
       <xbrldi:typedMember dimension=\"us-gaap:SomeTypedAxis\"><xbrldi:domain>2025-01-01</xbrldi:domain></xbrldi:typedMember>
     </xbrli:scenario>
   </xbrli:context>
   <xbrli:unit id=\"usd\"><xbrli:measure>iso4217:USD</xbrli:measure></xbrli:unit>
   <ix:nonFraction name=\"us-gaap:Revenue\" contextRef=\"c-t\" unitRef=\"usd\">10</ix:nonFraction>
   <ix:nonFraction name=\"us-gaap:Assets\" contextRef=\"missing\" unitRef=\"usd\">20</ix:nonFraction>
   </body></html>")

(deftest overview-keeps-typed-members-and-dangling-contexts
  (let [o (xbrl-overview/overview [{:text typed-cover :where "x"}])
        typed (first (filter #(= :SomeTypedAxis (:axis-concept %)) (:dimension o)))
        dangling (first (filter #(= :Assets (:concept %)) (:facts o)))
        context (first (filter #(= "c-t" (:context-id %)) (:context o)))]
    (is (= :us-gaap (:axis-taxonomy typed)))
    (is (nil? (:member-taxonomy typed)))
    (is (nil? (:member-concept typed)))
    (is (= "2025-01-01" (:member-value typed)))
    (is (= {:us-gaap/SomeTypedAxis "2025-01-01"} (:dimension context)))
    (is (= [] (:dimensions dangling)))
    (is (= "missing" (:context-id dangling)))))

(deftest facts-with-concepts-moves-dimension-into-entity
  (let [facts (xbrl-overview/facts-with-concepts
               (xbrl-overview/overview [{:text coke-cover :where "cover"}]))
        by-context (fn [id] (first (filter #(= id (:context-id %)) facts)))
        shares (filter #(and (= :EntityCommonStockSharesOutstanding (:concept %))
                             (= "c-3" (:context-id %)))
                       facts)]
    (is (= {:cik "0000317540"
            :dimension {:us-gaap/StatementClassOfStockAxis
                        :coke/CommonClassUndefinedMember}}
           (:entity (by-context "c-3"))))
    (is (= {:cik "0000317540"} (:entity (by-context "c-1"))))
    (is (not (contains? (:entity (by-context "c-1")) :dimension)))
    (is (every? #(not (contains? % :identifier)) facts))
    (is (every? #(not (contains? % :dimension)) facts))
    (is (= [{:axis-taxonomy :us-gaap
             :axis-concept :StatementClassOfStockAxis
             :member-taxonomy :coke
             :member-concept :CommonClassUndefinedMember}]
           (:dimensions (first shares))))))

(deftest facts-with-concepts-handles-typed-and-dangling-contexts
  (let [facts (xbrl-overview/facts-with-concepts
               (xbrl-overview/overview [{:text typed-cover :where "x"}]))
        typed (first (filter #(= :Revenue (:concept %)) facts))
        dangling (first (filter #(= :Assets (:concept %)) facts))]
    (is (= {:cik "0000000001"
            :dimension {:us-gaap/SomeTypedAxis "2025-01-01"}}
           (:entity typed)))
    (is (nil? (:entity dangling)))
    (is (= "missing" (:context-id dangling)))
    (is (nil? (:period dangling)))))

(deftest printable-period-renders-durations-and-instants
  (is (= "2025-01-01-2025-12-31"
         (xbrl-overview/printable-period {:start "2025-01-01" :end "2025-12-31"})))
  (is (= "2026-01-30" (xbrl-overview/printable-period {:instant "2026-01-30"})))
  (is (nil? (xbrl-overview/printable-period nil)))
  (is (nil? (xbrl-overview/printable-period {}))))

(deftest printable-dimension-joins-members-without-axes-and-caps
  (is (= ":a/X, date"
         (xbrl-overview/printable-dimension
          (array-map :a/Axis :a/X :b/TypedAxis "date"))))
  (let [member (keyword "us-gaap" (apply str (repeat 30 "x")))]
    (is (= 20 (count (xbrl-overview/printable-dimension {:us-gaap/Axis member})))))
  (is (nil? (xbrl-overview/printable-dimension nil)))
  (is (nil? (xbrl-overview/printable-dimension {}))))

(defn- rm-r
  [^java.io.File f]
  (when (.isDirectory f)
    (doseq [c (.listFiles f)]
      (rm-r c)))
  (.delete f))

(deftest missing-statement-exhibit-is-skipped
  (let [dir (io/file (System/getProperty "java.io.tmpdir")
                     (str "edgar2-xbrl-" (System/nanoTime)))
        file (io/file dir "cover.htm")]
    (.mkdirs dir)
    (spit file "<html><body><a href=\"ex99d2.htm\">exhibit</a></body></html>")
    (try
      (let [docs (xbrl-loader/load-documents file)]
        (is (= 1 (count docs)))
        (is (= (str file) (:where (first docs)))))
      (finally (rm-r dir)))))
