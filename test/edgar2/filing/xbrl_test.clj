(ns edgar2.filing.xbrl-test
  (:require [clojure.test :refer [deftest is]]
            [edgar2.filing.xbrl :as xbrl]))

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
  (let [parsed (xbrl/parse coke-cover filing-meta)
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

(deftest number-words-scale-to-a-count
  (let [html "<html><body>
              <xbrli:context id=\"c\">
                <xbrli:period><xbrli:instant>2026-01-30</xbrli:instant></xbrli:period>
              </xbrli:context>
              <xbrli:unit id=\"pure\"><xbrli:measure>xbrli:pure</xbrli:measure></xbrli:unit>
              <ix:nonFraction name=\"us-gaap:NumberOfReportingUnits\" contextRef=\"c\" unitRef=\"pure\" format=\"ixt-sec:numwordsen\">one hundred and five</ix:nonFraction>
              </body></html>"
        parsed (xbrl/parse html)]
    (is (= 105 (get-in parsed [:facts :us-gaap :NumberOfReportingUnits :units "pure" 0 :val])))))
