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

(deftest dei-outstanding-wins-over-shares-outstanding
  (is (= {"Common" {:weighted nil :outstanding 100}}
         (shares/share-summary
          {:facts {:dei {:EntityCommonStockSharesOutstanding (shares-fact 100)}
                   :us-gaap {:SharesOutstanding
                             {:units {:shares [{:end "2026-09-04"
                                                :val 379236005
                                                :filed "2026-09-04"}]}}}}}))))
