(ns edgar2.db.ticker-test
  (:require [clojure.test :refer [deftest is]]
            [edgar2.db.ticker :as db-ticker]))

(def ^:private sample-rows
  [{:cik "0000001750" :ticker "AIR" :name "  AAR Corp  " :exchange "NYSE"}
   {:cik "0000001750" :ticker "" :name "AAR Class A" :exchange "NYSE"}
   {:cik "0000001750" :ticker "AIR-A" :name "AAR Corp A" :exchange "AMEX"}
   {:cik "0000001750" :ticker "AIR" :name "AAR Corp" :exchange "NYSE"}
   {:cik "0000001800" :ticker "ABT" :name "Abbott Laboratories" :exchange "NYSE"}
   {:cik "0000002809" :ticker "AEM" :exchange "NYSE"}
   {:cik "" :ticker "NOCIK" :name "No Cik" :exchange "NASDAQ"}])

(defn- with-sample
  [f]
  (with-redefs [db-ticker/ticker-index
                (delay (db-ticker/load-ticker-index sample-rows))]
    (f)))

(deftest ticker->cik-lookups
  (with-sample
    (fn []
      (is (= "0000001750" (db-ticker/ticker->cik "AIR")))
      (is (= "0000001750" (db-ticker/ticker->cik " air ")))
      (is (= "0000001750" (db-ticker/ticker->cik "air")))
      (is (= "0000001800" (db-ticker/ticker->cik "ABT")))
      (is (nil? (db-ticker/ticker->cik "NOCIK")))
      (is (nil? (db-ticker/ticker->cik "NOPE")))
      (is (nil? (db-ticker/ticker->cik nil)))
      (is (nil? (db-ticker/ticker->cik "")))
      (is (nil? (db-ticker/ticker->cik "  "))))))

(deftest ticker->name-lookups
  (with-sample
    (fn []
      (is (= "AAR Corp" (db-ticker/ticker->name "AIR")))
      (is (= "AAR Corp" (db-ticker/ticker->name " air ")))
      (is (= "Abbott Laboratories" (db-ticker/ticker->name "abt")))
      (is (nil? (db-ticker/ticker->name "AEM")))
      (is (nil? (db-ticker/ticker->name "NOPE")))
      (is (nil? (db-ticker/ticker->name nil)))
      (is (nil? (db-ticker/ticker->name "  "))))))

(deftest tickers-for-cik-keeps-file-order
  (with-sample
    (fn []
      (is (= ["AIR" "AIR-A"] (db-ticker/tickers-for-cik "1750")))
      (is (= ["AIR" "AIR-A"] (db-ticker/tickers-for-cik "0000001750")))
      (is (= ["ABT"] (db-ticker/tickers-for-cik "1800")))
      (is (= [] (db-ticker/tickers-for-cik "9999")))
      (is (= [] (db-ticker/tickers-for-cik nil)))
      (is (= [] (db-ticker/tickers-for-cik "  "))))))

(deftest primary-ticker-for-cik-uses-first-listed-row
  (with-sample
    (fn []
      (is (= {:ticker "AIR" :exchange "NYSE"}
             (db-ticker/primary-ticker-for-cik "1750")))
      (is (= {:ticker "AIR" :exchange "NYSE"}
             (db-ticker/primary-ticker-for-cik "0000001750")))
      (is (= {:ticker "ABT" :exchange "NYSE"}
             (db-ticker/primary-ticker-for-cik "1800")))
      (is (nil? (db-ticker/primary-ticker-for-cik "9999")))
      (is (nil? (db-ticker/primary-ticker-for-cik nil)))
      (is (nil? (db-ticker/primary-ticker-for-cik "  "))))))

(deftest load-ticker-index-with-no-rows
  (is (= {:rows-by-cik {}
          :cik-by-ticker {}
          :name-by-ticker {}}
         (db-ticker/load-ticker-index []))))
