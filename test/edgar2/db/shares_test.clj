(ns edgar2.db.shares-test
  (:require [clojure.test :refer [deftest is]]
            [edgar2.db.shares :as shares]
            [edgar2.download.dataset :as dataset]))

(def ^:private sample-rows
  [{:cik "0000001750"
    :filing-date "2026-07-22"
    :form-type "10-K"
    :shares {"Common" {:weighted 38300000 :outstanding 39892472}}
    :ticker "AIR"}
   {:cik "0000001800"
    :filing-date "2026-02-20"
    :form-type "10-K"
    :shares {"Common" {:weighted 1 :outstanding nil}}
    :ticker "ABT"}
   {:cik "0000002809" :shares {} :ticker "AEM"}
   {:cik "0000004457"
    :filing-date "2026-05-27"
    :form-type "10-K"
    :shares {"Common" {:weighted 176470092 :outstanding 176470092}
             "Class A" {:weighted 19607788 :outstanding 19607788}}
    :ticker "UHAL"}
   {:cik "0000317540"
    :filing-date "2026-02-17"
    :form-type "10-K"
    :shares {"Common" {:weighted nil :outstanding 56517334}
             "Class B" {:weighted 10047000 :outstanding nil}}
    :ticker "COKE"}])

(defn- with-sample
  [f]
  (with-redefs [dataset/read-rows (fn [_] sample-rows)
                shares/shares-index (delay (shares/load-shares-index))]
    (f)))

(deftest no-xbrl-flags-the-share-row
  (with-redefs [dataset/read-rows
                (fn [_]
                  [{:cik "0000009999"
                    :ticker "NOX"
                    :filing-date "2026-06-25"
                    :form-type "40-F"
                    :shares {}
                    :xbrl false}])
                shares/shares-index (delay (shares/load-shares-index))]
    (is (= nil 
         (shares/get-shares-ticker "NOX")))))

(deftest share-info-by-cik-and-ticker
  (with-sample
    (fn []
      (is (= 39892472
             (shares/get-shares-cik "1750")))
      (is (= 39892472
             (shares/get-shares-cik "0000001750")))
      (is (= 39892472
           (shares/get-shares-ticker "AIR")))
      (is (= 39892472
             (shares/get-shares-ticker "air")))
      (is (= 39892472
             (shares/get-shares-ticker " air ")))
      (is (= nil
             (shares/get-shares-cik "1800")))
      (is (= nil
             (shares/get-shares-ticker "ABT")))
      (is (= nil
             (shares/get-shares-cik "2809")))
      (is (=  nil
              (shares/get-shares-ticker "AEM")))
      (is (=  196077880
              (shares/get-shares-ticker "UHAL")))
      (is (=  196077880
              (shares/get-shares-cik 4457)))
      (is (= 56517334
             (shares/get-shares-ticker "COKE")))
      (is (= 56517334
             (shares/get-shares-cik "317540")))
      (is (nil? (shares/get-shares-cik nil)))
      (is (nil? (shares/get-shares-cik "  ")))
      (is (nil? (shares/get-shares-ticker nil)))
      (is (nil? (shares/get-shares-ticker "  ")))
      (is (nil? (shares/get-shares-ticker "NOPE"))))))

(deftest share-tickers-lists-file-tickers
  (with-sample
    (fn []
      (is (= #{"AIR" "ABT" "AEM" "UHAL" "COKE"}
             (shares/share-tickers))))))
