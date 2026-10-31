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

(deftest share-info-by-cik-and-ticker
  (with-sample
    (fn []
      (is (= {:cik "0000001750"
              :filing-date "2026-07-22"
              :form-type "10-K"
              :weighted 38300000
              :outstanding 39892472}
             (shares/get-shares-cik "1750")))
      (is (= {:cik "0000001750"
              :filing-date "2026-07-22"
              :form-type "10-K"
              :weighted 38300000
              :outstanding 39892472}
             (shares/get-shares-cik "0000001750")))
      (is (= {:cik "0000001750"
              :filing-date "2026-07-22"
              :form-type "10-K"
              :weighted 38300000
              :outstanding 39892472}
             (shares/get-shares-ticker "AIR")))
      (is (= {:cik "0000001750"
              :filing-date "2026-07-22"
              :form-type "10-K"
              :weighted 38300000
              :outstanding 39892472}
             (shares/get-shares-ticker "air")))
      (is (= {:cik "0000001750"
              :filing-date "2026-07-22"
              :form-type "10-K"
              :weighted 38300000
              :outstanding 39892472}
             (shares/get-shares-ticker " air ")))
      (is (= {:cik "0000001800"
              :filing-date "2026-02-20"
              :form-type "10-K"
              :weighted 1
              :outstanding 0}
             (shares/get-shares-cik "1800")))
      (is (= {:cik "0000001800"
              :filing-date "2026-02-20"
              :form-type "10-K"
              :weighted 1
              :outstanding 0}
             (shares/get-shares-ticker "ABT")))
      (is (= {:cik "0000002809"
              :filing-date nil
              :form-type nil
              :weighted 0
              :outstanding 0}
             (shares/get-shares-cik "2809")))
      (is (= {:cik "0000002809"
              :filing-date nil
              :form-type nil
              :weighted 0
              :outstanding 0}
             (shares/get-shares-ticker "AEM")))
      (is (= {:cik "0000004457"
              :filing-date "2026-05-27"
              :form-type "10-K"
              :weighted 196077880
              :outstanding 196077880}
             (shares/get-shares-ticker "UHAL")))
      (is (= {:cik "0000004457"
              :filing-date "2026-05-27"
              :form-type "10-K"
              :weighted 196077880
              :outstanding 196077880}
             (shares/get-shares-cik 4457)))
      (is (= {:cik "0000317540"
              :filing-date "2026-02-17"
              :form-type "10-K"
              :weighted 10047000
              :outstanding 56517334}
             (shares/get-shares-ticker "COKE")))
      (is (= {:cik "0000317540"
              :filing-date "2026-02-17"
              :form-type "10-K"
              :weighted 10047000
              :outstanding 56517334}
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
