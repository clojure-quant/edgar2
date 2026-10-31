(ns edgar2.db.shares-test
  (:require [clojure.test :refer [deftest is]]
            [edgar2.db.shares :as shares]
            [edgar2.download.dataset :as dataset]))

(def ^:private sample-rows
  [{:cik "0000001750"
    :shares {"Common" {:weighted 38300000 :outstanding 39892472}}
    :ticker "AIR"}
   {:cik "0000001800"
    :shares {"Common" {:weighted 1 :outstanding nil}}
    :ticker "ABT"}
   {:cik "0000002809" :shares {} :ticker "AEM"}
   {:cik "0000004457"
    :shares {"Common" {:weighted 176470092 :outstanding 176470092}
             "Class A" {:weighted 19607788 :outstanding 19607788}}
    :ticker "UHAL"}])

(deftest common-outstanding-by-cik-and-ticker
  (with-redefs [dataset/read-rows (fn [_] sample-rows)
                shares/shares-index (delay (shares/load-shares-index))]
    (is (= 39892472 (shares/get-shares-cik "1750")))
    (is (= 39892472 (shares/get-shares-cik "0000001750")))
    (is (= 39892472 (shares/get-shares-ticker "AIR")))
    (is (= 39892472 (shares/get-shares-ticker "air")))
    (is (= 39892472 (shares/get-shares-ticker " air ")))
    (is (nil? (shares/get-shares-cik "1800")))
    (is (nil? (shares/get-shares-ticker "ABT")))
    (is (nil? (shares/get-shares-cik "2809")))
    (is (nil? (shares/get-shares-ticker "AEM")))
    (is (= 176470092 (shares/get-shares-ticker "UHAL")))
    (is (= 176470092 (shares/get-shares-cik 4457)))
    (is (nil? (shares/get-shares-cik nil)))
    (is (nil? (shares/get-shares-cik "  ")))
    (is (nil? (shares/get-shares-ticker nil)))
    (is (nil? (shares/get-shares-ticker "  ")))
    (is (nil? (shares/get-shares-ticker "NOPE")))))
