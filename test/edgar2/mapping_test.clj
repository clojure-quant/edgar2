(ns edgar2.mapping-test
  (:require [clojure.test :refer [deftest is]]
            [edgar2.db.mapping :as mapping]))

(def ^:private sample-rows
  [{:ticker "A.US" :cusip "00846U101"}
   {:ticker "AA.US" :cusip "013872106"}
   {:ticker "BRK.B.US" :cusip "084670702"}
   {:ticker "AIV.US" :cusip "03748R101"}
   {:ticker "AIV.US" :cusip "03748R796"}
   {:ticker "AIV.US" :cusip "03748R101"}
   {:ticker "NOCUSIP.US" :isin "US0000000000"}
   {:cusip "000000000"}])

(deftest cusip-and-ticker-lookups
  (let [v (find-var 'edgar2.db.mapping/indexes)
        original @v]
    (alter-var-root v (constantly (delay (#'mapping/build-indexes sample-rows))))
    (try
      (is (= "A" (mapping/cusip->ticker "00846U101")))
      (is (= "A" (mapping/cusip->ticker "00846u101")))
      (is (= "BRK.B" (mapping/cusip->ticker "084670702")))
      (is (nil? (mapping/cusip->ticker "missing")))
      (is (nil? (mapping/cusip->ticker nil)))
      (is (nil? (mapping/cusip->ticker "  ")))
      (is (= ["00846U101"] (mapping/ticker->cusip "A")))
      (is (= ["00846U101"] (mapping/ticker->cusip "a")))
      (is (= ["00846U101"] (mapping/ticker->cusip "A.US")))
      (is (= ["084670702"] (mapping/ticker->cusip "BRK.B")))
      (is (= ["013872106"] (mapping/ticker->cusip "AA.US")))
      (is (= ["03748R101" "03748R796"] (mapping/ticker->cusip "AIV")))
      (is (= [] (mapping/ticker->cusip "NOCUSIP")))
      (is (= [] (mapping/ticker->cusip "missing")))
      (is (nil? (mapping/ticker->cusip nil)))
      (is (nil? (mapping/ticker->cusip "  ")))
      (finally
        (alter-var-root v (constantly original))))))
