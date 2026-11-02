(ns edgar2.db.sector-test
  (:require [clojure.test :refer [deftest is]]
            [edgar2.db.sector :as sector]))

(def ^:private rows
  [{:sector "B" :industry "B2" :subindustry "B2b" :sic "0002"}
   {:sector "A" :industry "A1" :subindustry "A1a" :sic "0001"}
   {:sector "A" :industry "A1" :subindustry "A1b" :sic "0003"}
   {:sector "A" :industry "A2" :subindustry "A2a" :sic "0004"}])

(deftest sectors-are-distinct-and-sorted
  (is (= ["A" "B"] (sector/sectors rows)))
  (is (= ["A"] (sector/sectors [{:sector "A"} {:sector "A"}]))))

(deftest industries-require-a-sector
  (is (= ["A1" "A2"] (sector/industries rows "A")))
  (is (= ["B2"] (sector/industries rows "B")))
  (is (= [] (sector/industries rows nil)))
  (is (= [] (sector/industries rows "Z"))))

(deftest subindustries-require-both-parents
  (is (= ["A1a" "A1b"] (sector/subindustries rows "A" "A1")))
  (is (= [] (sector/subindustries rows nil "A1")))
  (is (= [] (sector/subindustries rows "A" nil)))
  (is (= [] (sector/subindustries rows "B" "A1"))))

(deftest sic-rows-loads-the-resource
  (let [loaded (sector/sic-rows)]
    (is (seq loaded))
    (is (every? #(and (:sector %) (:industry %) (:subindustry %)) loaded))))
