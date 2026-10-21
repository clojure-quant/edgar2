(ns edgar2.facts-ds-tak-test
  "TAK: forward FY window on a 20-F must not displace the completed annual."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [edgar.xbrl :as xbrl]
            [edgar2.financials.facts-ds :as facts-ds]
            [jsonista.core :as json]
            [tech.v3.dataset :as ds]))

(def tak-facts-path
  "test/edgar2/facts/TAK-facts.json")

(defn- load-tak-facts
  []
  (let [f (io/file tak-facts-path)]
    (when-not (.exists f)
      (throw (ex-info "Missing TAK facts fixture" {:path tak-facts-path})))
    (xbrl/facts->dataset
     (json/read-value (slurp f) json/keyword-keys-object-mapper))))

(deftest annual-periods-tak-no-future-end
  "20-F filed 2025-06-25 tags a forward ApplicableTaxRate window ending
  2026-03-31. annual-periods must keep the completed FY ending 2025-03-31."
  (let [dss (load-tak-facts)
        rows (vec (ds/rows (facts-ds/annual-periods dss) {:nil-missing? true}))
        newest (last (sort-by #(str (:end %)) rows))
        filing-2025 (filterv #(= "2025-06-25" (str (:filed %))) rows)]
    (testing "real FY ending 2025-03-31 is present for that filing"
      (is (some #(= "2025-03-31" (str (:end %))) filing-2025)))
    (testing "newest annual period is the completed year, not the forward stub"
      (is (= "2025-03-31" (str (:end newest))))
      (is (not= "2026-03-31" (str (:end newest)))))
    (testing "no annual period ends after its filing date"
      (is (every? #(not (pos? (compare (str (:end %)) (str (:filed %)))))
                  rows)))))
