(ns edgar2.algo.high-revenue-growth-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [edgar2.algo.high-revenue-growth :as high]
            [edgar2.report.screen :as report]))

(defn- item
  [ticker sales-growth]
  {:ticker ticker
   :row {:ticker ticker
         :sales-growth-avg-yoy sales-growth
         :marketcap 200e6}})

(defn- series
  []
  (mapv (fn [i]
          (item (str "R" i) (- 95 (* 5 i))))
        (range 10)))

(defn- calculate
  [items params]
  (with-redefs [report/load-stats (fn [] (mapv :row items))]
    (high/calculate (mapv :ticker items) params)))

(deftest default-keeps-everything-sorted-by-growth
  (let [{:keys [rows lines]} (calculate (series) {})]
    (is (= (mapv #(str "R" %) (range 10)) (mapv :ticker rows)))
    (is (str/starts-with? (first lines) "universe"))
    (is (some #(str/starts-with? % "mc >= 100 ($ millions)") lines))
    (is (some #(str/starts-with? % "sales-growth-avg-yoy numeric") lines))
    (is (= "top 50 by 7-year mean YoY sales-growth (showing 10)" (last lines)))))

(deftest top-n-truncates-after-sorting
  (let [{:keys [rows lines]} (calculate (series) {:top-n 3})]
    (is (= ["R0" "R1" "R2"] (mapv :ticker rows)))
    (is (= "top 3 by 7-year mean YoY sales-growth (showing 3)" (last lines)))))

(deftest numeric-stage-drops-missing-growth
  (let [items (assoc-in (series) [9 :row :sales-growth-avg-yoy] nil)
        {:keys [rows lines]} (calculate items {:top-n 10})]
    (is (= (mapv #(str "R" %) (range 9)) (mapv :ticker rows)))
    (is (some #(re-matches #"sales-growth-avg-yoy numeric\s+9" %) lines))))

(deftest min-mc-is-a-pipeline-stage
  (let [items (assoc-in (series) [0 :row :marketcap] 50e6)
        {:keys [rows lines]} (calculate items {:top-n 3})]
    (is (= ["R1" "R2" "R3"] (mapv :ticker rows)))
    (is (some #(re-matches #"mc >= 100 \(\$ millions\)\s+9" %) lines))))
