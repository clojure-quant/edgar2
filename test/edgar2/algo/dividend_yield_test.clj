(ns edgar2.algo.dividend-yield-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [edgar2.algo.dividend-yield :as dividend]
            [edgar2.report.screen :as report]))

(defn- item
  [ticker dividend-yield]
  {:ticker ticker
   :row {:ticker ticker
         :dividend-yield dividend-yield
         :marketcap 200e6}})

(defn- series
  []
  (mapv (fn [i]
          (item (str "R" i) (if (= 9 i) 0 (- 10 i))))
        (range 10)))

(defn- calculate
  [items params]
  (with-redefs [report/load-stats (fn [] (mapv :row items))]
    (dividend/calculate (mapv :ticker items) params)))

(deftest default-top-ten-percent-selects-the-highest-yield
  (let [{:keys [rows lines]} (calculate (series) {})]
    (is (= ["R0"] (mapv :ticker rows)))
    (is (str/starts-with? (first lines) "universe"))
    (is (some #(re-matches #"positive dividend-yield\s+9" %) lines))
    (is (some #(str/starts-with? %
                                 "dividend-yield >= 10.0% (top 10%, positive only)")
              lines))
    (is (= dividend/sort-line (last lines)))))

(deftest yield-fraction-changes-the-cut
  (let [{:keys [rows lines]} (calculate (series)
                                                  {:yield-fraction 0.30})]
    (is (= ["R0" "R1" "R2"] (mapv :ticker rows)))
    (is (some #(str/starts-with? %
                                 "dividend-yield >= 8.0% (top 30%, positive only)")
              lines))))

(deftest min-mc-is-a-pipeline-stage
  (let [items (assoc-in (series) [0 :row :marketcap] 50e6)
        {:keys [rows lines]} (calculate items {})]
    (is (= ["R1"] (mapv :ticker rows)))
    (is (some #(re-matches #"mc >= 100 \(\$ millions\)\s+9" %) lines))))

(deftest no-positive-dividend-yield-short-circuits
  (let [items [(item "ZERO" 0) (item "NEG" -1) (item "NIL" nil)]
        {:keys [rows lines]} (calculate items {})]
    (is (= [] rows))
    (is (= ["no positive dividend-yield"] lines))))
