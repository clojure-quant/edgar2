(ns edgar2.algo.cheap-consistent-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [edgar2.algo.cheap-consistent :as consistent]
            [edgar2.algo.cheap-consistent-margin :as margin]
            [edgar2.report.screen :as report]))

(defn- item
  [ticker overrides]
  {:ticker ticker
   :row (merge {:ticker ticker
                :price-earnings 20
                :revenue-score 80
                :operating-income-score 80
                :avg-profit-margin 0.50
                :marketcap 200e6}
               overrides)})

(defn- series
  []
  (mapv (fn [i]
          (let [pe (+ 6 (* 6 i))
                score (- 95 (* 5 i))]
            (item (str "R" i)
                  {:price-earnings pe
                   :revenue-score score
                   :operating-income-score score
                   :avg-profit-margin (/ score 100.0)})))
        (range 10)))

(defn- margin-series
  []
  (mapv (fn [i]
          (let [pe (+ 6 (* 6 i))
                high? (< i 5)]
            (item (str "R" i)
                  {:price-earnings pe
                   :revenue-score (if high? 95 10)
                   :operating-income-score (if high? 95 10)
                   :avg-profit-margin (if high? 0.95 0.10)})))
        (range 10)))

(defn- calculate
  [f items params]
  (with-redefs [report/load-stats (fn [] (mapv :row items))]
    (f (mapv :ticker items) params)))

(deftest default-fractions-select-the-bottom-and-top-third
  (let [{:keys [rows lines]} (calculate consistent/calculate (series) {})]
    (is (= ["R0" "R1" "R2"] (mapv :ticker rows)))
    (is (str/starts-with? (first lines) "universe"))
    (is (some #(str/starts-with? % "mc >= 100 ($ millions)") lines))
    (is (some #(str/starts-with? % "positive price-earnings") lines))
    (is (some #(str/starts-with? % "price-earnings <= 18.0 (bottom 30%)")
              lines))
    (is (some #(str/starts-with? % "revenue-score >= 85.0 (top 30%)") lines))
    (is (some #(str/starts-with? % "operating-income-score >= 85.0 (top 30%)")
              lines))))

(deftest fractions-change-the-selected-rows
  (is (= ["R0" "R1" "R2" "R3" "R4"]
         (->> (calculate consistent/calculate (series)
                                    {:pe-fraction 0.50
                                     :revenue-score-fraction 0.50
                                     :operating-income-score-fraction 0.50})
              :rows
              (mapv :ticker))))
  (is (= ["R0" "R1"]
         (->> (calculate consistent/calculate (series)
                                    {:revenue-score-fraction 0.20})
              :rows
              (mapv :ticker))))
  (is (= ["R0" "R1"]
         (->> (calculate consistent/calculate (series)
                                    {:operating-income-score-fraction 0.20})
              :rows
              (mapv :ticker)))))

(deftest min-mc-is-a-pipeline-stage
  (let [items (assoc-in (series) [0 :row :marketcap] 50e6)
        {:keys [rows lines]} (calculate consistent/calculate items {})]
    (is (= ["R1" "R2" "R3"] (mapv :ticker rows)))
    (is (some #(re-matches #"mc >= 100 \(\$ millions\)\s+9" %) lines)))
  (let [items (assoc-in (series) [0 :row :marketcap] 50e6)
        {:keys [rows lines]} (calculate consistent/calculate items {:min-mc 0})]
    (is (= ["R0" "R1" "R2"] (mapv :ticker rows)))
    (is (some #(str/starts-with? % "mc >= 0 ($ millions)") lines))))

(deftest no-positive-pe-short-circuits
  (let [items (mapv #(item % {:price-earnings -1}) ["A" "B"])
        {:keys [rows lines]} (calculate consistent/calculate items {})]
    (is (= [] rows))
    (is (= ["no positive PE"] lines))))

(deftest margin-screen-defaults-to-a-fifty-percent-pe-cut
  (let [{:keys [rows lines]} (calculate margin/calculate (margin-series) {})]
    (is (= ["R0" "R1" "R2" "R3" "R4"] (mapv :ticker rows)))
    (is (some #(str/starts-with? % "price-earnings <= 30.0 (bottom 50%)")
              lines))
    (is (some #(str/starts-with? % "avg-profit-margin >= 95.00% (top 30%)")
              lines))))
