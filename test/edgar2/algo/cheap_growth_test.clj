(ns edgar2.algo.cheap-growth-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [edgar2.algo.cheap-growth :as cheap]
            [edgar2.algo.cheap-growth-ev-ebit :as ev-ebit]
            [edgar2.report.screen :as report]))

(defn- item
  [ticker overrides]
  {:ticker ticker
   :row (merge {:ticker ticker
                :price-earnings 20
                :ev-ebit 20
                :ebit 10
                :sales-growth-avg-yoy 50
                :return-on-assets 0.50
                :marketcap 200e6}
               overrides)})

(defn- series
  []
  (mapv (fn [i]
          (let [pe (+ 6 (* 6 i))
                growth (- 95 (* 5 i))]
            (item (str "R" i)
                  {:price-earnings pe
                   :ev-ebit pe
                   :sales-growth-avg-yoy growth
                   :return-on-assets (/ growth 100.0)})))
        (range 10)))

(defn- calculate
  [f items params]
  (with-redefs [report/load-stats (fn [] (mapv :row items))]
    (f (mapv :ticker items) params)))

(deftest default-fractions-select-cheap-growth-names
  (let [{:keys [rows lines]} (calculate cheap/calculate (series) {})]
    (is (= ["R0" "R1" "R2"] (mapv :ticker rows)))
    (is (str/starts-with? (first lines) "universe"))
    (is (some #(str/starts-with? % "mc >= 100 ($ millions)") lines))
    (is (some #(str/starts-with? % "positive price-earnings") lines))
    (is (some #(str/starts-with? % "price-earnings <= 18.0 (bottom 30%)")
              lines))
    (is (some #(str/starts-with? % "sales-growth-avg-yoy >= 85.0 (top 30%)")
              lines))
    (is (some #(str/starts-with? % "return-on-assets >= 75.00% (top 50%)")
              lines))))

(deftest fractions-change-the-selected-rows
  (is (= ["R0" "R1"]
         (->> (calculate cheap/calculate (series)
                               {:pe-fraction 0.50
                                :roa-fraction 0.20})
              :rows
              (mapv :ticker)))))

(deftest no-positive-pe-short-circuits
  (let [items (mapv #(item % {:price-earnings -1}) ["A" "B"])
        {:keys [rows lines]} (calculate cheap/calculate items {})]
    (is (= [] rows))
    (is (= ["no positive PE"] lines))))

(deftest ev-ebit-keep-drops-non-positive-ebit
  (let [items (-> (series)
                  (assoc-in [0 :row :ebit] nil))
        {:keys [rows lines]} (calculate ev-ebit/calculate items {})]
    (is (= ["R1" "R2"] (mapv :ticker rows)))
    (is (some #(str/starts-with? % "positive EBIT") lines))
    (is (str/starts-with? (last lines) "EBIT ="))))

(deftest ev-ebit-fraction-changes-the-cut
  (let [{:keys [lines]} (calculate ev-ebit/calculate (series) {:ev-ebit-fraction 0.50})]
    (is (some #(str/starts-with? % "ev-ebit <= 30.0 (bottom 50%)") lines))))
