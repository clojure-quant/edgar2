(ns edgar2.algo.dividend-yield
  "Top 10% of positive dividend yields, highest yield first."
  (:require [edgar2.algo.cutoff :as cutoff]
            [edgar2.report.screen :as report]
            [edgar2.screen.universe :as uni]))

(def columns
  [{:key :ticker :label "Ticker"}
   {:key :market-cap :label "Market cap" :num? true}
   {:key :pe :label "PE" :num? true :decimals 1}
   {:key :ev-ebit :label "EV/EBIT" :num? true :decimals 1}
   {:key :dividend-yield :label "Dividend yield" :num? true :decimals 1}
   {:key :dividend-coverage :label "Dividend coverage" :num? true :decimals 1}])

(defn- yield-screen [rows]
  (let [with-dy (filterv #(and (number? (:dividend-yield %))
                               (pos? (double (:dividend-yield %))))
                         rows)
        cut (when (seq with-dy)
              (cutoff/cutoff-min-top (cutoff/numeric :dividend-yield with-dy) 0.10))
        hits (if cut
               (->> with-dy
                    (filter #(>= (double (:dividend-yield %)) (double cut)))
                    (sort-by :dividend-yield >)
                    vec)
               [])]
    {:cut cut
     :n (count with-dy)
     :rows hits}))

(defn- yield-lines [screen]
  (if-not (:cut screen)
    ["  no positive dividend-yield"]
    [(format "  top 10%% dividend-yield (n=%d, >= %.1f%%, positive only)"
             (:n screen) (double (:cut screen)))
     "  sorted by dividend-yield descending"
     "  dividend-coverage = net income / dividends paid"
     "  market-cap in $ millions"
     (format "  %d names" (count (:rows screen)))]))

(defn calculate
  [items _params]
  (let [result (yield-screen (mapv :row items))]
    {:rows (mapv report/table-row (:rows result))
     :lines (yield-lines result)}))

(def screen
  {:id :dividend-yield
   :title "Dividend yield"
   :description ["Top 10% of positive dividend yields, highest yield first."]
   :params [uni/min-mc]
   :columns columns
   :universe uni/stats
   :calculate calculate
   :stamp uni/stats-stamp})
