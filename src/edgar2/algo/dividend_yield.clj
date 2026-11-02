(ns edgar2.algo.dividend-yield
  "Top fraction of positive dividend yields, highest yield first.
  The fraction and Min mc are screen parameters."
  (:require [edgar2.algo.cutoff :as cutoff]
            [edgar2.report.screen :as report]
            [edgar2.screen.universe :as uni]
            [edgar2.screen.util :as util]))

(def columns
  [{:key :ticker :label "Ticker"}
   {:key :market-cap :label "Market cap" :num? true}
   {:key :pe :label "PE" :num? true :decimals 1}
   {:key :ev-ebit :label "EV/EBIT" :num? true :decimals 1}
   {:key :dividend-yield :label "Dividend yield" :num? true :decimals 1}
   {:key :buyback-yield :label "Buyback yield" :num? true :decimals 1}
   {:key :dividend-coverage :label "Dividend coverage" :num? true :decimals 1}])

(def yield-fraction
  (util/choice-param :yield-fraction "Yield fraction"
                     cutoff/fraction-choices 0.10))

(def notes
  ["buyback-yield = stock repurchase / market cap"
   "dividend-coverage = net income / dividends paid"
   "market-cap in $ millions"])

(def sort-line
  "sorted by dividend-yield descending")

(defn calculate
  [items params]
  (let [min-mc (util/param-value params uni/min-mc)
        yield-p (util/param-value params yield-fraction)
        rows (uni/calculate-stats items)
        with-dy (filterv #(and (number? (:dividend-yield %))
                               (pos? (double (:dividend-yield %))))
                         (report/at-least-mc rows min-mc))
        cut (when (seq with-dy)
              (cutoff/cutoff-min-top
               (cutoff/numeric :dividend-yield with-dy) yield-p))]
    (if-not cut
      {:rows []
       :lines ["no positive dividend-yield"]}
      (let [steps [(uni/min-mc-step min-mc)
                   {:label "positive dividend-yield"
                    :pred (util/positive-at :dividend-yield)}
                   {:label (format "dividend-yield >= %.1f%% (top %d%%, positive only)"
                                   (double cut)
                                   (cutoff/pct-label yield-p))
                    :pred (util/at-least :dividend-yield cut)}]
            result (util/apply-and-pipeline rows steps)
            hits (vec (sort-by :dividend-yield > (:tickers result)))]
        {:rows (mapv report/table-row hits)
         :lines (conj (into (util/report-pipeline-info result) notes)
                      sort-line)}))))

(def screen
  {:id :dividend-yield
   :title "Dividend yield"
   :description ["Top fraction of positive dividend yields, highest yield first. The fraction and Min mc are parameters."]
   :params [yield-fraction uni/min-mc]
   :columns columns
   :universe uni/stats-universe-all
   :calculate calculate
   :stamp uni/stats-stamp})
