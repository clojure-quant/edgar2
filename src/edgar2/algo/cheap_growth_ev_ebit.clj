(ns edgar2.algo.cheap-growth-ev-ebit
  "Bottom 30% EV/EBIT, top 30% sales growth, top 50% ROA."
  (:require [edgar2.algo.cheap-growth :as cheap]
            [edgar2.report.screen :as report]
            [edgar2.screen.universe :as uni]))

(defn calculate
  [items _params]
  (let [result (cheap/cheap-growth-on (mapv :row items)
                                      :ev-ebit
                                      #(and (number? (:ebit %))
                                            (pos? (double (:ebit %)))))]
    {:rows (mapv report/table-row (:rows result))
     :lines (cheap/cheap-lines
             "EV/EBIT" "positive EBIT only" result
             ["  EV (USD millions) = market cap + interest-bearing debt + preferred + NCI - cash"
              "  EBIT = operating income, else net income + interest + tax"])}))

(def screen
  {:id :cheap-growth-ev-ebit
   :title "Cheap growth EV/EBIT"
   :description ["Bottom 30% EV/EBIT (positive EBIT only), top 30% 7y mean YoY sales growth, top 50% ROA."]
   :params [uni/min-mc]
   :columns cheap/columns
   :universe uni/stats
   :calculate calculate
   :stamp uni/stats-stamp})
