(ns edgar2.algo.cheap-growth-ev-ebit
  "Bottom fraction of EV/EBIT, top fractions of sales growth and ROA.
  Each fraction and Min mc is a screen parameter."
  (:require [edgar2.algo.cheap-growth :as cheap]
            [edgar2.algo.cutoff :as cutoff]
            [edgar2.report.screen :as report]
            [edgar2.screen.universe :as uni]
            [edgar2.screen.util :as util]))

(def ev-ebit-fraction
  (util/choice-param :ev-ebit-fraction "EV/EBIT fraction"
                     cutoff/fraction-choices 0.30))

(def notes
  ["EV (USD millions) = market cap + interest-bearing debt + preferred + NCI - cash"
   "EBIT = operating income, else net income + interest + tax"])

(defn calculate
  [items params]
  (let [result (cheap/cheap-growth-on
                (uni/calculate-stats items)
                :ev-ebit
                {:keep? #(and (number? (:ebit %))
                              (pos? (double (:ebit %))))
                 :min-mc (util/param-value params uni/min-mc)
                 :cheap-fraction (util/param-value params ev-ebit-fraction)
                 :growth-fraction (util/param-value params cheap/sales-growth-fraction)
                 :roa-fraction (util/param-value params cheap/roa-fraction)})]
    {:rows (mapv report/table-row (:rows result))
     :lines (into (:lines result) notes)}))

(def screen
  {:id :cheap-growth-ev-ebit
   :title "Cheap growth EV/EBIT"
   :description ["Bottom fraction of EV/EBIT (positive EBIT only), top fractions of 7y mean YoY sales growth and ROA. Each fraction and Min mc is a parameter."]
   :params [ev-ebit-fraction
            cheap/sales-growth-fraction
            cheap/roa-fraction
            uni/min-mc]
   :columns cheap/columns
   :universe uni/stats-universe-all
   :calculate calculate
   :stamp uni/stats-stamp})
