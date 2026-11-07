(ns edgar2.algo.cheap-growth
  "Bottom fraction of PE, top fractions of sales growth and ROA.
  Each fraction and Min mc is a screen parameter."
  (:require [edgar2.algo.cutoff :as cutoff]
            [edgar2.report.screen :as report]
            [edgar2.screen.universe :as uni]
            [edgar2.screen.util :as util]))

(def columns
  [{:key :ticker :label "Ticker"}
   {:key :entityName :label "Name"}
   {:key :price :label "Price" :num? true :decimals 2}
   {:key :mc :label "MC" :num? true}
   {:key :ev :label "EV" :num? true}
   {:key :pe :label "PE" :num? true :decimals 1}
   {:key :ev-ebit :label "EV/EBIT" :num? true :decimals 1}
   {:key :s-yoy :label "Sales YoY" :num? true :decimals 1}
   {:key :roa :label "ROA" :num? true}
   {:key :roce :label "ROCE" :num? true}
   {:key :dyld :label "Yield" :num? true}])

(def pe-fraction
  (util/choice-param :pe-fraction "PE fraction"
                     cutoff/fraction-choices 0.30))

(def sales-growth-fraction
  (util/choice-param :sales-growth-fraction "Sales-growth fraction"
                     cutoff/fraction-choices 0.30))

(def roa-fraction
  (util/choice-param :roa-fraction "ROA fraction"
                     cutoff/fraction-choices 0.50))

(defn- metric-label
  [k]
  (case k
    :price-earnings "PE"
    :ev-ebit "EV/EBIT"
    (name k)))

(defn cheap-growth-on
  "Bottom `cheap-fraction` of positive `k`, top `growth-fraction` 7y
  mean YoY sales growth, top `roa-fraction` ROA. `keep?` further
  restricts the cheap set. Cuts are taken after the Min mc cut."
  [rows k {:keys [keep? cheap-fraction growth-fraction roa-fraction min-mc]}]
  (let [keep-given? (some? keep?)
        keep? (or keep? (constantly true))
        min-mc (or min-mc report/default-min-mc)
        cheap-fraction (or cheap-fraction 0.30)
        growth-fraction (or growth-fraction 0.30)
        roa-fraction (or roa-fraction 0.50)
        rows (vec rows)
        after-mc (vec (report/at-least-mc rows min-mc))
        with-k (filterv #(and (number? (k %))
                              (pos? (double (k %)))
                              (keep? %))
                        after-mc)
        k-cut (when (seq with-k)
                (cutoff/cutoff-max-bottom
                 (cutoff/numeric k with-k) cheap-fraction))
        with-g (filterv #(number? (:sales-growth-avg-yoy %)) after-mc)
        g-cut (when (seq with-g)
                (cutoff/cutoff-min-top
                 (cutoff/numeric :sales-growth-avg-yoy with-g) growth-fraction))
        with-roa (filterv #(number? ( :roa %)) after-mc)
        roa-cut (when (seq with-roa)
                  (cutoff/cutoff-min-top
                   (cutoff/numeric  :roa with-roa) roa-fraction))]
    (if-not (and k-cut g-cut roa-cut)
      {:rows []
       :lines [(format "no positive %s" (metric-label k))]}
      (let [steps
            (into (cond-> [(uni/min-mc-step min-mc)
                           {:label (format "positive %s" (name k))
                            :pred (util/positive-at k)}]
                    keep-given?
                    (conj {:label "positive EBIT"
                           :pred keep?}))
                  [{:label (format "%s <= %.1f (bottom %d%%)"
                                   (name k)
                                   (double k-cut)
                                   (cutoff/pct-label cheap-fraction))
                    :pred (util/at-most k k-cut)}
                   {:label "numeric sales-growth-avg-yoy"
                    :pred (util/number-at :sales-growth-avg-yoy)}
                   {:label (format "sales-growth-avg-yoy >= %.1f (top %d%%)"
                                   (double g-cut)
                                   (cutoff/pct-label growth-fraction))
                    :pred (util/at-least :sales-growth-avg-yoy g-cut)}
                   {:label (format "return-on-assets >= %.2f%% (top %d%%)"
                                   (* 100.0 (double roa-cut))
                                   (cutoff/pct-label roa-fraction))
                    :pred (util/at-least :roa roa-cut)}])
            result (util/apply-and-pipeline rows steps)
            hits (sort-by (juxt k (comp - :sales-growth-avg-yoy))
                          (:tickers result))]
        {:rows (vec hits)
         :lines (util/report-pipeline-info result)}))))

(defn calculate
  [items params]
  (let [result (cheap-growth-on
                (uni/calculate-stats items)
                :price-earnings
                {:min-mc (util/param-value params uni/min-mc)
                 :cheap-fraction (util/param-value params pe-fraction)
                 :growth-fraction (util/param-value params sales-growth-fraction)
                 :roa-fraction (util/param-value params roa-fraction)})]
    {:rows (mapv report/table-row (:rows result))
     :lines (:lines result)}))

(def screen
  {:id :cheap-growth
   :title "Cheap growth"
   :description ["Bottom fraction of PE (positive only), top fractions of 7y mean YoY sales growth and ROA. Each fraction and Min mc is a parameter."]
   :params [pe-fraction
            sales-growth-fraction
            roa-fraction
            uni/min-mc]
   :columns columns
   :universe uni/stats-universe-all
   :calculate calculate
   :stamp uni/stats-stamp})
