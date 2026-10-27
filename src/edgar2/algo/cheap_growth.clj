(ns edgar2.algo.cheap-growth
  "Bottom 30% PE, top 30% sales growth, top 50% ROA."
  (:require [edgar2.algo.cutoff :as cutoff]
            [edgar2.report.screen :as report]
            [edgar2.screen.universe :as uni]))

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
   {:key :roc :label "ROC" :num? true}
   {:key :dyld :label "Yield" :num? true}])

(defn cheap-growth-on
  "Bottom 30% of positive `k`, top 30% 7y mean YoY sales growth, top 50% ROA.
  `keep?` further restricts the cheap set (growth and ROA cuts stay on all rows)."
  ([rows k] (cheap-growth-on rows k (constantly true)))
  ([rows k keep?]
   (let [with-k (filter #(and (number? (k %)) (pos? (double (k %))) (keep? %)) rows)
         with-g (filter #(number? (:sales-growth-avg-yoy %)) rows)
         with-roa (filter #(number? (:return-on-assets %)) rows)
         k-cut (when (seq with-k) (cutoff/cutoff-max-bottom (cutoff/numeric k with-k) 0.30))
         g-cut (when (seq with-g) (cutoff/cutoff-min-top (cutoff/numeric :sales-growth-avg-yoy with-g) 0.30))
         roa-cut (when (seq with-roa) (cutoff/cutoff-min-top (cutoff/numeric :return-on-assets with-roa) 0.50))
         hits (if (and k-cut g-cut roa-cut)
                (->> rows
                     (filter (fn [r]
                               (and (number? (k r))
                                    (pos? (double (k r)))
                                    (keep? r)
                                    (number? (:sales-growth-avg-yoy r))
                                    (number? (:return-on-assets r))
                                    (<= (double (k r)) k-cut)
                                    (>= (double (:sales-growth-avg-yoy r)) g-cut)
                                    (>= (double (:return-on-assets r)) roa-cut))))
                     (sort-by (juxt k (comp - :sales-growth-avg-yoy))))
                [])]
     {:cut k-cut
      :growth-cut g-cut
      :roa-cut roa-cut
      :n-cheap (count with-k)
      :n-growth (count with-g)
      :n-roa (count with-roa)
      :rows hits})))

(defn cheap-lines
  [metric-label positive-label screen notes]
  (into (vec (or notes []))
        (if-not (:cut screen)
          [(format "  no positive %s" metric-label)]
          [(format "  bottom 30%% %s (n=%d, %s <= %.1f, %s)"
                   metric-label (:n-cheap screen) metric-label
                   (double (:cut screen)) positive-label)
           (format "  top 30%% s-yoy (7y mean YoY; n=%d, >= %.1f%%)"
                   (:n-growth screen) (double (:growth-cut screen)))
           (format "  top 50%% roa (n=%d, >= %.2f%%)"
                   (:n-roa screen) (* 100.0 (double (:roa-cut screen))))
           (format "  %d names" (count (:rows screen)))])))

(defn calculate
  [items _params]
  (let [result (cheap-growth-on (mapv :row items) :price-earnings)]
    {:rows (mapv report/table-row (:rows result))
     :lines (cheap-lines "PE" "positive only" result nil)}))

(def screen
  {:id :cheap-growth
   :title "Cheap growth"
   :description ["Bottom 30% PE (positive only), top 30% 7y mean YoY sales growth, top 50% ROA."]
   :params [uni/min-mc]
   :columns columns
   :universe uni/stats
   :calculate calculate
   :stamp uni/stats-stamp})
