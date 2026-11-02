(ns edgar2.algo.cheap-consistent
  "Bottom fraction of PE, top fractions of revenue-score and
  operating-income-score. Each fraction and Min mc is a screen
  parameter."
  (:require [edgar2.algo.cutoff :as cutoff]
            [edgar2.report.screen :as report]
            [edgar2.screen.universe :as uni]
            [edgar2.screen.util :as util]))

(def columns
  [{:key :ticker :label "Ticker"}
   {:key :entityName :label "Name"}
   {:key :price :label "Price" :num? true :decimals 2}
   {:key :mc :label "MC" :num? true}
   {:key :pe :label "PE" :num? true :decimals 1}
   {:key :rev-score :label "Rev score" :num? true :decimals 1}
   {:key :oi-score :label "OI score" :num? true :decimals 1}
   {:key :dyld :label "Yield" :num? true}])

(def pe-fraction
  (util/choice-param :pe-fraction "PE fraction"
                     cutoff/fraction-choices 0.30))

(def revenue-score-fraction
  (util/choice-param :revenue-score-fraction "Revenue-score fraction"
                     cutoff/fraction-choices 0.30))

(def operating-income-score-fraction
  (util/choice-param :operating-income-score-fraction
                     "Operating-income-score fraction"
                     cutoff/fraction-choices 0.30))

(def margin-fraction
  (util/choice-param :margin-fraction "Margin fraction"
                     cutoff/fraction-choices 0.30))

(defn- bottom-cut
  "Largest value in the bottom `p` fraction of `k` over `rows`, or nil."
  [rows k p]
  (let [values (cutoff/numeric k rows)]
    (when (seq values)
      (cutoff/cutoff-max-bottom values p))))

(defn- top-cut
  "Smallest value in the top `p` fraction of `k` over `rows`, or nil."
  [rows k p]
  (let [values (cutoff/numeric k rows)]
    (when (seq values)
      (cutoff/cutoff-min-top values p))))

(defn cheap-consistent-on
  "Bottom `pe-p` of positive PE, top `rev-p` revenue-score, top `oi-p`
  operating-income-score. When `margin-p` is set, also the top
  `margin-p` of avg-profit-margin. Cuts are taken after the Min mc cut."
  [rows {:keys [min-mc pe-p rev-p oi-p margin-p]}]
  (let [min-mc (or min-mc report/default-min-mc)
        pe-p (or pe-p 0.30)
        rev-p (or rev-p 0.30)
        oi-p (or oi-p 0.30)
        rows (vec rows)
        after-mc (vec (report/at-least-mc rows min-mc))
        with-pe (filterv #(and (number? (:price-earnings %))
                               (pos? (double (:price-earnings %))))
                         after-mc)
        pe-cut (bottom-cut with-pe :price-earnings pe-p)
        rev-cut (top-cut after-mc :revenue-score rev-p)
        oi-cut (top-cut after-mc :operating-income-score oi-p)
        margin-cut (when margin-p
                     (top-cut after-mc :avg-profit-margin margin-p))]
    (if-not (and pe-cut rev-cut oi-cut (or (nil? margin-p) margin-cut))
      {:rows []
       :lines ["no positive PE"]}
      (let [steps
            (cond-> [(uni/min-mc-step min-mc)
                     {:label "positive price-earnings"
                      :pred (util/positive-at :price-earnings)}
                     {:label (format "price-earnings <= %.1f (bottom %d%%)"
                                     (double pe-cut)
                                     (cutoff/pct-label pe-p))
                      :pred (util/at-most :price-earnings pe-cut)}
                     {:label "numeric revenue-score"
                      :pred (util/number-at :revenue-score)}
                     {:label (format "revenue-score >= %.1f (top %d%%)"
                                     (double rev-cut)
                                     (cutoff/pct-label rev-p))
                      :pred (util/at-least :revenue-score rev-cut)}
                     {:label "numeric operating-income-score"
                      :pred (util/number-at :operating-income-score)}
                     {:label (format "operating-income-score >= %.1f (top %d%%)"
                                     (double oi-cut)
                                     (cutoff/pct-label oi-p))
                      :pred (util/at-least :operating-income-score oi-cut)}]
              margin-p
              (conj {:label "numeric avg-profit-margin"
                     :pred (util/number-at :avg-profit-margin)}
                    {:label (format "avg-profit-margin >= %.2f%% (top %d%%)"
                                    (* 100.0 (double margin-cut))
                                    (cutoff/pct-label margin-p))
                     :pred (util/at-least :avg-profit-margin margin-cut)}))
            result (util/apply-and-pipeline rows steps)
            hits (sort-by (if margin-p
                            (juxt :price-earnings
                                  (comp - :revenue-score)
                                  (comp - :operating-income-score)
                                  (comp - :avg-profit-margin))
                            (juxt :price-earnings
                                  (comp - :revenue-score)
                                  (comp - :operating-income-score)))
                          (:tickers result))]
        {:rows (vec hits)
         :lines (util/report-pipeline-info result)}))))

(defn calculate
  [items params]
  (let [result (cheap-consistent-on
                (uni/calculate-stats items)
                {:min-mc (util/param-value params uni/min-mc)
                 :pe-p (util/param-value params pe-fraction)
                 :rev-p (util/param-value params revenue-score-fraction)
                 :oi-p (util/param-value params operating-income-score-fraction)})]
    {:rows (mapv report/table-row (:rows result))
     :lines (:lines result)}))

(def screen
  {:id :cheap-consistent
   :title "Cheap consistent"
   :description ["Bottom fraction of PE (positive only), top fractions of revenue-score and operating-income-score. Each fraction and Min mc is a parameter."]
   :params [pe-fraction
            revenue-score-fraction
            operating-income-score-fraction
            uni/min-mc]
   :columns columns
   :universe uni/stats-universe-all
   :calculate calculate
   :stamp uni/stats-stamp})
