(ns edgar2.algo.cheap-consistent
  "Bottom 30% PE, top 30% revenue-score, top 30% operating-income-score."
  (:require [edgar2.algo.cutoff :as cutoff]
            [edgar2.report.screen :as report]
            [edgar2.screen.universe :as uni]))

(def columns
  [{:key :ticker :label "Ticker"}
   {:key :entityName :label "Name"}
   {:key :price :label "Price" :num? true :decimals 2}
   {:key :mc :label "MC" :num? true}
   {:key :pe :label "PE" :num? true :decimals 1}
   {:key :rev-score :label "Rev score" :num? true :decimals 1}
   {:key :oi-score :label "OI score" :num? true :decimals 1}
   {:key :dyld :label "Yield" :num? true}])

(defn- pct-label [p]
  (long (Math/round (* 100.0 (double p)))))

(defn cheap-consistent-on
  "Bottom `pe-p` of positive PE, top 30% revenue-score, top 30% operating-income-score.
  When `margin-p` is set, also the top `margin-p` of avg-profit-margin.
  Score and margin cutoffs use every row; the PE cutoff uses positive PE only."
  ([rows pe-p] (cheap-consistent-on rows pe-p nil))
  ([rows pe-p margin-p]
   (let [with-pe (filter #(and (number? (:price-earnings %))
                               (pos? (double (:price-earnings %))))
                         rows)
         with-rev (filter #(number? (:revenue-score %)) rows)
         with-oi (filter #(number? (:operating-income-score %)) rows)
         with-margin (filter #(number? (:avg-profit-margin %)) rows)
         pe-cut (when (seq with-pe) (cutoff/cutoff-max-bottom (cutoff/numeric :price-earnings with-pe) pe-p))
         rev-cut (when (seq with-rev) (cutoff/cutoff-min-top (cutoff/numeric :revenue-score with-rev) 0.30))
         oi-cut (when (seq with-oi) (cutoff/cutoff-min-top (cutoff/numeric :operating-income-score with-oi) 0.30))
         margin-cut (when (and margin-p (seq with-margin))
                      (cutoff/cutoff-min-top (cutoff/numeric :avg-profit-margin with-margin) margin-p))
         hits (if (and pe-cut rev-cut oi-cut (or (nil? margin-p) margin-cut))
                (->> rows
                     (filter (fn [r]
                               (and (number? (:price-earnings r))
                                    (pos? (double (:price-earnings r)))
                                    (number? (:revenue-score r))
                                    (number? (:operating-income-score r))
                                    (<= (double (:price-earnings r)) pe-cut)
                                    (>= (double (:revenue-score r)) rev-cut)
                                    (>= (double (:operating-income-score r)) oi-cut)
                                    (or (nil? margin-p)
                                        (and (number? (:avg-profit-margin r))
                                             (>= (double (:avg-profit-margin r)) margin-cut))))))
                     (sort-by (if margin-p
                                (juxt :price-earnings
                                      (comp - :revenue-score)
                                      (comp - :operating-income-score)
                                      (comp - :avg-profit-margin))
                                (juxt :price-earnings
                                      (comp - :revenue-score)
                                      (comp - :operating-income-score)))))
                [])]
     {:cut pe-cut
      :pe-p pe-p
      :rev-cut rev-cut
      :oi-cut oi-cut
      :margin-cut margin-cut
      :margin-p margin-p
      :n-cheap (count with-pe)
      :n-rev (count with-rev)
      :n-oi (count with-oi)
      :n-margin (count with-margin)
      :rows hits})))

(defn consistent-lines
  [screen]
  (if-not (:cut screen)
    ["  no positive PE"]
    (cond-> [(format "  bottom %d%% PE (n=%d, PE <= %.1f, positive only)"
                     (pct-label (:pe-p screen)) (:n-cheap screen) (double (:cut screen)))
             (format "  top 30%% revenue-score (n=%d, >= %.1f)"
                     (:n-rev screen) (double (:rev-cut screen)))
             (format "  top 30%% operating-income-score (n=%d, >= %.1f)"
                     (:n-oi screen) (double (:oi-cut screen)))]
      (:margin-p screen)
      (conj (format "  top %d%% avg-profit-margin (n=%d, >= %.2f%%)"
                    (pct-label (:margin-p screen))
                    (:n-margin screen)
                    (* 100.0 (double (:margin-cut screen)))))
      true
      (conj (format "  %d names" (count (:rows screen)))))))

(defn calculate
  [items _params]
  (let [result (cheap-consistent-on (mapv :row items) 0.30)]
    {:rows (mapv report/table-row (:rows result))
     :lines (consistent-lines result)}))

(def screen
  {:id :cheap-consistent
   :title "Cheap consistent"
   :description ["Bottom 30% PE (positive only), top 30% revenue-score, top 30% operating-income-score."]
   :params [uni/min-mc]
   :columns columns
   :universe uni/stats
   :calculate calculate
   :stamp uni/stats-stamp})
