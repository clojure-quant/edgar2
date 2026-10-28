(ns edgar2.report.stats-category
  "Category membership and display format for the stats and financials pages.

  Each entry is [key format] or [key format label]. `:percent` values are
  already stored ×100. `:fraction` values are stored as ratios and shown ×100.")

(def category-order
  [:price :quality :growth :valuation :dividend])

(def categories
  {:price [[:price :dec2]
           [:shares :millions "shares (M)"]
           [:adr-ratio :dec2]
           [:marketcap :millions]
           [:enterprise-value :millions]
           [:report-currency :text "reporting-cur"]
           [:annual-count :integer "annual-report-count"]]
   :quality [[:return-on-assets :fraction]
             [:return-on-capital :fraction]
             [:revenue-score :dec1]
             [:operating-income-score :dec1]
             [:profit-margin :fraction]
             [:avg-profit-margin :fraction]
             [:debt2ebitda :dec2]]
   :growth [[:sales-growth-avg-yoy :percent]
            [:earnings-stabilized-yoy :percent]
            [:earnings-variation :percent]
            [:sales-change-q-on-y :percent]
            [:net-income-change-q-on-y :percent]]
   :valuation [[:ev-ebit :dec1]
               [:price-earnings :dec1 "pe"]
               [:earnings-yield :percent]
               [:earnings-stabilized-yield :percent]
               [:earnings-plus-growth-yield :percent]
               [:price-sales :dec2]]
   :dividend [[:dividend-yield :percent]
              [:buyback-yield :percent]
              [:dividend-buyback-yield :percent]
              [:dividend-coverage :dec1]
              [:dividend-buyback-coverage :dec1]]})

(defn item-label
  "Heading for a category entry. A third element is used as-is.
  Percent and fraction headings gain a (%) suffix. Millions gain ($M)."
  [[k fmt label]]
  (or label
      (let [base (name k)]
        (case fmt
          (:percent :fraction) (str base " (%)")
          :millions (str base " ($M)")
          base))))

(defn format-value
  "Display `x` for a category format tag. Nil stays nil."
  [fmt x]
  (when (some? x)
    (if (and (not= fmt :text) (not (number? x)))
      (str x)
      (case fmt
        :percent (format "%.1f" (double x))
        :fraction (format "%.1f" (* 100.0 (double x)))
        :dec1 (format "%.1f" (double x))
        :dec2 (format "%.2f" (double x))
        :millions (str (long (Math/round (/ (double x) 1.0e6))))
        :integer (str (long (Math/round (double x))))
        :text (str x)
        (str x)))))

(defn groups
  "[{:title :metrics}] in `category-order`. Text items are omitted so a
  percentile table can rank every remaining metric."
  [title-fn]
  (mapv (fn [cat]
          {:title (title-fn cat)
           :metrics (vec
                     (keep (fn [entry]
                             (when-not (= :text (second entry))
                               {:key (first entry)
                                :label (item-label entry)
                                :fmt (second entry)}))
                           (get categories cat)))})
        category-order))
