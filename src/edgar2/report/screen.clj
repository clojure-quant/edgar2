(ns edgar2.report.screen
  "Text screens from data/stats.edn → data/screen.txt."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [edgar2.report.stats :as stats]))

(def screen-path "data/screen.txt")

(def mc-choices
  "Minimum market cap, in millions of dollars."
  [0 5 10 50 100 250 500 1000 5000 10000])

(def default-min-mc 100)

(defn parse-min-mc
  "One of `mc-choices`, or nil. Accepts the number or its string."
  [x]
  (let [n (cond
            (integer? x) (long x)
            (number? x) (long x)
            :else (try (Long/parseLong (str/trim (str x)))
                       (catch Exception _ nil)))]
    (when (some #{n} mc-choices) n)))

(defn load-stats
  []
  (when-not (.exists (io/file stats/stats-path))
    (throw (ex-info "Missing stats file; run clj -X:stats first"
                    {:path stats/stats-path})))
  (edn/read-string (slurp stats/stats-path)))

(defn cutoff-max-bottom
  "Largest value still in the bottom `p` fraction of xs."
  [xs p]
  (let [v (vec (sort xs))
        n (count v)
        k (max 1 (int (Math/ceil (* p n))))]
    (nth v (dec k))))

(defn cutoff-min-top
  "Smallest value still in the top `p` fraction of xs."
  [xs p]
  (let [v (vec (sort xs))
        n (count v)
        k (max 1 (int (Math/ceil (* p n))))]
    (nth v (- n k))))

(defn numeric
  [k rows]
  (keep (fn [r] (when (number? (k r)) (k r))) rows))

(defn cheap-growth-on
  "Bottom 30% of positive `k`, top 30% 7y mean YoY sales growth, top 50% ROA.
  `keep?` further restricts the cheap set (growth and ROC cuts stay on all rows)."
  ([rows k] (cheap-growth-on rows k (constantly true)))
  ([rows k keep?]
   (let [with-k (filter #(and (number? (k %)) (pos? (double (k %))) (keep? %)) rows)
         with-g (filter #(number? (:sales-growth-yoy %)) rows)
         with-roa (filter #(number? (:return-on-assets %)) rows)
         k-cut (when (seq with-k) (cutoff-max-bottom (numeric k with-k) 0.30))
         g-cut (when (seq with-g) (cutoff-min-top (numeric :sales-growth-yoy with-g) 0.30))
         roa-cut (when (seq with-roa) (cutoff-min-top (numeric :return-on-assets with-roa) 0.50))
         hits (if (and k-cut g-cut roa-cut)
                (->> rows
                     (filter (fn [r]
                               (and (number? (k r))
                                    (pos? (double (k r)))
                                    (keep? r)
                                    (number? (:sales-growth-yoy r))
                                    (number? (:return-on-assets r))
                                    (<= (double (k r)) k-cut)
                                    (>= (double (:sales-growth-yoy r)) g-cut)
                                    (>= (double (:return-on-assets r)) roa-cut))))
                     (sort-by (juxt k (comp - :sales-growth-yoy))))
                [])]
     {:cut k-cut
      :growth-cut g-cut
      :roa-cut roa-cut
      :n-cheap (count with-k)
      :n-growth (count with-g)
      :n-roa (count with-roa)
      :rows hits})))

(defn cheap-growth
  "Bottom 30% PE (positive only), top 30% 7y mean YoY sales growth, top 50% ROA."
  [rows]
  (cheap-growth-on rows :price-earnings))

(defn cheap-growth-ev-ebit
  "Bottom 30% EV/EBIT (positive EBIT only), top 30% 7y mean YoY sales growth, top 50% ROA."
  [rows]
  (cheap-growth-on rows :ev-ebit #(and (number? (:ebit %)) (pos? (double (:ebit %))))))

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
         pe-cut (when (seq with-pe) (cutoff-max-bottom (numeric :price-earnings with-pe) pe-p))
         rev-cut (when (seq with-rev) (cutoff-min-top (numeric :revenue-score with-rev) 0.30))
         oi-cut (when (seq with-oi) (cutoff-min-top (numeric :operating-income-score with-oi) 0.30))
         margin-cut (when (and margin-p (seq with-margin))
                      (cutoff-min-top (numeric :avg-profit-margin with-margin) margin-p))
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

(defn cheap-consistent
  "Bottom 30% PE (positive only), top 30% revenue-score, top 30% operating-income-score."
  [rows]
  (cheap-consistent-on rows 0.30))

(defn cheap-consistent-margin
  "Bottom 50% PE (positive only), top 30% revenue-score, top 30% operating-income-score,
  top 30% avg-profit-margin."
  [rows]
  (cheap-consistent-on rows 0.50 0.30))

(defn high-revenue-growth
  [rows n]
  (->> rows
       (filter #(number? (:sales-growth-yoy %)))
       (sort-by :sales-growth-yoy >)
       (take n)
       vec))

(defn dividend-yield-screen
  "Top 10% of positive dividend yields, highest yield first."
  [rows]
  (let [with-dy (filterv #(and (number? (:dividend-yield %))
                               (pos? (double (:dividend-yield %))))
                         rows)
        cut (when (seq with-dy)
              (cutoff-min-top (numeric :dividend-yield with-dy) 0.10))
        hits (if cut
               (->> with-dy
                    (filter #(>= (double (:dividend-yield %)) (double cut)))
                    (sort-by :dividend-yield >)
                    vec)
               [])]
    {:cut cut
     :n (count with-dy)
     :rows hits}))

(defn cap-name
  [s]
  (let [s (str s)]
    (if (> (count s) 30) (subs s 0 30) s)))

(defn millions
  [x]
  (when (number? x)
    (Math/round (/ (double x) 1.0e6))))

(defn at-least-mc
  "Rows whose market cap, in millions of dollars, is at least `min-mc`.
  A company with no market cap is left out at every level, including 0."
  [rows min-mc]
  (let [min-mc (long min-mc)]
    (filterv (fn [r]
               (when-let [mc (millions (:marketcap r))]
                 (>= mc min-mc)))
             rows)))

(defn table-row
  [r]
  (cond-> {:ticker (:ticker r)
           :entityName (cap-name (:entityName r))}
    (number? (:price r))
    (assoc :price (->> (:price r) double (format "%.2f") Double/parseDouble))
    (number? (:marketcap r))
    (assoc :mc (millions (:marketcap r))
           :market-cap (millions (:marketcap r)))
    (number? (:enterprise-value r)) (assoc :ev (millions (:enterprise-value r)))
    (number? (:price-earnings r)) (assoc :pe (stats/round1 (:price-earnings r)))
    (number? (:ev-ebit r)) (assoc :ev-ebit (stats/round1 (:ev-ebit r)))
    (number? (:sales-growth-yoy r)) (assoc :s-yoy (stats/round1 (:sales-growth-yoy r)))
    (number? (:revenue-score r)) (assoc :rev-score (stats/round1 (:revenue-score r)))
    (number? (:operating-income-score r)) (assoc :oi-score (stats/round1 (:operating-income-score r)))
    (number? (:avg-profit-margin r)) (assoc :avg-margin (stats/round4 (:avg-profit-margin r)))
    (number? (:return-on-assets r)) (assoc :roa (stats/round4 (:return-on-assets r)))
    (number? (:return-on-capital r)) (assoc :roc (stats/round4 (:return-on-capital r)))
    (number? (:dividend-yield r))
    (assoc :dyld (format "%.1f" (double (:dividend-yield r)))
           :dividend-yield (stats/round1 (:dividend-yield r)))
    (number? (:dividend-coverage r))
    (assoc :dividend-coverage (stats/round1 (:dividend-coverage r)))))

(defn- cheap-lines
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

(defn- pct-label
  [p]
  (long (Math/round (* 100.0 (double p)))))

(defn- dividend-yield-lines
  [screen]
  (if-not (:cut screen)
    ["  no positive dividend-yield"]
    [(format "  top 10%% dividend-yield (n=%d, >= %.1f%%, positive only)"
             (:n screen) (double (:cut screen)))
     "  sorted by dividend-yield descending"
     "  dividend-coverage = net income / dividends paid"
     "  market-cap in $ millions"
     (format "  %d names" (count (:rows screen)))]))

(defn- consistent-lines
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

(defn- with-mc-line
  [specs min-mc n-universe n-all]
  (let [line (format "  mc >= %d ($ millions; n=%d of %d)"
                     (long min-mc) n-universe n-all)]
    (mapv (fn [spec] (update spec :lines (fn [lines] (into [line] lines)))) specs)))

(declare screen-specs*)

(defn screen-specs
  "Screens printed by `print-screen`: title, cutoff lines, columns, and rows.

  `min-mc` is applied first. Percentile cutoffs and the growth ranking
  use only companies at or above that market cap, in millions of dollars.
  Defaults to 100."
  ([rows] (screen-specs rows {}))
  ([rows {:keys [min-mc]}]
   (let [min-mc (or (parse-min-mc min-mc) default-min-mc)
         universe (at-least-mc rows min-mc)]
     (with-mc-line (screen-specs* universe) min-mc (count universe) (count rows)))))

(defn- screen-specs*
  [rows]
  (let [cheap (cheap-growth rows)
        cheap-ev (cheap-growth-ev-ebit rows)
        consistent (cheap-consistent rows)
        consistent-margin (cheap-consistent-margin rows)
        growth (high-revenue-growth rows 50)
        yield-screen (dividend-yield-screen rows)
        cheap-cols [:ticker :entityName :price :mc :ev :pe :ev-ebit :s-yoy :roa :roc :dyld]
        consistent-cols [:ticker :entityName :price :mc :pe :rev-score :oi-score :dyld]
        margin-cols [:ticker :entityName :price :mc :pe :rev-score :oi-score :avg-margin :dyld]
        growth-cols [:ticker :entityName :price :mc :s-yoy :dyld]
        yield-cols [:ticker :market-cap :pe :ev-ebit :dividend-yield :dividend-coverage]
        cheap-spec (fn [id title metric positive screen notes]
                     {:id id
                      :title title
                      :lines (cheap-lines metric positive screen notes)
                      :columns cheap-cols
                      :rows (mapv table-row (:rows screen))
                      :table? (boolean (:cut screen))})]
    [(cheap-spec :cheap-growth "Cheap growth" "PE" "positive only" cheap nil)
     (cheap-spec :cheap-growth-ev-ebit
                 "Cheap growth EV/EBIT" "EV/EBIT" "positive EBIT only" cheap-ev
                 ["  EV (USD millions) = market cap + interest-bearing debt + preferred + NCI - cash"
                  "  EBIT = operating income, else net income + interest + tax"])
     {:id :cheap-consistent
      :title "Cheap consistent"
      :lines (consistent-lines consistent)
      :columns consistent-cols
      :rows (mapv table-row (:rows consistent))
      :table? (boolean (:cut consistent))}
     {:id :cheap-consistent-margin
      :title "Cheap consistent margin"
      :lines (consistent-lines consistent-margin)
      :columns margin-cols
      :rows (mapv table-row (:rows consistent-margin))
      :table? (boolean (:cut consistent-margin))}
     {:id :high-revenue-growth
      :title "High revenue growth"
      :lines ["  50 names with the highest 7-year mean YoY sales-growth"]
      :columns growth-cols
      :rows (mapv table-row growth)
      :table? true}
     {:id :dividend-yield
      :title "Dividend yield"
      :lines (dividend-yield-lines yield-screen)
      :columns yield-cols
      :rows (mapv table-row (:rows yield-screen))
      :table? (boolean (:cut yield-screen))}]))

(defn- print-spec
  [{:keys [title lines columns rows table?]}]
  (println title)
  (doseq [line lines]
    (println line))
  (when table?
    (println)
    (pprint/print-table columns rows)))

(defn print-screen
  ([rows] (print-screen rows {}))
  ([rows opts]
   (let [[first-spec & more] (screen-specs rows opts)]
     (print-spec first-spec)
     (doseq [spec more]
       (println)
       (print-spec spec)))))

(defn screen
  "Write data/screen.txt from data/stats.edn.
  Market cap is filtered first; default minimum is 100 ($ millions).

  Usage: clj -X:screen
         clj -X:screen :min-mc 1000"
  ([] (screen {}))
  ([{:keys [min-mc]}]
   (let [min-mc (or (parse-min-mc min-mc) default-min-mc)
         rows (load-stats)
         text (with-out-str (print-screen rows {:min-mc min-mc}))]
     (spit screen-path text)
     (print text)
     (println (format "Wrote %s" screen-path))
     nil)))
