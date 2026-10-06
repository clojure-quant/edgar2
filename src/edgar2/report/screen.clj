(ns edgar2.report.screen
  "Text screens from data/stats.edn → screen.txt."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [edgar2.report.stats :as stats]))

(def screen-path "screen.txt")

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
  "Bottom 30% of positive `k`, top 30% 7y mean YoY sales growth, top 50% ROC.
  `keep?` further restricts the cheap set (growth and ROC cuts stay on all rows)."
  ([rows k] (cheap-growth-on rows k (constantly true)))
  ([rows k keep?]
   (let [with-k (filter #(and (number? (k %)) (pos? (double (k %))) (keep? %)) rows)
         with-g (filter #(number? (:sales-growth-yoy %)) rows)
         with-roc (filter #(number? (:return-on-capital %)) rows)
         k-cut (when (seq with-k) (cutoff-max-bottom (numeric k with-k) 0.30))
         g-cut (when (seq with-g) (cutoff-min-top (numeric :sales-growth-yoy with-g) 0.30))
         roc-cut (when (seq with-roc) (cutoff-min-top (numeric :return-on-capital with-roc) 0.50))
         hits (if (and k-cut g-cut roc-cut)
                (->> rows
                     (filter (fn [r]
                               (and (number? (k r))
                                    (pos? (double (k r)))
                                    (keep? r)
                                    (number? (:sales-growth-yoy r))
                                    (number? (:return-on-capital r))
                                    (<= (double (k r)) k-cut)
                                    (>= (double (:sales-growth-yoy r)) g-cut)
                                    (>= (double (:return-on-capital r)) roc-cut))))
                     (sort-by (juxt k (comp - :sales-growth-yoy))))
                [])]
     {:cut k-cut
      :growth-cut g-cut
      :roc-cut roc-cut
      :n-cheap (count with-k)
      :n-growth (count with-g)
      :n-roc (count with-roc)
      :rows hits})))

(defn cheap-growth
  "Bottom 30% PE (positive only), top 30% 7y mean YoY sales growth, top 50% ROC."
  [rows]
  (cheap-growth-on rows :price-earnings))

(defn cheap-growth-ev-ebit
  "Bottom 30% EV/EBIT (positive EBIT only), top 30% 7y mean YoY sales growth, top 50% ROC."
  [rows]
  (cheap-growth-on rows :ev-ebit #(and (number? (:ebit %)) (pos? (double (:ebit %))))))

(defn high-revenue-growth
  [rows n]
  (->> rows
       (filter #(number? (:sales-growth-yoy %)))
       (sort-by :sales-growth-yoy >)
       (take n)
       vec))

(defn cap-name
  [s]
  (let [s (str s)]
    (if (> (count s) 30) (subs s 0 30) s)))

(defn millions
  [x]
  (when (number? x)
    (Math/round (/ (double x) 1.0e6))))

(defn table-row
  [r]
  (cond-> {:ticker (:ticker r)
           :entityName (cap-name (:entityName r))}
    (number? (:price r))
    (assoc :price (->> (:price r) double (format "%.2f") Double/parseDouble))
    (number? (:marketcap r)) (assoc :mc (millions (:marketcap r)))
    (number? (:enterprise-value r)) (assoc :ev (millions (:enterprise-value r)))
    (number? (:price-earnings r)) (assoc :pe (stats/round1 (:price-earnings r)))
    (number? (:ev-ebit r)) (assoc :ev-ebit (stats/round1 (:ev-ebit r)))
    (number? (:sales-growth-yoy r)) (assoc :s-yoy (stats/round1 (:sales-growth-yoy r)))
    (number? (:return-on-capital r)) (assoc :roc (stats/round4 (:return-on-capital r)))
    (number? (:dividend-yield r))
    (assoc :dyld (format "%.1f" (double (:dividend-yield r))))))

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
           (format "  top 50%% roc (n=%d, >= %.2f%%)"
                   (:n-roc screen) (* 100.0 (double (:roc-cut screen))))
           (format "  %d names" (count (:rows screen)))])))

(defn screen-specs
  "Screens printed by `print-screen`: title, cutoff lines, columns, and rows."
  [rows]
  (let [cheap (cheap-growth rows)
        cheap-ev (cheap-growth-ev-ebit rows)
        growth (high-revenue-growth rows 50)
        cheap-cols [:ticker :entityName :price :mc :ev :pe :ev-ebit :s-yoy :roc :dyld]
        growth-cols [:ticker :entityName :price :mc :s-yoy :dyld]
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
     {:id :high-revenue-growth
      :title "High revenue growth"
      :lines ["  50 names with the highest 7-year mean YoY sales-growth"]
      :columns growth-cols
      :rows (mapv table-row growth)
      :table? true}]))

(defn- print-spec
  [{:keys [title lines columns rows table?]}]
  (println title)
  (doseq [line lines]
    (println line))
  (when table?
    (println)
    (pprint/print-table columns rows)))

(defn print-screen
  [rows]
  (let [[first-spec & more] (screen-specs rows)]
    (print-spec first-spec)
    (doseq [spec more]
      (println)
      (print-spec spec))))

(defn screen
  "Write screen.txt from data/stats.edn.

  Usage: clj -X:screen"
  ([] (screen {}))
  ([_]
   (let [rows (load-stats)
         text (with-out-str (print-screen rows))]
     (spit screen-path text)
     (print text)
     (println (format "Wrote %s" screen-path))
     nil)))
