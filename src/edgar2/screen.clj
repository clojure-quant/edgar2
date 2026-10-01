(ns edgar2.screen
  "Text screens from data/stats.edn → screen.txt."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [edgar2.stats :as stats]))

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

(defn cheap-growth
  "Bottom 30% PE (positive only), top 30% 7y mean YoY sales growth, top 50% ROC."
  [rows]
  (let [with-pe (filter #(and (number? (:price-earnings %))
                              (pos? (double (:price-earnings %))))
                        rows)
        with-g (filter #(number? (:sales-growth-yoy %)) rows)
        with-roc (filter #(number? (:return-on-capital %)) rows)
        pe-cut (cutoff-max-bottom (numeric :price-earnings with-pe) 0.30)
        g-cut (cutoff-min-top (numeric :sales-growth-yoy with-g) 0.30)
        roc-cut (cutoff-min-top (numeric :return-on-capital with-roc) 0.50)
        hits (->> rows
                  (filter (fn [r]
                            (and (number? (:price-earnings r))
                                 (pos? (double (:price-earnings r)))
                                 (number? (:sales-growth-yoy r))
                                 (number? (:return-on-capital r))
                                 (<= (double (:price-earnings r)) pe-cut)
                                 (>= (double (:sales-growth-yoy r)) g-cut)
                                 (>= (double (:return-on-capital r)) roc-cut))))
                  (sort-by (juxt :price-earnings (comp - :sales-growth-yoy))))]
    {:pe-cut pe-cut
     :growth-cut g-cut
     :roc-cut roc-cut
     :n-pe (count with-pe)
     :n-growth (count with-g)
     :n-roc (count with-roc)
     :rows hits}))

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
    (number? (:price-earnings r)) (assoc :pe (stats/round1 (:price-earnings r)))
    (number? (:sales-growth-yoy r)) (assoc :s-yoy (stats/round1 (:sales-growth-yoy r)))
    (number? (:return-on-capital r)) (assoc :roc (stats/round4 (:return-on-capital r)))
    (number? (:dividend-yield r))
    (assoc :dyld (format "%.1f" (double (:dividend-yield r))))))

(defn print-screen
  [rows]
  (let [cheap (cheap-growth rows)
        growth (high-revenue-growth rows 50)
        cheap-cols [:ticker :entityName :price :mc :pe :s-yoy :roc :dyld]
        growth-cols [:ticker :entityName :price :mc :s-yoy :dyld]]
    (println "Cheap growth")
    (println (format "  bottom 30%% PE (n=%d, PE <= %.1f, positive only)"
                     (:n-pe cheap) (double (:pe-cut cheap))))
    (println (format "  top 30%% s-yoy (7y mean YoY; n=%d, >= %.1f%%)"
                     (:n-growth cheap) (double (:growth-cut cheap))))
    (println (format "  top 50%% roc (n=%d, >= %.2f%%)"
                     (:n-roc cheap) (* 100.0 (double (:roc-cut cheap)))))
    (println (format "  %d names" (count (:rows cheap))))
    (println)
    (pprint/print-table cheap-cols (map table-row (:rows cheap)))
    (println)
    (println "High revenue growth")
    (println "  50 names with the highest 7-year mean YoY sales-growth")
    (println)
    (pprint/print-table growth-cols (map table-row growth))))

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
