(ns edgar2.report.screen
  "Stats rows and the display fields the fundamental screens share.

  `load-stats` and `millions` are also used by the company pages.
  The screens themselves live in edgar2.algo and run through edgar2.screen."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [edgar2.report.stats :as stats]))

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
