(ns edgar2.financials.calc.row.dividend-coverage
  "Per-period dividend paid, buyback, and coverage from one report row."
  (:require [edgar2.financials.concepts :as concepts]))

(defn- as-number
  [v]
  (when (number? v)
    (if (and (float? v)
             (== (double v) (Math/rint (double v)))
             (<= (Math/abs (double v)) (double Long/MAX_VALUE)))
      (long v)
      v)))

(defn- positive-amount
  "Absolute value when positive; else nil."
  [v]
  (when (number? v)
    (let [amount (Math/abs (double v))]
      (when (pos? amount) amount))))

(defn- first-tag-val
  [row tag-pairs]
  (some (fn [[_ c]]
          (as-number (get row (name c))))
        tag-pairs))

(defn- net-income
  "Net income from `concept-row`, else tags on `ds-row`."
  [ds-row concept-row]
  (or (as-number (:net-income concept-row))
      (first-tag-val ds-row (:data (concepts/get-concept-id :net-income)))))

(defn calculate-dividend-coverage
  "Dividend cash metrics for one annual period.

  `ds-row` is a report-table row; `concept-row` is the aligned `calc-concepts`
  row (`:dividends-paid` / `:stock-repurchase` already absolute when present).
  Returns a map with only the keys that can be calculated:
  `:dividends-paid`, `:stock-repurchase`, `:dividend-coverage`
  (net income / dividends paid), and `:dividend-buyback-coverage`
  (net income / dividends paid + stock repurchase). A missing cash leg
  counts as zero when the other is positive."
  [ds-row concept-row]
  (let [paid (positive-amount (:dividends-paid concept-row))
        repurchase (positive-amount (:stock-repurchase concept-row))
        ni (net-income ds-row concept-row)
        coverage (when (and (number? paid) (number? ni)
                            (not (zero? (double paid))))
                   (/ (double ni) (double paid)))
        returned (when (or (number? paid) (number? repurchase))
                   (+ (double (or paid 0)) (double (or repurchase 0))))
        bb-coverage (when (and (number? returned)
                               (pos? (double returned))
                               (number? ni))
                      (/ (double ni) (double returned)))]
    (cond-> {}
      (number? paid) (assoc :dividends-paid paid)
      (number? repurchase) (assoc :stock-repurchase repurchase)
      (number? coverage) (assoc :dividend-coverage coverage)
      (number? bb-coverage) (assoc :dividend-buyback-coverage bb-coverage))))
