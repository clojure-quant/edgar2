(ns edgar2.financials.calc.concept
  "Per-period concept vectors from an annual (or quarterly) report-table."
  (:require [edgar2.financials.concepts :as concepts]
            [edgar2.financials.derived :as derived]
            [edgar2.financials.facts-ds :as facts-ds]
            [tech.v3.dataset :as ds]))

(defn- as-number
  [v]
  (when (number? v)
    (if (and (float? v)
             (== (double v) (Math/rint (double v)))
             (<= (Math/abs (double v)) (double Long/MAX_VALUE)))
      (long v)
      v)))

(defn- ends
  [table]
  (mapv #(str (:end %)) (ds/rows table {:nil-missing? true})))

(defn- col-num-vec
  "Newest-first numeric vector for a concept column on `table`, else nils."
  [table concept]
  (let [k (name concept)
        n (ds/row-count table)]
    (if (ds/has-column? table k)
      (mapv as-number (ds/column table k))
      (vec (repeat n nil)))))

(defn- or-nums
  "Per-index first number among parallel column vectors."
  [& cols]
  (apply mapv (fn [& vs] (some #(when (number? %) %) vs)) cols))

(defn approx=
  "True when a and b are within 2% or 1 unit (tag double-counts)."
  [a b]
  (and (number? a) (number? b)
       (let [a (double a)
             b (double b)]
         (<= (Math/abs (- a b))
             (max 1.0 (* 0.02 (Math/abs b)))))))

(defn sum-unique
  "Sum xs, counting near-duplicate tags once."
  [xs]
  (loop [acc [] xs (filter number? xs)]
    (if-let [x (first xs)]
      (if (some #(approx= % x) acc)
        (recur acc (next xs))
        (recur (conj acc (double x)) (next xs)))
      (when (seq acc)
        (reduce + acc)))))

(defn- debt-at
  "Interest-bearing debt for one period from component values."
  [non cur ltd cp stb ost]
  (let [term (cond
               (and (number? non) (number? cur)) (+ (double non) (double cur))
               (and (number? non) (number? ltd)
                    (> (double ltd) (* 1.01 (double non)))) (double ltd)
               (number? non) (double non)
               (number? ltd) (double ltd)
               (number? cur) (double cur))
        already (when (and (number? cur) (number? term)
                           (or (and (number? non) (number? cur))
                               (approx= ltd term)))
                  cur)
        extras (sum-unique
                (remove #(approx= % already) [cp stb ost]))]
    (when (or term extras)
      (+ (double (or term 0)) (double (or extras 0))))))

(defn interest-bearing-debt
  "Per-row interest-bearing debt from a report-table dataset (newest first).

  Term debt is the noncurrent portion plus the current portion when both
  are tagged, otherwise the long-term debt total (US-GAAP LongTermDebt or
  IFRS Borrowings). Commercial paper and other short-term borrowings are
  added unless they repeat that current portion."
  [table]
  (mapv debt-at
        (or-nums (col-num-vec table :LongTermDebtNoncurrent)
                 (col-num-vec table :LongtermBorrowings))
        (or-nums (col-num-vec table :LongTermDebtCurrent)
                 (col-num-vec table :CurrentBorrowingsAndCurrentPortionOfNoncurrentBorrowings)
                 (col-num-vec table :ShorttermBorrowings))
        (or-nums (col-num-vec table :LongTermDebt)
                 (col-num-vec table :Borrowings))
        (col-num-vec table :CommercialPaper)
        (col-num-vec table :ShortTermBorrowings)
        (col-num-vec table :OtherShortTermBorrowings)))

(defn total-equity
  "Per-row total equity from a report-table dataset (newest first).

  Prefer `calc-concept` `:total-equity` (tags that already include NCI).
  When that is empty for the whole table, use parent equity, adding NCI
  on periods that report both."
  [table]
  (let [inclusive (facts-ds/calc-concept table :total-equity)]
    (if (some number? inclusive)
      inclusive
      (mapv (fn [p n]
              (cond
                (and (number? p) (number? n)) (+ (double p) (double n))
                (number? p) p
                :else nil))
            (facts-ds/calc-concept table :parent-equity)
            (facts-ds/calc-concept table :nci)))))

(defn- statement-by-item
  "Annual {line-label {end value}} via `calc-concept` for derived lines."
  [table]
  (let [es (ends table)]
    (into {}
          (keep (fn [id]
                  (let [label (:label (concepts/get-concept-id id))
                        vals (facts-ds/calc-concept table id)
                        by-end (into {}
                                     (keep (fn [[e v]]
                                             (when (number? v) [e v]))
                                           (map vector es vals)))]
                    (when (seq by-end)
                      [label by-end])))
                derived/statement-line-ids))))

(defn- derived-operating-income
  "end → operating income filled only where derived (not reported)."
  [table]
  (let [by-item (statement-by-item table)
        item-ends (->> (vals by-item) (mapcat keys) distinct sort vec)
        filled (derived/with-derived-lines by-item item-ends)
        before (get by-item derived/operating-income)]
    (into {}
          (keep (fn [[end v]]
                  (when (and (number? v)
                             (not (number? (get before end))))
                    [end v]))
                (get filled derived/operating-income)))))

(defn operating-income
  "Per-row operating income: reported tags, else derived when available."
  [table]
  (let [reported (facts-ds/calc-concept table :operating-income)
        derived-oi (derived-operating-income table)]
    (mapv (fn [e v]
            (or (when (number? v) v)
                (get derived-oi e)))
          (ends table)
          reported)))

(defn concept-absolute
  "Per-row absolute values for `concept-id` on a report-table (newest first).

  Non-positive amounts become nil (cash outflows stored as negatives in XBRL
  still yield a positive amount)."
  [table concept-id]
  (mapv (fn [v]
          (when (number? v)
            (let [amount (Math/abs (double v))]
              (when (pos? amount) amount))))
        (facts-ds/calc-concept table concept-id)))

(defn- first-tag-val
  [row tag-pairs]
  (some (fn [[_ c]]
          (as-number (get row (name c))))
        tag-pairs))

(defn derived-ebit-row
  "EBIT for one report-table row: net income + interest + tax.

  Nil unless net income is present and at least one of interest or tax is."
  [row]
  (let [ni (first-tag-val row (:data (concepts/get-concept-id :net-income)))
        interest (first-tag-val row (:data (concepts/get-concept-id :interest-expense)))
        tax (first-tag-val row (:data (concepts/get-concept-id :income-tax)))]
    (when (and ni (or interest tax))
      (+ (double ni)
         (double (or interest 0))
         (double (or tax 0))))))

(defn derived-ebit
  "Per-row derived EBIT (net income + interest + tax), newest first."
  [ds]
  (mapv derived-ebit-row (ds/rows ds {:nil-missing? true})))

(defn actual-or-derived-ebit
  "Per-row EBIT: prefer reported `ebit-concept` values, else derived.

  `ebit-concept` is a newest-first vector aligned with `ds` (e.g. operating
  income from `calc-concept` / `operating-income`). Comparison is row by
  row — a missing reported value falls back to `derived-ebit` for that row."
  [ds ebit-concept]
  (mapv (fn [reported derived]
          (or (when (number? reported) reported) derived))
        ebit-concept
        (derived-ebit ds)))

(def concept-ids
  "Concept :ids whose per-period vectors are added by `calc-concepts`."
  [:revenue :net-income :assets :operating-income :total-equity :debt :eps :dps
   :dividends-paid :stock-repurchase :ebit :cash :preferred-stock :nci])

(defn calc-concepts
  "Wide dataset of calculated concepts aligned with `ds-annual`.

  Keeps `:filed :period :end` from the report-table. Adds one column per
  entry in `concept-ids` (column name = concept `:id`). Values are
  newest-first, same row order as `ds-annual`. `:dividends-paid` and
  `:stock-repurchase` are absolute (positive) amounts. `:ebit` prefers
  operating income, else net income + interest + tax."
  [ds-annual]
  (let [period (ds/select-columns ds-annual [:filed :period :end])
        oi (operating-income ds-annual)]
    (-> period
        (ds/add-or-update-column :revenue
                                 (facts-ds/calc-concept ds-annual :revenue))
        (ds/add-or-update-column :net-income
                                 (facts-ds/calc-concept ds-annual :net-income))
        (ds/add-or-update-column :assets
                                 (facts-ds/calc-concept ds-annual :assets))
        (ds/add-or-update-column :operating-income oi)
        (ds/add-or-update-column :total-equity
                                 (total-equity ds-annual))
        (ds/add-or-update-column :debt
                                 (interest-bearing-debt ds-annual))
        (ds/add-or-update-column :eps
                                 (facts-ds/calc-concept ds-annual :eps))
        (ds/add-or-update-column :dps
                                 (facts-ds/calc-concept ds-annual :dps))
        (ds/add-or-update-column :dividends-paid
                                 (concept-absolute ds-annual :dividends-paid))
        (ds/add-or-update-column :stock-repurchase
                                 (concept-absolute ds-annual :stock-repurchase))
        (ds/add-or-update-column :ebit
                                 (actual-or-derived-ebit ds-annual oi))
        (ds/add-or-update-column :cash
                                 (facts-ds/calc-concept ds-annual :cash))
        (ds/add-or-update-column :preferred-stock
                                 (facts-ds/calc-concept ds-annual :preferred-stock))
        (ds/add-or-update-column :nci
                                 (facts-ds/calc-concept ds-annual :nci)))))
