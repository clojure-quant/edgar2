(ns edgar2.financials.calc.math
  (:require
   [tech.v3.dataset :as ds]
   [tablecloth.api :as tc]
   [edgar2.financials.calc.util :as u]
   ))

(defn prefer [preferred-col fallback-col]
  (map (fn [p f]
         (if p p f))
       preferred-col fallback-col))

(defn absolute-concept [ds concept]
  (u/assert-cols ds [concept])
  (map (fn [c]
         (when c (Math/abs c)))
       (concept ds)))

;; derived calculation 

(defn gross-profit [concept-ds]
  (u/assert-cols concept-ds [:revenue :cost-of-revenue :gross-profit])
  (prefer
   (:gross-profit concept-ds)
   (u/subtract concept-ds :revenue :cost-of-revenue)))

(defn deprecion-amortization [concept-ds]
  (u/assert-cols concept-ds [:depreciation-amortization :depreciation :amortization])
  (prefer
   (:depreciation-amortization concept-ds)
   (u/add concept-ds :depreciation :amortization)))
 
#_(defn deprecion-amortization [concept-ds]
  (add concept-ds :depreciation :amortization))

(defn- derived-operating-expenses
  "Operating expenses from the income-statement identity, for one row:

  revenue + non-operating income + asset gains − pre-tax income
  − cost of revenue − interest.

  Nil when revenue or pre-tax income is missing. This is the identity
  result only; `operating-expenses` prefers a reported tag."
  [{:keys [revenue pretax-income nonoperating-income asset-gains
           cost-of-revenue interest-expense]}]
  (when (and (number? revenue) (number? pretax-income))
    (- (+ (double revenue)
          (double (or nonoperating-income 0))
          (double (or asset-gains 0)))
       (double pretax-income)
       (double (or cost-of-revenue 0))
       (double (or interest-expense 0)))))

(defn- operating-expenses
  "Per-row operating expenses: reported `:operating-expenses` else the
  income-statement identity.

  Returns `[values derived?]`; `derived?` marks the rows filled from the
  identity rather than a reported tag, and is the mask `operating-income`
  needs."
  [concept-ds]
  (u/assert-cols concept-ds [:operating-expenses :pretax-income
                             :nonoperating-income :asset-gains
                             :revenue :cost-of-revenue :interest-expense])
  (let [rows (ds/mapseq-reader concept-ds)
        reported (mapv :operating-expenses rows)
        derived (mapv derived-operating-expenses rows)]
    [(mapv (fn [r d] (if (number? r) r d)) reported derived)
     (mapv (fn [r d] (and (not (number? r)) (number? d))) reported derived)]))

(defn- operating-income
  "Per-row operating income: reported `:operating-income`, else gross profit
  minus operating expenses only where operating expenses was derived."
  [concept-1-ds derived?]
  (u/assert-cols concept-1-ds [:operating-income :gross-profit :operating-expenses])
  (mapv (fn [row d?]
          (let [reported (:operating-income row)]
            (if (number? reported)
              reported
              (when (and d?
                         (number? (:gross-profit row))
                         (number? (:operating-expenses row)))
                (- (double (:gross-profit row))
                   (double (:operating-expenses row)))))))
        (ds/mapseq-reader concept-1-ds)
        derived?))

#_(defn calc-operating-expenses
    "Operating expenses from the income-statement identity.

  revenue + non-operating income + asset gains − pre-tax income
  − cost of revenue − interest. Nil when revenue or pre-tax income is
  missing. Does not look at a reported operating-expenses tag; `derive` does."
    [ds]
    (let [by-item (statement-by-item ds)]
      (mapv (fn [end]
              (derived/derived-operating-expenses
               {:revenue (at by-item derived/revenue end)
                :pretax (at by-item derived/pretax-income end)
                :nonoperating-income (at by-item derived/nonoperating-income end)
                :asset-gains (at by-item derived/asset-gains end)
                :cost-of-revenue (at by-item derived/cost-of-revenue end)
                :interest (at by-item derived/interest-expense end)}))
            (ends ds))))

#_(defn calc-operating-income
    "Gross profit minus operating expenses, only where operating expenses came
  from the identity and operating income was not reported.

  Gross profit is the reported gross-profit line, else revenue − cost of
  revenue. Same periods as `derived/with-derived-lines`."
    [ds]
    (let [es (ends ds)
          by-item (statement-by-item ds)
          filled (derived/with-derived-lines by-item es)
          before (get by-item derived/operating-income)]
      (mapv (fn [end]
              (let [v (at filled derived/operating-income end)]
                (when (and (number? v)
                           (not (number? (get before end))))
                  v)))
            es)))

(defn ebit
  "EBIT for one report-table row: net income + interest + tax."
  [concept-ds]
  (u/assert-cols concept-ds [:ebit :net-income :interest-expense :income-tax])
  (prefer
   (:ebit concept-ds)
   (u/add concept-ds :net-income :interest-expense :income-tax)))

(defn ebitda
  [concept-ds]
  (u/assert-cols concept-ds [:ebit :depreciation-amortization])
  (u/add concept-ds :ebit :depreciation-amortization))



(defn total-equity [concept-ds]
  (u/assert-cols concept-ds [:total-equity :parent-equity :nci])
  (prefer
   (:total-equity concept-ds)
   (u/add concept-ds :parent-equity :nci)))

(defn interest-bearing-debt
  "Term debt is the noncurrent portion plus the current portion when both
  are tagged, otherwise the long-term debt total (US-GAAP LongTermDebt or
  IFRS Borrowings). Commercial paper and other short-term borrowings are
  added unless they repeat that current portion."
  [concept-ds]
  (u/assert-cols concept-ds [:debt-longterm :debt-shortterm])
  (u/add concept-ds :debt-longterm :debt-shortterm)
  ;:LongTermDebtNoncurrent :LongtermBorrowings :LongTermDebtCurrent
  ;:CurrentBorrowingsAndCurrentPortionOfNoncurrentBorrowings :ShorttermBorrowings
  ;:LongTermDebt :Borrowings :CommercialPaper :ShortTermBorrowings :OtherShortTermBorrowings
  )

(defn- capital-employed
  "Per-period assets minus current liabilities.
  Nil where either side is missing. A missing current-liabilities figure
  is not treated as zero."
  [concept-ds]
  (u/assert-cols concept-ds [:assets :liabilities-current])
  (u/subtract-when-exist concept-ds :assets :liabilities-current))


;; concept-math-ds

(defn calculate-math [concept-ds]
  (let [[opex opex-derived?] (operating-expenses concept-ds)
        concept-1-ds (tc/add-or-replace-columns
                      concept-ds
                      {:gross-profit (gross-profit concept-ds)
                       :depreciation-amortization (deprecion-amortization concept-ds)
                       :ebit (ebit concept-ds)
                       :operating-expenses opex
                       :dividends-paid-abs (absolute-concept concept-ds :dividends-paid)       
                       :stock-repurchase-abs (absolute-concept concept-ds :stock-repurchase)
                       })]
    (-> concept-1-ds
        (tc/add-or-replace-columns
         {:ebitda (ebitda concept-1-ds)
          :total-equity (total-equity concept-1-ds)
          :debt (interest-bearing-debt concept-1-ds)
          :capital-employed (capital-employed concept-1-ds)
          :dividend-buyback (u/add concept-1-ds :dividends-paid-abs :stock-repurchase-abs)
          :operating-income (operating-income concept-1-ds opex-derived?)
          }))))


(defn calculate-ratios [concept-math-ds]
   (-> concept-math-ds
      (tc/add-or-replace-columns
       {:roce (u/divide concept-math-ds :net-income :capital-employed)
        :net-income-margin (u/divide concept-math-ds :net-income :revenue)
        :roa (u/divide concept-math-ds :net-income :assets)
        :debt2ebitda (u/divide concept-math-ds :debt :ebitda)
        :dividend-coverage (u/divide concept-math-ds :net-income :dividends-paid-abs)
        :buyback-coverage (u/divide concept-math-ds :net-income :stock-repurchase-abs)
        :dividend-buyback-coverage (u/divide concept-math-ds :net-income :dividend-buyback)
       })))
  
(comment 
  

  (def ds-dep-a (tc/dataset [{:depreciation 1
                              :amortization 2
                              :depreciation-amortization 3
                              }
                             {:depreciation 1
                              :amortization 2
                              :depreciation-amortization 5}
                             {:depreciation 1
                              :amortization 2
                              :depreciation-amortization nil}
                             ]))
  
  (deprecion-amortization ds-dep-a)

  (calculate-math ds-dep-a)
 

  (tc/column-names ds-dep-a)
  (u/contains-col ds-dep-a :a)
  (u/contains-col ds-dep-a :depreciation)
  (u/contains-cols ds-dep-a [:depreciation :amortization])

;
  )