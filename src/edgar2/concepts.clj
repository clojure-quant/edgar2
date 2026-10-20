(ns edgar2.concepts
  "Canonical XBRL concept fallback chains for the project.

  Each concept is {:id :label :data}. :data is usually a priority-ordered
  seq of [taxonomy tag] pairs. A few entries use a label→pairs map instead
  (see :ifrs-income-by-label and :income-fallbacks-by-label).

  Public API: get-concept-id, series-mixed."
  (:require [tech.v3.dataset :as ds]))

(def concepts
  [{:id :revenue
    :label "Revenue"
    :data [[:us-gaap :Revenues]
           [:us-gaap :RealEstateRevenueNet]
           [:us-gaap :SalesRevenueNet]
           [:us-gaap :SalesRevenueGoodsNet]
           [:us-gaap :RevenuesNetOfInterestExpense]
           [:us-gaap :InterestAndDividendIncomeOperating]
           [:us-gaap :PremiumsEarnedNet]
           [:us-gaap :RevenueFromContractWithCustomerExcludingAssessedTax]
           [:us-gaap :RevenueFromContractWithCustomerIncludingAssessedTax]
           [:ifrs-full :Revenue]
           [:ifrs-full :RevenueFromContractsWithCustomers]
           [:ifrs-full :RevenueFromSaleOfGoods]]}

   {:id :cost-of-revenue
    :label "Cost of Revenue"
    :data [[:us-gaap :CostOfRevenue]
           [:us-gaap :CostOfGoodsSold]
           [:us-gaap :CostOfGoodsAndServicesSold]
           [:ifrs-full :CostOfSales]]}

   {:id :gross-profit
    :label "Gross Profit"
    :data [[:us-gaap :GrossProfit]
           [:ifrs-full :GrossProfit]]}

   {:id :operating-expenses
    :label "Operating Expenses"
    :data [[:us-gaap :OperatingExpenses]
           [:ifrs-full :OperatingExpense]]}

   {:id :operating-income
    :label "Operating Income"
    :data [[:us-gaap :OperatingIncomeLoss]
           [:ifrs-full :ProfitLossFromOperatingActivities]]}

   {:id :nonoperating-income
    :label "Non-Operating Income"
    :data [[:us-gaap :NonoperatingIncomeExpense]]}

   {:id :asset-gains
    :label "Gain (Loss) on Sale of Assets"
    :data [[:us-gaap :GainLossOnDispositionOfAssets1]
           [:us-gaap :GainsLossesOnSalesOfAssets]
           [:us-gaap :GainLossOnDispositionOfAssets]
           [:us-gaap :GainLossOnSaleOfPropertyPlantEquipment]
           [:us-gaap :GainLossOnDispositionOfProperty]]}

   {:id :interest-expense
    :label "Interest Expense"
    :data [[:us-gaap :InterestExpense]
           [:us-gaap :InterestExpenseDebt]
           [:us-gaap :InterestExpenseNonoperating]
           [:us-gaap :InterestAndDebtExpense]
           [:ifrs-full :InterestExpense]
           [:ifrs-full :InterestExpenseOnBorrowings]]}

   {:id :pretax-income
    :label "Pre-Tax Income"
    :data [[:us-gaap :IncomeLossFromContinuingOperationsBeforeIncomeTaxesExtraordinaryItemsNoncontrollingInterest]
           [:us-gaap :IncomeLossFromContinuingOperationsBeforeIncomeTaxesMinorityInterestAndIncomeLossFromEquityMethodInvestments]
           [:ifrs-full :ProfitLossBeforeTax]]}

   {:id :net-income
    :label "Net Income"
    :data [[:us-gaap :NetIncomeLoss]
           [:us-gaap :ProfitLoss]
           [:ifrs-full :ProfitLoss]
           [:ifrs-full :ProfitLossAttributableToOwnersOfParent]]}

   {:id :eps
    :label "Earnings Per Share"
    :data [[:us-gaap :EarningsPerShareDiluted]
           [:us-gaap :EarningsPerShareBasic]
           [:ifrs-full :DilutedEarningsLossPerShare]
           [:ifrs-full :BasicEarningsLossPerShare]
           [:ifrs-full :BasicAndDilutedEarningsLossPerShare]]}

   {:id :income-tax
    :label "Income Tax Expense"
    :data [[:us-gaap :IncomeTaxExpenseBenefit]
           [:ifrs-full :IncomeTaxExpenseContinuingOperations]]}

   {:id :dps-cash
    :label "Dividends Per Share (Cash)"
    :data [[:us-gaap :CommonStockDividendsPerShareCashPaid]]}

   {:id :dps-other
    :label "Dividends Per Share (Other)"
    :data [[:us-gaap :CommonStockDividendsPerShare]
           [:us-gaap :CommonStockDividendsPerShareDeclared]
           [:ifrs-full :DividendsPaidOrdinaryShares]]}

   {:id :dividends-paid
    :label "Dividends Paid"
    :data [[:us-gaap :PaymentsOfDividends]
           [:us-gaap :PaymentsOfOrdinaryDividends]
           [:us-gaap :PaymentsOfDividendsCommonStock]
           [:ifrs-full :DividendsPaidClassifiedAsFinancingActivities]
           [:ifrs-full :DividendsPaidToEquityHoldersOfParentClassifiedAsFinancingActivities]
           [:ifrs-full :DividendsPaid]]}

   {:id :stock-repurchase
    :label "Stock Repurchase"
    :data [[:us-gaap :PaymentsForRepurchaseOfCommonStock]
           [:us-gaap :PaymentsForRepurchaseOfEquity]
           [:ifrs-full :PaymentsToAcquireOrRedeemEntitysShares]]}

   {:id :assets
    :label "Assets"
    :data [[:us-gaap :Assets]
           [:ifrs-full :Assets]]}

   {:id :total-equity
    :label "Total Equity"
    :data [[:us-gaap :StockholdersEquityIncludingPortionAttributableToNoncontrollingInterest]
           [:ifrs-full :Equity]]}

   {:id :parent-equity
    :label "Parent Equity"
    :data [[:us-gaap :StockholdersEquity]
           [:ifrs-full :EquityAttributableToOwnersOfParent]]}

   {:id :nci
    :label "Noncontrolling Interest"
    :data [[:us-gaap :MinorityInterest]
           [:ifrs-full :NoncontrollingInterests]]}

   {:id :cash
    :label "Cash"
    :data [[:us-gaap :CashAndCashEquivalentsAtCarryingValue]
           [:ifrs-full :CashAndCashEquivalents]
           [:us-gaap :Cash]]}

   {:id :preferred-stock
    :label "Preferred Stock"
    :data [[:us-gaap :PreferredStockValue]]}

   {:id :debt
    :label "Debt"
    :data [[:us-gaap :LongTermDebtNoncurrent]
           [:ifrs-full :LongtermBorrowings]
           [:us-gaap :LongTermDebtCurrent]
           [:ifrs-full :CurrentBorrowingsAndCurrentPortionOfNoncurrentBorrowings]
           [:ifrs-full :ShorttermBorrowings]
           [:us-gaap :LongTermDebt]
           [:ifrs-full :Borrowings]
           [:us-gaap :CommercialPaper]
           [:us-gaap :ShortTermBorrowings]
           [:us-gaap :OtherShortTermBorrowings]]}

   {:id :common-shares-outstanding
    :label "Common Shares Outstanding"
    :data [[:dei :EntityCommonStockSharesOutstanding]
           [:us-gaap :CommonStockSharesOutstanding]]}

   {:id :ifrs-shares-outstanding
    :label "IFRS Shares Outstanding"
    :data [[:ifrs-full :NumberOfSharesOutstanding]]}

   {:id :weighted-average-shares
    :label "Weighted Average Shares"
    :data [[:us-gaap :WeightedAverageNumberOfSharesOutstandingBasic]
           [:us-gaap :WeightedAverageNumberOfSharesOutstandingBasicAndDiluted]
           [:us-gaap :WeightedAverageNumberOfDilutedSharesOutstanding]]}

   {:id :depreciation
    :label "Depreciation"
    :data [[:ifrs-full :DepreciationAndAmortisationExpense]
           [:us-gaap :DepreciationExpense]
           [:us-gaap :DepreciationAndAmortization]
           [:us-gaap :Depreciation]
           [:us-gaap :DepreciationDepletionAndAmortization]]}

   {:id :ifrs-income-by-label
    :label "IFRS income supplements by statement line"
    :data {"Revenue" [[:ifrs-full :RevenueFromContractsWithCustomers]
                      [:ifrs-full :Revenue]]
           "Cost of Revenue" [[:ifrs-full :CostOfSales]]
           "Operating Expenses" [[:ifrs-full :OperatingExpense]]
           "Operating Income" [[:ifrs-full :ProfitLossFromOperatingActivities]]
           "Pre-Tax Income" [[:ifrs-full :ProfitLossBeforeTax]]
           "Income Tax Expense" [[:ifrs-full :IncomeTaxExpenseContinuingOperations]]
           "EPS Basic" [[:ifrs-full :BasicEarningsLossPerShare]]
           "EPS Diluted" [[:ifrs-full :DilutedEarningsLossPerShare]]}}

   {:id :income-fallbacks-by-label
    :label "Extra income fallbacks by statement line"
    :data {"Total Revenue" [[:us-gaap :RevenueFromContractWithCustomerExcludingAssessedTax]
                            [:us-gaap :RevenueFromContractWithCustomerIncludingAssessedTax]
                            [:us-gaap :SalesRevenueNet]]}}])

(def concept-dict
  (into {} (map (juxt :id identity) concepts)))

(defn get-concept-id
  "Concept record for `id`, or nil."
  [id]
  (get concept-dict id))

(defn- as-number
  [v]
  (when (number? v)
    (if (and (float? v)
             (== (double v) (Math/rint (double v)))
             (<= (Math/abs (double v)) (double Long/MAX_VALUE)))
      (long v)
      v)))

(defn series-mixed
  "Oldest-first [{:end :val :concept} ...] from a wide `report-table` dataset.

  `tag-pairs` is a priority list of `[taxonomy concept-kw]` (same shape as
  concept `:data`). For each period row, the first tag whose local-name
  column is numeric wins — so later years may pick a different concept
  than earlier years (e.g. NetIncomeLoss then ProfitLoss)."
  [table tag-pairs]
  (->> (ds/rows table {:nil-missing? true})
       reverse
       (keep (fn [row]
               (some (fn [[_tax concept]]
                       (when-let [v (as-number (get row (name concept)))]
                         {:end (str (:end row))
                          :val v
                          :concept concept}))
                     tag-pairs)))
       vec))
