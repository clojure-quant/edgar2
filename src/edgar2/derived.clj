(ns edgar2.derived
  "Derived income-statement lines shared by the financials page and stats.

  Operating expenses, when that line is not already present:
  revenue + non-operating income + asset gains − pre-tax income
  − cost of revenue − interest.

  Operating profit, on those same periods, when operating income is not
  already present: gross profit − operating expenses.
  Gross profit is the gross-profit line, or revenue − cost of revenue.")

(def operating-expenses "Operating Expenses")
(def operating-income "Operating Income")
(def gross-profit "Gross Profit")
(def revenue "Revenue")
(def cost-of-revenue "Cost of Revenue")
(def pretax-income "Pre-Tax Income")
(def nonoperating-income "Non-Operating Income")
(def asset-gains "Gain (Loss) on Sale of Assets")
(def interest-expense "Interest Expense")

(def statement-lines
  "Line label → concept pairs, first match per line.
  Same order as the standard income-statement chains, then the IFRS names."
  [[revenue
    [[:us-gaap :RevenueFromContractWithCustomerExcludingAssessedTax]
     [:us-gaap :Revenues]
     [:us-gaap :SalesRevenueNet]
     [:us-gaap :SalesRevenueGoodsNet]
     [:us-gaap :RevenueFromContractWithCustomerIncludingAssessedTax]
     [:ifrs-full :RevenueFromContractsWithCustomers]
     [:ifrs-full :Revenue]
     [:ifrs-full :RevenueFromSaleOfGoods]]]
   [cost-of-revenue
    [[:us-gaap :CostOfRevenue]
     [:us-gaap :CostOfGoodsSold]
     [:us-gaap :CostOfGoodsAndServicesSold]
     [:ifrs-full :CostOfSales]]]
   [gross-profit
    [[:us-gaap :GrossProfit]
     [:ifrs-full :GrossProfit]]]
   [operating-expenses
    [[:us-gaap :OperatingExpenses]
     [:ifrs-full :OperatingExpense]]]
   [operating-income
    [[:us-gaap :OperatingIncomeLoss]
     [:ifrs-full :ProfitLossFromOperatingActivities]]]
   [nonoperating-income
    [[:us-gaap :NonoperatingIncomeExpense]]]
   [asset-gains
    [[:us-gaap :GainLossOnDispositionOfAssets1]
     [:us-gaap :GainsLossesOnSalesOfAssets]
     [:us-gaap :GainLossOnDispositionOfAssets]
     [:us-gaap :GainLossOnSaleOfPropertyPlantEquipment]
     [:us-gaap :GainLossOnDispositionOfProperty]]]
   [interest-expense
    [[:us-gaap :InterestExpense]
     [:us-gaap :InterestExpenseDebt]
     [:us-gaap :InterestExpenseNonoperating]
     [:us-gaap :InterestAndDebtExpense]
     [:ifrs-full :InterestExpense]
     [:ifrs-full :InterestExpenseOnBorrowings]]]
   [pretax-income
    [[:us-gaap :IncomeLossFromContinuingOperationsBeforeIncomeTaxesExtraordinaryItemsNoncontrollingInterest]
     [:us-gaap :IncomeLossFromContinuingOperationsBeforeIncomeTaxesMinorityInterestAndIncomeLossFromEquityMethodInvestments]
     [:ifrs-full :ProfitLossBeforeTax]]]])

(defn derived-operating-expenses
  "Operating expenses implied by the income-statement identity.
  Nil when revenue or pre-tax income is missing."
  [{:keys [revenue pretax nonoperating-income asset-gains cost-of-revenue interest]}]
  (when (and (number? revenue) (number? pretax))
    (- (+ (double revenue)
          (double (or nonoperating-income 0))
          (double (or asset-gains 0)))
       (double pretax)
       (double (or cost-of-revenue 0))
       (double (or interest 0)))))

(defn operating-profit
  "Gross profit minus operating expenses. Nil unless both are numbers."
  [gross-profit operating-expenses]
  (when (and (number? gross-profit) (number? operating-expenses))
    (- (double gross-profit) (double operating-expenses))))

(defn- at
  [by-item line end]
  (get (get by-item line) end))

(defn- gross-profit-at
  [by-item end]
  (or (at by-item gross-profit end)
      (let [rev (at by-item revenue end)
            cogs (at by-item cost-of-revenue end)]
        (when (and (number? rev) (number? cogs))
          (- (double rev) (double cogs))))))

(defn- fill-operating-expenses
  [by-item ends]
  (let [existing (or (get by-item operating-expenses) {})
        filled (reduce (fn [m end]
                         (if (number? (get m end))
                           m
                           (if-let [v (derived-operating-expenses
                                       {:revenue (at by-item revenue end)
                                        :pretax (at by-item pretax-income end)
                                        :nonoperating-income (at by-item nonoperating-income end)
                                        :asset-gains (at by-item asset-gains end)
                                        :cost-of-revenue (at by-item cost-of-revenue end)
                                        :interest (at by-item interest-expense end)})]
                             (assoc m end v)
                             m)))
                       existing
                       ends)]
    (assoc by-item operating-expenses filled)))

(defn- fill-operating-profit
  "Operating income on `derived-ends` only: periods whose operating
  expenses were just derived, and which do not already have operating income."
  [by-item derived-ends]
  (let [opex (or (get by-item operating-expenses) {})
        existing (or (get by-item operating-income) {})
        filled (reduce (fn [m end]
                         (if (number? (get m end))
                           m
                           (if-let [v (operating-profit (gross-profit-at by-item end)
                                                        (get opex end))]
                             (assoc m end v)
                             m)))
                       existing
                       derived-ends)]
    (assoc by-item operating-income filled)))

(defn with-derived-lines
  "`by-item` is {line-label {period-end number}}. Fills operating expenses
  on `ends` that lack them, then operating income on exactly those ends
  when operating income is absent. Reported numbers stay."
  [by-item ends]
  (let [before (or (get by-item operating-expenses) {})
        with-opex (fill-operating-expenses by-item ends)
        derived-ends (filter (fn [end]
                               (and (not (number? (get before end)))
                                    (number? (get-in with-opex [operating-expenses end]))))
                             ends)]
    (fill-operating-profit with-opex derived-ends)))
