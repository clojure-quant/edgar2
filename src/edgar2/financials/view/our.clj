(ns edgar2.financials.view.our
  "Our curated statement rows: P&L + balance sheet + ratios.

  Most rows read a `calc.concept/calc-concepts` column (`:our`); rows with
  no calc-concepts equivalent fall back to an edgarjure statement label
  (`:edgarjure`). Labels are unique because they become column names.

  Every non-ratio row carries `:format :millions` (dollars and share
  counts); ratio rows carry `:format :percent` (a fraction shown ×100) or
  `:format :dec2` (a multiple).")

(defn our-view
  "Our curated statement rows, in display order."
  []
  [; income
   {:our :revenue
    :label "Revenue"
    :section :income
    :format :millions}
   {:our :cost-of-revenue
    :label "Cost of Revenue"
    :section :income
    :format :millions}
   {:our :gross-profit
    :label "Gross Profit"
    :section :income
    :style :background-pl-green
    :format :millions}
   {:edgarjure "SG&A Expense"
    :label "SG&A Expense"
    :section :income
    :format :millions}
   {:edgarjure "Selling and Marketing Expense"
    :label "Selling and Marketing Expense"
    :section :income
    :format :millions}
   {:edgarjure "General and Administrative Expense"
    :label "General and Administrative Expense"
    :section :income
    :format :millions}
   {:edgarjure "R&D Expense"
    :label "R&D Expense"
    :section :income
    :format :millions}
   {:our :depreciation
    :label "Depreciation"
    :section :income
    :format :millions}
   {:our :depreciation-amortization
    :label "Depreciation & Amortization"
    :section :income
    :format :millions}
   {:our :amortization
    :label "Amortization"
    :section :income
    :format :millions}
   {:our :operating-expenses
    :label "Operating Expenses"
    :section :income
    :format :millions}
   {:our :operating-income
    :label "Operating Income"
    :section :income
    :style :background-pl-green
    :format :millions}
   {:our :ebit
    :label "EBIT"
    :section :income
    :format :millions}
   {:our :ebitda
    :label "EBITDA"
    :section :income
    :format :millions}
   {:our :interest-expense
    :label "Interest Expense"
    :section :income
    :format :millions}
   {:our :nonoperating-income
    :label "Non-Operating Income"
    :section :income
    :format :millions}
   {:our :asset-gains
    :label "Gain (Loss) on Sale of Assets"
    :section :income
    :format :millions}
   {:our :pretax-income
    :label "Pre-Tax Income"
    :section :income
    :format :millions}
   {:our :income-tax
    :label "Income Tax Expense"
    :section :income
    :format :millions}
   {:our :net-income
    :label "Net Income"
    :section :income
    :style :background-pl-green
    :format :millions}
   {:concept :shares-outstanding
    :label "Shares Outstanding"
    :section :income
    :format :millions}
   {:our :dividends-paid-abs
    :label "Dividends Paid"
    :section :income
    :style :background-pl-green
    :format :millions}
   {:our :dividends-paid
    :label "Dividends Paid (reported)"
    :section :income
    :format :millions}
   {:our :stock-repurchase-abs
    :label "Stock Repurchase"
    :section :income
    :style :background-pl-green
    :format :millions}
   {:our :stock-repurchase
    :label "Stock Repurchase (reported)"
    :section :income
    :format :millions}
   ; balance
   {:our :cash
    :label "Cash"
    :section :balance
    :style :background-balance-active
    :format :millions}
   {:edgarjure "Accounts Receivable"
    :label "Accounts Receivable"
    :section :balance
    :style :background-balance-active
    :format :millions}
   {:edgarjure "Inventory"
    :label "Inventory"
    :section :balance
    :style :background-balance-active
    :format :millions}
   {:our :assets-current
    :label "Current Assets"
    :section :balance
    :style :background-balance-active
    :format :millions}
   {:edgarjure "PP&E Net"
    :label "PP&E Net"
    :section :balance
    :style :background-balance-active
    :format :millions}
   {:our :assets
    :label "Total Assets"
    :section :balance
    :style :background-balance-active
    :format :millions}
   {:edgarjure "Accounts Payable"
    :label "Accounts Payable"
    :section :balance
    :style :background-balance-passive
    :format :millions}
   {:edgarjure "Current Debt"
    :label "Current Debt"
    :section :balance
    :style :background-balance-passive
    :format :millions}
   {:edgarjure "Current Portion of Long-Term Debt"
    :label "Current Portion of Long-Term Debt"
    :section :balance
    :style :background-balance-passive
    :format :millions}
   {:edgarjure "Short-Term Borrowings"
    :label "Short-Term Borrowings"
    :section :balance
    :style :background-balance-passive
    :format :millions}
   {:our :liabilities-current
    :label "Current Liabilities"
    :section :balance
    :style :background-balance-passive
    :format :millions}
   {:our :debt-longterm
    :label "Long-Term Debt"
    :section :balance
    :style :background-balance-passive
    :format :millions}
   {:our :debt-shortterm
    :label "Short-Term Debt"
    :section :balance
    :style :background-balance-passive
    :format :millions}
   {:edgarjure "Total Liabilities"
    :label "Total Liabilities"
    :section :balance
    :style :background-balance-passive
    :format :millions}
   {:edgarjure "Stockholders Equity"
    :label "Stockholders Equity"
    :section :balance
    :style :background-balance-passive
    :format :millions}
   {:our :total-equity
    :label "Total Equity"
    :section :balance
    :style :background-balance-passive
    :format :millions}
   {:our :parent-equity
    :label "Parent Equity"
    :section :balance
    :style :background-balance-passive
    :format :millions}
   {:our :nci
    :label "Noncontrolling Interest"
    :section :balance
    :style :background-balance-passive
    :format :millions}
   {:our :preferred-stock
    :label "Preferred Stock"
    :section :balance
    :style :background-balance-passive
    :format :millions}
   {:edgarjure "Total Liabilities and Equity"
    :label "Total Liabilities and Equity"
    :section :balance
    :style :background-balance-passive
    :format :millions}
   {:our :debt
    :label "Debt"
    :section :balance
    :style :background-balance-passive
    :format :millions}
   {:our :capital-employed
    :label "Capital Employed"
    :section :balance
    :style :background-balance-active
    :format :millions}
   {:our :dividend-buyback
    :label "Dividends + Buyback"
    :section :balance
    :format :millions}
   ; ratios
   {:our :roce
    :label "ROCE"
    :section :ratio
    :format :percent}
   {:our :net-income-margin
    :label "Net Income Margin"
    :section :ratio
    :format :percent}
   {:our :roa
    :label "ROA"
    :section :ratio
    :format :percent}
   {:our :debt2ebitda
    :label "Debt/EBITDA"
    :section :ratio
    :format :dec2}
   {:our :dividend-coverage
    :label "Dividend Coverage"
    :section :ratio
    :format :dec2}
   {:our :buyback-coverage
    :label "Buyback Coverage"
    :section :ratio
    :format :dec2}
   {:our :dividend-buyback-coverage
    :label "Dividends + Buyback Coverage"
    :section :ratio
    :format :dec2}])
