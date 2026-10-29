(ns edgar2.financials.view.pl
  "P&L field specs for the :default financials view.

  Static display order. `:concept` and `:math` lines are data maps (same
  shape as `:edgarjure` rows), not built from edgarjure income labels.")

(def pl-fields
  [{:concept :revenue  ; CONCEPT
    :label "Revenue"
    :section :income}
   {:edgarjure "Cost of Revenue"
    :label "Cost of Revenue"
    :section :income}
   {:edgarjure "Gross Profit"
    :label "Gross Profit"
    :section :income
    :style :background-pl-green}
   {:edgarjure "SG&A Expense"
    :label "SG&A Expense"
    :section :income}
   {:edgarjure "Selling and Marketing Expense"
    :label "Selling and Marketing Expense"
    :section :income}
   {:edgarjure "General and Administrative Expense"
    :label "General and Administrative Expense"
    :section :income}
   {:edgarjure "R&D Expense"
    :label "R&D Expense"
    :section :income}
   {:concept :depreciation
    :label "Depreciation"
    :section :income}
   {:math :operating-expenses ; MATH
    :label "Operating Expenses"
    :section :income}
   {:math :operating-income ; MATH
    :label "Operating Income"
    :section :income
    :style :background-pl-green}
   {:edgarjure "Interest Expense"
    :label "Interest Expense"
    :section :income}
   {:edgarjure "Non-Operating Income"
    :label "Non-Operating Income"
    :section :income}
   {:edgarjure "Pre-Tax Income"
    :label "Pre-Tax Income"
    :section :income}
   {:edgarjure "Income Tax Expense"
    :label "Income Tax Expense"
    :section :income}
   {:edgarjure "Net Income"
    :label "Net Income"
    :section :income
    :style :background-pl-green}
   {:concept :shares-outstanding
    :label "Shares Outstanding"
    :section :income}
   {:math :dividends-paid ; MATH
    :label "Dividends Paid"
    :section :income
    :style :background-pl-green}
   {:math :stock-repurchase ; MATH
    :label "Stock Repurchase"
    :section :income
    :style :background-pl-green}])
