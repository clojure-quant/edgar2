(ns edgar2.financials.view.edgarjure
  "edgarjure income-line field specs by industry.

  Each field is `{:edgarjure label :label label :section :income}` with
  `:style` when the label is a highlighted P&L total.")

(def income-labels
  "edgarjure income line labels, by industry."
  {:standard ["Revenue"
              "Cost of Revenue"
              "Gross Profit"
              "Operating Expenses"
              "Total Costs and Expenses"
              "R&D Expense"
              "SG&A Expense"
              "Selling and Marketing Expense"
              "General and Administrative Expense"
              "Marketing Expense"
              "Other Operating Expense"
              "Excise Taxes"
              "Restructuring Charges"
              "Impairment Charges"
              "Goodwill Impairment"
              "Acquisition-Related Costs"
              "Litigation Settlement"
              "Gain (Loss) on Sale of Assets"
              "In-Process R&D"
              "Operating Income"
              "Non-Operating Income"
              "Interest Expense"
              "Pre-Tax Income"
              "Income Tax Expense"
              "Net Income"
              "EPS Basic"
              "EPS Diluted"
              "Shares Basic"
              "Shares Diluted"]
   :bank ["Interest Income"
          "Interest Expense"
          "Net Interest Income"
          "Provision for Credit Losses"
          "Net Interest Income After Provision"
          "Noninterest Income"
          "Noninterest Expense"
          "Total Revenue"
          "Total Net Revenue"
          "Compensation Expense"
          "Occupancy Expense"
          "Pre-Tax Income"
          "Income Tax Expense"
          "Net Income"
          "EPS Basic"
          "EPS Diluted"
          "Shares Basic"
          "Shares Diluted"]
   :insurance ["Premiums Earned"
               "Net Investment Income"
               "Realized Investment Gains"
               "Total Revenue"
               "Policyholder Benefits and Claims"
               "Interest Credited to Policyholders"
               "Policyholder Dividends"
               "Acquisition Cost Amortization"
               "Total Benefits and Expenses"
               "Pre-Tax Income"
               "Income Tax Expense"
               "Net Income"
               "EPS Basic"
               "EPS Diluted"
               "Shares Basic"
               "Shares Diluted"]
   :reit ["Revenue"
          "Rental Revenue"
          "Real Estate Revenue"
          "Tenant Reimbursements"
          "Other Property Revenue"
          "Property Operating Expense"
          "Real Estate Taxes"
          "Ground Rent Expense"
          "General and Administrative Expense"
          "SG&A Expense"
          "D&A"
          "Impairment of Real Estate"
          "Total Costs and Expenses"
          "Gain on Sale of Real Estate"
          "Operating Income"
          "Non-Operating Income"
          "Interest Expense"
          "Pre-Tax Income"
          "Income Tax Expense"
          "Net Income"
          "EPS Basic"
          "EPS Diluted"
          "Shares Basic"
          "Shares Diluted"]})

(def ^:private pl-green-labels
  #{"Gross Profit" "Operating Income" "Net Income"
    "Net Interest Income" "Net Interest Income After Provision"
    "Premiums Earned"})

(def ^:private per-share-labels
  "Per-share lines stay in raw dollars; everything else is scaled."
  #{"EPS Basic" "EPS Diluted"})

(defn- with-style
  [field]
  (if (pl-green-labels (:label field))
    (assoc field :style :background-pl-green)
    field))

(defn- with-format
  [field]
  (if (per-share-labels (:label field))
    field
    (assoc field :format :millions)))

(defn- income-field
  [label]
  (-> {:edgarjure label :label label :section :income}
      with-style
      with-format))

(defn edgarjure-income-industry
  "Income field maps for `industry` (:standard, :bank, :insurance, :reit).

  One `:edgarjure` line per label in `income-labels`, in display order."
  [industry]
  (mapv income-field (or (get income-labels industry) [])))

(def ^:private balance-fields
  "edgarjure balance-sheet rows, scaled to millions. `:style` only; no
  `:format :background-*` — `:format` is a value format."
  [; ASSETS / ACTIVE
   {:concept :cash
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
   {:concept :assets-current
    :label "Current Assets"
    :section :balance
    :style :background-balance-active
    :format :millions}
   {:edgarjure "PP&E Net"
    :label "PP&E Net"
    :section :balance
    :style :background-balance-active
    :format :millions}
   {:edgarjure "Total Assets"
    :label "Total Assets"
    :section :balance
    :style :background-balance-active
    :format :millions}
   ; LIABILITIES / PASSIVE
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
   {:concept :liabilities-current
    :label "Current Liabilities"
    :section :balance
    :style :background-balance-passive
    :format :millions}
   {:edgarjure "Long-Term Debt"
    :label "Long-Term Debt"
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
   {:edgarjure "Total Equity"
    :label "Total Equity"
    :section :balance
    :style :background-balance-passive
    :format :millions}
   {:edgarjure "Total Liabilities and Equity"
    :label "Total Liabilities and Equity"
    :section :balance
    :style :background-balance-passive
    :format :millions}])

(defn edgarjure-views
  "All edgarjure field rows for `industry`: income labels + balance sheet."
  [industry]
  (vec (concat (edgarjure-income-industry industry) balance-fields)))
