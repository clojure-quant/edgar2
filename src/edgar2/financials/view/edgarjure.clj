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

(defn- with-style
  [field]
  (if (pl-green-labels (:label field))
    (assoc field :style :background-pl-green)
    field))

(defn- income-field
  [label]
  (with-style {:edgarjure label :label label :section :income}))

(defn edgarjure-income-industry
  "Income field maps for `industry` (:standard, :bank, :insurance, :reit).

  One `:edgarjure` line per label in `income-labels`, in display order."
  [industry]
  (mapv income-field (or (get income-labels industry) [])))
