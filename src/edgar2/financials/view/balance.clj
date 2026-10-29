(ns edgar2.financials.view.balance
  "Shared balance-sheet field specs for financials views.")

(def balance-fields
  [{:edgarjure "Cash and Equivalents"
    :label "Cash and Equivalents"
    :section :balance
    :style :background-balance-active
    :format :background-balance-active}
   {:edgarjure "Accounts Receivable"
    :label "Accounts Receivable"
    :section :balance
    :style :background-balance-active
    :format :background-balance-active}
   {:edgarjure "Inventory"
    :label "Inventory"
    :section :balance
    :style :background-balance-active
    :format :background-balance-active}
   {:edgarjure "Current Assets"
    :label "Current Assets"
    :section :balance
    :style :background-balance-active
    :format :background-balance-active}
   {:edgarjure "PP&E Net"
    :label "PP&E Net"
    :section :balance
    :style :background-balance-active
    :format :background-balance-active}
   {:edgarjure "Total Assets"
    :label "Total Assets"
    :section :balance
    :style :background-balance-active
    :format :background-balance-active}
   {:edgarjure "Accounts Payable"
    :label "Accounts Payable"
    :section :balance
    :style :background-balance-passive
    :format :background-balance-passive}
   {:edgarjure "Current Debt"
    :label "Current Debt"
    :section :balance
    :style :background-balance-passive
    :format :background-balance-passive}
   {:edgarjure "Current Portion of Long-Term Debt"
    :label "Current Portion of Long-Term Debt"
    :section :balance
    :style :background-balance-passive
    :format :background-balance-passive}
   {:edgarjure "Short-Term Borrowings"
    :label "Short-Term Borrowings"
    :section :balance
    :style :background-balance-passive
    :format :background-balance-passive}
   {:edgarjure "Current Liabilities"
    :label "Current Liabilities"
    :section :balance
    :style :background-balance-passive
    :format :background-balance-passive}
   {:edgarjure "Long-Term Debt"
    :label "Long-Term Debt"
    :section :balance
    :style :background-balance-passive
    :format :background-balance-passive}
   {:edgarjure "Total Liabilities"
    :label "Total Liabilities"
    :section :balance
    :style :background-balance-passive
    :format :background-balance-passive}
   {:edgarjure "Stockholders Equity"
    :label "Stockholders Equity"
    :section :balance
    :style :background-balance-passive
    :format :background-balance-passive}
   {:edgarjure "Total Equity"
    :label "Total Equity"
    :section :balance
    :style :background-balance-passive
    :format :background-balance-passive}
   {:edgarjure "Total Liabilities and Equity"
    :label "Total Liabilities and Equity"
    :section :balance
    :style :background-balance-passive
    :format :background-balance-passive}])
