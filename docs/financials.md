

20-F is the annual report for a foreign private issuer listed in the U.S. (usually via ADRs). It is the 10-K equivalent for non-U.S. companies: Toyota, Sony, Ecopetrol. They can use IFRS or home-country GAAP, and numbers are often in yen, won, pesos, not dollars. Interim updates are usually 6-K, not 10-Q.

40-F is the annual report for Canadian issuers under the Multijurisdictional Disclosure System (MJDS). The SEC lets them file their Canadian annual package (NI 51-102 AIF / audited statements) wrapped as a 40-F instead of rewriting it as a 10-K or 20-F. Agnico Eagle in your file is a 40-F. Interims are often 6-K as well.


A 6-K is how a foreign private issuer sends the SEC information it has already made public at home, filed with its home exchange, or sent to shareholders. Sony uses it that way. A US company uses an 8-K for current news and a 10-Q for the quarter. Sony has no 10-Q, so both jobs land on the 6-K.

Sony's recent 6-Ks show both uses:

Current news. The March 2026 6-Ks are the TCL partnership, the end of the Sony Honda electric-vehicle program, and the share-repurchase notice.
Financial results. The 6-K filed 2026-05-08 is the FY2025 earnings release for the year ended 2026-03-31. The annual report itself is still the 20-F, filed 2026-06-18.
In this project a 6-K counts as a quarterly report only when it carries the numbers (isXBRLNumeric, or inline XBRL on older filings). A press-release 6-K filed the same day stays a current report. Companyfacts often keeps the 20-F numbers and leaves the 6-K figures out, which is why Sony's May results are not in test/facts/SONY.json.

An 8-K is the current report for a US company. It tells the SEC about a specific event shortly after it happens, usually within four business days. Earnings releases, a signed merger agreement, a director resigning, a bankruptcy, and the financial statements attached to those announcements are typical 8-Ks.

The quarter still goes on the 10-K or 10-Q. The 8-K is the news, not the periodic report. A foreign issuer such as Sony uses a 6-K for that same current-news role.

In this project an 8-K stays a current report. If it also carries numbers for a year that has a 10-K, 20-F, or 40-F, the annual form keeps the period.



Enterprise value (shown as :ev, in millions of dollars) is:

market cap + interest-bearing debt + preferred stock + noncontrolling interest − cash

Debt is long-term debt including the current portion, plus commercial paper and other short-term borrowings when those are not already counted. EBIT is operating income. When that tag is missing, it is net income + interest + tax. EV/EBIT (:ev-ebit) is that enterprise value divided by EBIT, and only when the financials are in USD.


us-gaap:PaymentsForRepurchaseOfCommonStock
dei:EntityCommonStockSharesOutstanding or us-gaap:CommonStockSharesOutstanding
ifrs-full:NumberOfSharesOutstanding



operating expenses (selling, general and administrative, research and development, and usually depreciation) → operating income



588 listed companies report a revenue concept and a net-income concept, and report neither us-gaap:OperatingIncomeLoss nor ifrs-full:ProfitLossFromOperatingActivities. CLX is one of them: it has NetIncomeLoss and revenue tags, and no operating-income tag.

Of those 588, 480 have NetIncomeLoss and 108 have only IFRS ProfitLoss