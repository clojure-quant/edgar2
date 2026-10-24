

Portfolio and voting reports
N-MFP3 — Monthly portfolio report for a money market fund (Form N-MFP, current version). The July 6, August 6, and September 4 dates are the monthly cycle. It contains every holding (issuer, CUSIP, maturity, yield, amortized cost, market value), daily and weekly liquid assets, weighted average maturity and life, net asset value and shadow price, shareholder flows, and sponsor support.

N-PX — Annual proxy-voting record. It lists each portfolio security the fund voted on, the meeting date, the matter voted, how the fund voted, and whether that vote was for or against management.

NPORT-P — The public monthly portfolio report on Form N-PORT. Funds file it every month; only the report for the last month of each fiscal quarter is public, which is why several series file together in late August. It contains the full list of investments (name, identifier, balance, value, currency, asset category, payoff profile), derivatives and securities-lending detail, portfolio-level risk measures for debt holdings, monthly total returns, and shareholder flow.

NPORT-P/A — An amendment of an earlier NPORT-P. The /A is EDGAR’s amendment suffix, used the same way on other forms. The September 17 filings correct or replace the August NPORT-P reports. The content is the same portfolio report, with the corrected figures.

# 13F

A 13F is the quarterly holdings report that large institutional investment managers file under Section 13(f) of the Securities Exchange Act. The usual submission types are 13F-HR (the holdings report), 13F-HR/A (an amendment of one), and 13F-NT (a notice that another manager is reporting the holdings, so this filer has no table of its own).

A manager must file if it exercises investment discretion over at least $100 million of Section 13(f) securities. The report is due within 45 days after each calendar quarter-end, and it is a snapshot of the last day of that quarter.

What the information table lists
Each row is one position:

Issuer name, title of class, and CUSIP
Market value at quarter-end, in dollars
Number of shares, or principal amount for convertible debt, plus a flag for which of those it is
Put/Call, filled in only when the position is an option
Investment discretion: sole, shared, or other (and, if other, which managers share it)
Voting authority: how many shares the manager votes solely, shares, or does not vote
The cover page names the manager, the quarter, and any other managers included in or reporting for this filing.

What counts as a 13F security
The SEC publishes the official list each quarter. It is mostly US exchange-listed equities, plus ETFs and closed-end funds on that list, ADRs, certain convertible notes, and listed options and warrants on those stocks.

The report does not include short positions, cash, most bonds, mutual funds that are not on the list, or private holdings. A manager can also omit a position for a while under a confidential-treatment request, then add it later in an amendment.

The fund does not file those holdings again as a 13F. The adviser that has investment discretion over the fund includes the fund’s 13(f) securities in the adviser’s own 13F, pooled with every other account it manages.

What overlaps is only the listed-equity slice. An NPORT-P line for a common stock, an ETF on the SEC’s 13(f) list, an ADR, or a listed option can show up inside that adviser’s 13F-HR. Bonds, cash, loans, private holdings, and mutual funds that are not 13(f) securities stay on N-PORT only.

The two reports still do not line up row for row:

NPORT-P is one fund series. The 13F is the whole manager, so the same CUSIP is one combined share count across every fund and separate account the adviser runs.
NPORT-P’s public report is the fund’s fiscal quarter-end. The 13F is the calendar quarter-end, due 45 days later. The dates match only when the fund’s fiscal quarter ends on March 31, June 30, September 30, or December 31.
The fund files N-PORT even when the adviser is under the $100 million 13(f) threshold. Under that threshold there is no 13F at all.
A sub-adviser that has the discretion reports the position. The other manager files a 13F-NT instead of repeating the table, so the holding is not listed twice.
A 13F can omit a position under a confidential-treatment request while the same position is already public on NPORT-P.

## Duplicate holdings

The same shares can sit in two information tables. Form 13F tells the managers to prevent that. When two managers who each have to file both exercise investment discretion over the same securities, only one of them includes those securities. The filer who includes them lists the other manager on the summary page, List of Other Included Managers, and puts that manager’s sequence number in column 7 of the row. Discretion on that row is shared-defined (`DFND`) or shared-other (`OTR`). The other manager is supposed to leave the shares out of its own table and file a 13F notice, or a combination report that names the filer who reported them.

A duplicate is the case where that second manager also lists the CUSIP in its own information table for the same quarter. Two filers can also both report the same shares for one included manager. A row whose column 7 names several managers is one share count, shared with every name on the line.

A row with an empty column 7 stays on the filer who reported it. That includes every `SOLE` line. Two managers can both mark the same CUSIP `SOLE` and name nobody. Nothing on the form ties those rows together, so both counts remain.

Stage 1 writes `data/holdings-raw-<year>.nippy`, one submission per record, with column 7 still on each line. Lines are summed only when the CUSIP, the discretion, and the column-7 numbers all match. A sole line and a column-7 line of the same CUSIP stay separate.

Stage 2 reads that file and writes `data/holdings-<year>.nippy`. Within one quarter, a later restatement replaces that manager’s earlier book, and a `NEW HOLDINGS` amendment adds rows to it. Then each column-7 line is read as a claim for the included manager with the lowest sequence number. When several filings claim the same manager and CUSIP, the largest claim is kept and the others drop those rows. The kept claim is reduced by the shares that manager’s own filing already lists for the CUSIP. The named manager’s count is left as filed. When that manager has no such CUSIP, the claim stays, which is the case where the other manager left the shares out. The value removed follows the shares removed. The pruned book has one row per CUSIP.