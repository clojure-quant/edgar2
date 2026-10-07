# edgar2

Clojure CLI project that pulls SEC EDGAR annuals via [edgarjure](https://github.com/clojure-finance/edgarjure) and prints revenue, gross profit, and net profit.

Requires Clojure 1.12+ and Java 21+. SEC needs a User-Agent; override the default with `EDGAR_IDENTITY` if you want (e.g. `Your Name you@email.com`).

## Examples


```bash
clj -X:download  ; Download the last five XOM 10-Ks (default ticker and year count)
clj -X:report :ticker AAPL :years 3  ; Print Apple’s last three years of annual income-statement figures
clj -X:financials :ticker IMPP :n 20 ; Print P&L and main balance-sheet lines (last 20 fiscal years; 10-K, 20-F, or 40-F)
clj -X:pl-fields :ticker OXY     ; Print last year’s income-statement lines with XBRL tags
clj -X:business :ticker OXY    ; Print the latest 10-K Item 1 business description
```



```bash
clj -X:download-tickers
```
Download the SEC listed-ticker directory (one main ticker per CIK) and write `data/tickers.edn`:

```bash
clj -X:download-exchanges
```
Download listing venue per ticker (`data/ticker-exchange.edn`, one main ticker per CIK; hyphenated class/preferred/units and symbols with no exchange omitted):


```bash
clj -X:download-universe-fsds
clj -X:download-universe-fsds :limit 10
```

Build a fast all-filer overview (`data/universe-fsds.edn`) from a few SEC **bulk** files (latest FSDS quarters + the exchange list) — not one HTTP call per company. Each row includes `:adr-ratio` from the latest 20-F when that filing tagged `EntityListingDepositoryReceiptRatio` (ordinary shares per ADR). Full statements and 10-K text stay on the per-ticker aliases.


```bash
clj -X:universe-facts
clj -X:universe-facts :force true
```

Download the nightly `companyfacts.zip` (progress printed), join listed `:ticker` / `:exchange` from `data/ticker-exchange.edn` by CIK, and write `data/universe-facts.edn` (unlisted CIKs omitted; ETF / ETF Trust / Trust ETF names with no revenue are omitted). Each revenue row keeps `:revenue-unit` (USD, JPY, COP, …), `:revenue-form` (10-K, 20-F, 40-F, 10-Q, …), and `:revenue-fp` (FY, Q1, …) from the same observation as `:revenue`.

Wrote data/universe-facts.edn  (6227 listed; dropped 3701 empty/ETF/filing-fee, 10467 unlisted; shares=6002  revenue=5528)
revenue-unit ([USD 5119] [CNY 132] [CAD 82] [EUR 54] [BRL 21] [JPY 19] [GBP 17] [MXN 12] [AUD 9] [KRW 8] [ARS 8] [CHF 8] [TWD 6] [ZAR 5] [ILS 5] [CLP 4] [HKD 3] [PEN 3] [SGD 2] [TRY 2] [COP 2] [INR 2] [MYR 1] [PHP 1] [KZT 1] [SEK 1] [DKK 1])
revenue-form ([10-Q 3947] [20-F 824] [10-K 376] [6-K 111] [40-F 105] [20-F/A 32] [10-Q/A 28] [S-1 18] [10-K/A 15] [S-1/A 13] [F-1/A 13] [POS AM 9] [6-K/A 9] [40-F/A 5] [S-4/A 5] [F-1 5] [S-4 4] [F-4/A 2] [8-K 2] [10-KT 2] [F-3 1] [S-11 1] [DEF 14A 1])


```bash
clj -X:stats
clj -X:stats :limit 25
```

From `universe-facts.edn` + `companyfacts.zip` + `prices.edn`, write `data/stats.edn`: 7-year mean YoY sales growth (%), profit margin (Net Income / revenue), return on capital (Net Income / assets), `:shares`, `:price`, `:marketcap` (price × shares), `:price-sales` (USD market cap / USD revenue), `:price-earnings` (USD price / USD EPS, else USD market cap / USD Net Income), and `:dividend-yield` (USD dividend per share / price, %).

```bash
clj -X:web
clj -X:web :port 8080
```

Open a web UI with the screen tabs (from `data/stats.edn`; run `clj -X:stats` first) and a financials page for one ticker. Same tables as `clj -X:screen` and `clj -X:financials`. Financials defaults to `:n 5` (five fiscal years, or five quarters when the period is quarterly). The page also accepts 10, 15, 20, 25, 30, 35, 40, 45, or 50. No login.

`:ticker` and `:years` work on the download and report aliases. `:financials` takes `:n`: fiscal years when annual, quarters when `:period` is quarterly. `:years` is still accepted there as a synonym for `:n`.


20-F is the annual report for a foreign private issuer listed in the U.S. (usually via ADRs). It is the 10-K equivalent for non-U.S. companies: Toyota, Sony, Ecopetrol. They can use IFRS or home-country GAAP, and numbers are often in yen, won, pesos, not dollars. Interim updates are usually 6-K, not 10-Q.

40-F is the annual report for Canadian issuers under the Multijurisdictional Disclosure System (MJDS). The SEC lets them file their Canadian annual package (NI 51-102 AIF / audited statements) wrapped as a 40-F instead of rewriting it as a 10-K or 20-F. Agnico Eagle in your file is a 40-F. Interims are often 6-K as well.

Enterprise value (shown as :ev, in millions of dollars) is:

market cap + interest-bearing debt + preferred stock + noncontrolling interest − cash

Debt is long-term debt including the current portion, plus commercial paper and other short-term borrowings when those are not already counted. EBIT is operating income. When that tag is missing, it is net income + interest + tax. EV/EBIT (:ev-ebit) is that enterprise value divided by EBIT, and only when the financials are in USD.


us-gaap:PaymentsForRepurchaseOfCommonStock
dei:EntityCommonStockSharesOutstanding or us-gaap:CommonStockSharesOutstanding
ifrs-full:NumberOfSharesOutstanding


FSDS
- 1 zip per quarter for all companies
- sub.txt submission summary
- pre.txt presentation rows. accounting standard / fields
- tag.txt tag dictionary (all reported fields)


operating expenses (selling, general and administrative, research and development, and usually depreciation) → operating income