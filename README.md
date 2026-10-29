# edgar2

Clojure project that pulls SEC EDGAR data via [edgarjure](https://github.com/clojure-finance/edgarjure) and runs a webserver that visualizes per ticker or screens on a universe.

Requires Clojure 1.12+ and Java 21+. SEC needs a User-Agent; override the default with `EDGAR_IDENTITY` if you want (e.g. `Your Name you@email.com`).

```bash
clj -X:web
clj -X:web :port 8080
```

Open a web UI. `/` is quintiles from `data/stats.edn` (run `clj -X:stats` first). `/screen` is the screen tabs, same tables as `clj -X:screen`. `/company/financials` is one ticker, same tables as `clj -X:financials`. The ticker stays with the browser tab across `/company/financials`, `/company/google-finance`, `/company/filings`, `/company/current-annual`, and `/company/description`. Financials defaults to `:n 5` (five fiscal years, or five quarters when the period is quarterly). The page also accepts 10, 15, 20, 25, 30, 35, 40, 45, or 50. No login.


## Entrypoints for cli tasks


```bash
clj -X:download  ; Download the last five XOM 10-Ks (default ticker and year count)
clj -X:report :ticker AAPL :years 3  ; Print Apple’s last three years of annual income-statement figures
clj -X:financials :ticker IMPP :n 20 ; Print P&L and main balance-sheet lines (last 20 fiscal years; 10-K, 20-F, or 40-F)
```



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

Download the nightly `companyfacts.zip` (progress printed), join listed `:ticker` / `:exchange` from `data/ticker-exchange.edn` by CIK, and write `data/universe-facts.edn` (unlisted CIKs omitted; ETF / ETF Trust / Trust ETF names with no revenue are omitted). Each revenue row keeps `:report-currency` (USD, JPY, COP, …), `:revenue-form` (10-K, 20-F, 40-F, 10-Q, …), and `:revenue-fp` (FY, Q1, …) from the same observation as `:revenue`.

Wrote data/universe-facts.edn  (6227 listed; dropped 3701 empty/ETF/filing-fee, 10467 unlisted; shares=6002  revenue=5528)
report-currency ([USD 5119] [CNY 132] [CAD 82] [EUR 54] [BRL 21] [JPY 19] [GBP 17] [MXN 12] [AUD 9] [KRW 8] [ARS 8] [CHF 8] [TWD 6] [ZAR 5] [ILS 5] [CLP 4] [HKD 3] [PEN 3] [SGD 2] [TRY 2] [COP 2] [INR 2] [MYR 1] [PHP 1] [KZT 1] [SEK 1] [DKK 1])
revenue-form ([10-Q 3947] [20-F 824] [10-K 376] [6-K 111] [40-F 105] [20-F/A 32] [10-Q/A 28] [S-1 18] [10-K/A 15] [S-1/A 13] [F-1/A 13] [POS AM 9] [6-K/A 9] [40-F/A 5] [S-4/A 5] [F-1 5] [S-4 4] [F-4/A 2] [8-K 2] [10-KT 2] [F-3 1] [S-11 1] [DEF 14A 1])


```bash
clj -X:stats
clj -X:stats :limit 25
```

From `universe-facts.edn` + `companyfacts.zip` + `prices.edn`, write `data/stats.edn`: 7-year mean YoY sales growth (%), profit margin (Net Income / revenue), return on assets (Net Income / assets), return on capital employed (EBIT / (assets − current liabilities)), `:shares` (most recent 10-Q when that quarter is at least as recent as the annual report; otherwise the annual report), `:price`, `:marketcap` (price × shares), `:price-sales` (USD market cap / USD revenue), `:price-earnings` (USD market cap / USD Net Income, else USD price / USD EPS), `:dividend-yield` (USD dividends paid / USD market cap, else USD dividend per share / price, %), and `:dividend-coverage` (net income / dividends paid, only when a dividend was paid).



`:ticker` and `:years` work on the download and report aliases. `:financials` takes `:n`: fiscal years when annual, quarters when `:period` is quarterly. `:years` is still accepted there as a synonym for `:n`.
