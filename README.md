# edgar2

Clojure CLI project that pulls SEC EDGAR annuals via [edgarjure](https://github.com/clojure-finance/edgarjure) and prints revenue, gross profit, and net profit.

Requires Clojure 1.12+ and Java 21+. SEC needs a User-Agent; override the default with `EDGAR_IDENTITY` if you want (e.g. `Your Name you@email.com`).

## Examples

Download the last five XOM 10-Ks (default ticker and year count):

```bash
clj -X:download
```

Print Apple’s last three years of annual income-statement figures:

```bash
clj -X:report :ticker AAPL :years 3
```

Print every income-statement line for Microsoft (last 20 fiscal years):

```bash
clj -X:pl :ticker MSFT :years 20
```

Print last year’s income-statement lines with XBRL tags:

```bash
clj -X:pl-fields :ticker OXY
```

Print the latest 10-K Item 1 business description:

```bash
clj -X:business :ticker OXY
```

Download the SEC listed-ticker directory (one main ticker per CIK) and write `data/tickers.edn`:

```bash
clj -X:download-tickers
```

Build a fast all-filer overview (`data/filer-info.edn`) from a few SEC **bulk** files (latest FSDS quarters + the exchange list) — not one HTTP call per company. Full statements and 10-K text stay on the per-ticker aliases.

```bash
clj -X:filer-info
clj -X:filer-info :limit 10
```

Download the nightly `companyfacts.zip` (progress printed) and write a compact `data/facts.edn` (ETF / ETF Trust / Trust ETF names with no revenue are omitted). Each revenue row keeps `:revenue-unit` (USD, JPY, COP, …), `:revenue-form` (10-K, 20-F, 40-F, 10-Q, …), and `:revenue-fp` (FY, Q1, …) from the same observation as `:revenue`.

```bash
clj -X:facts-all
clj -X:facts-all :force true
```

`:ticker` and `:years` work on all aliases. `:n` is accepted as a synonym for `:years`.


20-F is the annual report for a foreign private issuer listed in the U.S. (usually via ADRs). It is the 10-K equivalent for non-U.S. companies: Toyota, Sony, Ecopetrol. They can use IFRS or home-country GAAP, and numbers are often in yen, won, pesos, not dollars. Interim updates are usually 6-K, not 10-Q.

40-F is the annual report for Canadian issuers under the Multijurisdictional Disclosure System (MJDS). The SEC lets them file their Canadian annual package (NI 51-102 AIF / audited statements) wrapped as a 40-F instead of rewriting it as a 10-K or 20-F. Agnico Eagle in your file is a 40-F. Interims are often 6-K as well.