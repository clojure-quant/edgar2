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

`:ticker` and `:years` work on both aliases. `:n` is accepted as a synonym for `:years`.
