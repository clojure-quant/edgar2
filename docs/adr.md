

Annual Reports publish adr ratios, but
for many adrs the ratios are not tagged as a full text,
so we would need to manually scan that.


The depositary banks publish the ratios.


Rebuild the merged bank listings with:

```
clj -X:adr-list
```

That writes `data/adr-list.edn` (exchange-listed) and `data/adr-list-otc.edn`
from Citi + BNY + JPM. One row per ticker; `:source` is the set of providers
that listed it. `:ratio` is ordinary shares per one DR.

When providers disagree on a ticker’s ratio, the row keeps the majority-vote
`:ratio` (ties: bny → jpm → citi) and adds
`:conflict {:bny … :jpm … :citi …}` with every provider’s value. The run also
prints an ALERT. BNY is preferred over JPM because JPM often lags ADS ratio
changes. Example — ORIX (`IX`):

```
{:ticker "IX"
 :company-name "ORIX CORP"
 :ratio 1
 :source #{:bny :jpm}
 :conflict {:bny 1 :jpm 5}}
```


Citi’s unsponsored DR directory is a table with ticker, CUSIP, and ratio:
depositaryreceipts.citi.com. The column is ORD:DR, so 4:1 means four ordinary
shares per one receipt. Examples on that page: ALSYY 4:1, CHEOY 1:2, BCHEY 20:1.
That page is unsponsored programs only, so sponsored names such as BABA and TSM
are not on it.

BNY’s directory is at https://www.adrbny.com/directory/dr-directory.html
(API: `/bin/adr/export/drDirectoryList`). It covers programs at BNY, Citi,
J.P. Morgan, and Deutsche Bank. BNY writes the ratio the other way, DR:ORD.
A BNY line of 1 : 2 is the same thing as Citi’s 2:1.

BNY quirks (not ratio-parse bugs):
- `--` / `NA` are placeholder tickers for programs with no public symbol
  (hundreds of 144A / Reg S rows). Those rows are skipped.
- The same real ticker can name two different issuers (e.g. `TOUR` =
  Hanatour Reg S at 5:1 → 0.2 ord/DR, and Tuniu at 1:30 → 30). We keep the
  exchange-listed Level I/II/III row and drop the Reg S / 144A collision.
- A `:conflict` vector like `{:bny [300 1]}` was never “ADR:stock as a pair”
  — it was two issuers wrongly merged on ticker `NA` before placeholders
  were filtered.

J.P. Morgan keeps the same kind of directory at adr.com (API: adr.api.wsod.com).
Sponsored programs are looked up one CUSIP at a time for `programDetails`.

There is no single free file of every sponsored ADR ratio; this merge is the
bulk substitute.
