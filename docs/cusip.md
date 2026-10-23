# When a CUSIP changes

A normal forward split does not change the CUSIP. A reverse split does. A forward split changes it only when the company makes shareholders exchange the old shares for new ones.

That is the current rule from CUSIP Global Services (CGS), the organization that assigns CUSIPs. The same rule is in the [CUSIP Permanence FAQ](https://www.cusip.com/pdf/news/CUSIP-Permanence-FAQ.pdf) and on the [CGS application page](https://www.cusip.com/apply/index).

A US ISIN is `US` plus the CUSIP plus a check digit, so a CUSIP change changes the ISIN. The ticker often does not.

## Stock splits

A forward split done in book-entry form, with no exchange of certificates, keeps the CUSIP. A forward split that requires a mandatory exchange of shares gets a new CUSIP, and the old one is suspended. A reverse split always gets a new CUSIP. CGS suspends the old number when it receives notice of shareholder approval.

Two SEC filings show the difference:

NVIDIA, 10-for-1, effective June 7, 2024. The Delaware amendment subdivided each share into ten. Existing certificates stayed valid, and holders did not have to turn them in. The CUSIP stayed 67066G104 (ISIN US67066G1040). The 8-K does not announce a new CUSIP: [nvda-20240607](https://www.sec.gov/Archives/edgar/data/1045810/000104581024000144/nvda-20240607.htm). NVIDIA’s investor FAQ still lists that same number.

Lam Research, 10-for-1, effective October 2, 2024. Same economic split, but certificated holders had to surrender old certificates. Lam’s Form 8937 calls it a mandatory exchange. The CUSIP changed from 512807108 to 512807306. The ticker stayed LRCX. Filings: [8-K of September 25, 2024](https://www.sec.gov/Archives/edgar/data/707549/000070754924000117/lrcx-20240925.htm) and [8-K of October 2, 2024](https://www.sec.gov/Archives/edgar/data/707549/000070754924000123/lrcx-20241002.htm). Nasdaq’s corporate-action alert said the CUSIP change was because surrender of certificates was required.

Reverse splits on EDGAR follow the same pattern. Spire Global’s 1-for-8 reverse split (August 31, 2023) began trading under a new CUSIP, 848560306 ([8-K exhibit](https://www.sec.gov/Archives/edgar/data/1816017/000095017023045434/spir-ex99_1.htm)).

## Other actions that change the CUSIP of the existing equity

CGS still assigns a new CUSIP for these, including after the October 1, 2021 rule change:

A merger or reorganization that creates a new legal entity. Shareholders receive stock of a different issuer. A merger in which the survivor is the same legal entity and shares are not exchanged does not, by itself, require a new CUSIP.

A Chapter 11 reorganization. Emergence usually means new common stock, and often new warrants, in the reorganized company, each with a new CUSIP. The pre-bankruptcy CUSIP is retired.

A redomicile, when the company becomes a new legal entity and shares are exchanged. CGS then assigns a new CINS and ISIN. A change of state of incorporation with no new entity and no share exchange is a different case.

A reclassification into a different share class (ordinary shares become Class A, or one class is exchanged for another). The new class is a new security. A pure subdivision of the same class, as in the NVIDIA split, is not.

## Actions that do not change it

A name change, since October 1, 2021 (“CUSIP Permanence”). Before that date, an equity name change that moved the issuer in alphabetical order usually got a new CUSIP. Debt CUSIPs generally stayed put even then. Old name-change CUSIPs were not rewritten. CGS still wants the certificate of amendment so it can update the name on the existing record. [DTCC notice 16309-21](https://www.dtcc.com/-/media/Files/pdf/2022/1/3/16309-21.pdf) aligned DTC’s name-change processing with that rule effective February 1, 2022.

A ticker change, an exchange transfer, a cash dividend, an ordinary stock dividend, an authorized-share increase, and a par-value change by itself. A par-value change often shows up in the same Nasdaq alert as a reverse split; the reverse split is what changes the CUSIP.

## New securities, separate from the common stock

These get their own CUSIP. The common stock’s CUSIP changes only if one of the events above also applies:

- A spin-off (the spun-off company)
- A rights offering (the rights)
- A new preferred, warrant, unit, or share class
- For bonds: a partial pre-refunding (two new CUSIPs, underlying suspended) and secondary insurance of part of an issue (new CUSIP on the insured piece only)

## 13F

A 13F reports holdings by CUSIP. A reverse split, a forward split that the issuer or Nasdaq describes as a mandatory exchange, a merger into a new legal entity, or a bankruptcy emergence will make the next quarter’s 13F show a different CUSIP for the same issuer. A book-entry forward split and a post-2021 name change leave the CUSIP in place, so holdings can be joined across quarters on that number.
