

issues with share count:

1. quarterly can be newer, 
   but sometimes it is off by 1000. 
   we dont use quarterly numbers if they differ more than 100
   from annual.

2. annual can be missing
   take quarterly
   see ADIG unit test

3. multiple share classes - parse 40F filings directly.
  {:cik "0000317540",
   :ticker "COKE",
   :shares {"Common" {:weighted 73658000, :outstanding 56517334},
            "Class B" {:weighted 10047000, :outstanding 10046960}}}
   multiple common share classes result to no :dei shares in companyfacts.
   the only way to do it is download annual filings, parse xbrl and 
   extract from there. see edgar2/download/xbrl_shares.clj that writes data/share-classes.edn
   
 4. 40F filing wiht multiple documents  
   also, some 40F filings contain a separate html file that contains the
   xbrl statemetns, so they need to be used. 

5. some filers have different entities, 
   for example WMB has 3 different filers.
   We take only the main entity from the dei filing.

6. some filings dont have xbrl inline tags.
   ZTEKF	40-F	no XBRL at all (0 facts, all 9 docs untagged); counts only in prose: 107,792,002 / weighted 105,919,553	genuine data gap, not a parser bug

7. some filers do not have shares, only bonds:
   NRUC: truly zero — a tax-exempt cooperative that "does not issue capital stock", and the 
   word no itself is inside the dei:EntityCommonStockSharesOutstanding tag with ixt:fixed-zero. 
   The ticker is a bond (NRUC 26), not equity.


FMCC  freddi Mac
- 10 classes of preferrence shares.
- dei-common and gaap-common:  650 million common stock outstanding.
- but gaap-weigthed and  gaap-diluted 
  is 3.5 billion.
- Shares the warrant would issue about 2.5 billion
- Note 11 (Common Stock Warrant and Earnings Per Share) says the warrant is treated as in-substance common stock because the exercise price is nominal. Those shares are therefore included in the basic weighted-average share count, not only in diluted shares.


AIBT
now:	567,695,492	
prior:  850,165,334
and I cannto find the 850 number.


GP
companyfacts misses the last 2 filings.
clj -X:demo-table-full :ticker GP :show shares :ixbrl true

HALF YEAR REPORTS ???



TODO ALTX - display financial numbers with 2 digits (if below 10)
and 1 digit if below 100.

