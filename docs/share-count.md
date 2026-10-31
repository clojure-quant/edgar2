

issues with share count:

1. quarterly can be newer, 
   but sometimes it is off by 1000. 
   we dont use quarterly numbers if they differ more than 100
   from annual.

2. annual can be missing
   take quarterly
   see ADIG unit test

3. multiple share classes
  {:cik "0000317540",
   :ticker "COKE",
   :shares {"Common" {:weighted 73658000, :outstanding 56517334},
            "Class B" {:weighted 10047000, :outstanding 10046960}}}
   multiple common share classes result to no :dei shares in companyfacs.
   the only way to do it is download annual filings, parse xbrl and 
   extract from there. see demo/xbrl.clj that writes data/share-classes.edn


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
