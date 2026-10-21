

ADR
- adr ratio can be parsed from annual reports
  but many times the ratio is not properly tagged
  we would need to wirte custom parser and 
  also search for filings that report ADR ratio 
  change this could be it
- using data from bny/jpy/citi works,
  but sometimes it has conflicts.
  bny has more accurate data that jpm, so 
  the conflict resolves first to bny.   

COMAPNY-FACTS
- some foreign issuer miss quarterly reports in 
  facts
- come filers report future periods which contains
  only tax rates or other data, we need to remove it.  
