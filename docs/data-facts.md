# COMPANY FACTS

FACTS 1.3 GIG
- It is the full XBRL history for those companies, rebuilt nightly, not one quarter.
- for each filer there is one json file
- Under facts the numbers are grouped by standard taxonomy (us-gaap, ifrs-full, dei), then by tag. A tag such as revenue holds every whole-company observation the SEC kept: period start and end, value, unit (USD, JPY, shares), accession number, form, fiscal year, fiscal period, the date it was filed, and sometimes a calendar frame such as CY2016. Custom tags and segment breakdowns are left out. There is no entityType and no filing list.
- taxonomies: {'dei': 4, 'us-gaap': 6632, 'ffd': 8, 'ecd': 8, 'invest': 8, 'srt': 53, 'ifrs-full': 593, 'vip': 41, 'us-gaap-ebp': 7, 'cef': 23}

units ['AUD', 'AUD/shares', 'Account', 'Aircraft', 'Bank', 'BarrelofOilEquivalen', 'Boe', 'Bushels', 'Cases',  'Claims', 'Company', 'Contract', 'Countries', 'Country', 'Customer','Derivative', 'EUR', 'Employee', 'Employees', 'MBbls', 'MBoe', 'MMBTU', 'MMBbls', 'MMBoe', 'Mcf', 'Mcfe', 'Operating_Segments', 'Operations', 'Principal_Segment', 'Principal_segments', 'Product', 'Project', 'Rate', 'Ratio', 'Reportable_Segment', 'Reportable_segment', 'ReportingUnit', 'Reporting_Unit', 'Restaurants',  'SalesContract', 'Securities', 'Security', 'Secutrity', 'Segment', 'Segments', 'State', 'States', 'Store','barrel', ', 'numberOfSegment', 'numberOfSegments', 'numberofstores',  'operating_segments', 'reportableSegment', 'reportable_segment', 'reportable_segments', 'reportable_unit', 'reportingSegment',  'retail_store',  'Operating_Segments', 'Principal_Segment', 'Principal_segments',  'countries', ]

| JSON name | What it is | Presence |
|---|---|---|
| `start` | Period start | Only on duration facts. Instant facts omit it. |
| `end` | Period end | Always |
| `val` | Value | Always |
| `accn` | Accession number | Always |
| `fy` | Fiscal year | Key is always there. Value is sometimes `null`. |
| `fp` | Fiscal period (`FY`, `Q1`, `Q2`, `Q3`, `Q4`) | Key is always there. Value is sometimes `null`. |
| `form` | Form (`10-K`, `10-Q`, `20-F`, …) | Always |
| `filed` | Date filed | Always |
| `frame` | Calendar frame (`CY2016`, `CY2016Q3`, or `CY2016Q3I`) | Often omitted |


- some foreign issuer miss quarterly reports in 
  facts

- some filers report future periods which contains
  only tax rates or other data, we need to remove it. 


# FACTS-JSON
- for each [:taxonomy :concept] there is one series with 
  multiple fields
- different [:taxonomy :concept] cannot be compared, because
  the periods are not aligned. 

# FIN-RAW-DS
- edgarjure converts facts-json to a dataset
  that has concepts in columns and rows are periods.
- periods are raw, 
  - restatements are one row
  - contains annual/quarterly/half-annual/other
  - compareables (one filing reports prior periods)

