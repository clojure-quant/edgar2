# Quarterly index and the daily feed

`clj -X:institutional` builds `data/13f-index-<year>.edn` from the quarterly master index, then reads each 13F-HR out of the daily `.nc.tar.gz` feed. A few filings print `missing` even though the submission is on disk and `edgar2.filing.complete` parses it. The lookup uses the filing date. Those submissions were disseminated in the next feed file.

This is not implemented. The notes below are enough to add the second pass later. Do not change `edgar2.filing.complete`.

## What `missing` means

`load-day` in `src/edgar2/institutional.clj` prints `missing` only when `day/read-accessions` has no body for that accession. A parser exception prints `skip`. `get-holdings` returning nil, because the submission has no information table, is silent. The three lines from the 2026 run are the first branch:

```
    missing 0001444864-26-000001
    missing 0000033250-26-000001
    missing 0001173460-26-000001
```

`load-holdings` groups `data/13f-index-<year>.edn` by `:filing-date` and opens `{dest}/{year}/QTR{n}/{yyyymmdd}.nc.tar.gz` for that date. `edgar2.filing.day/archive-file` is that path. Members are `./{accession}.nc`. `read-accessions` asks for those names and returns only the ones present.

The quarterly index date and the feed filename are different dates. The index date is Date Filed. The feed file is every public submission disseminated that day. For almost every 13F they are the same day. For the three below they are not, so the member is not in the tar the index points at.

## The quarterly index has no accepted date

`data/full-index/{year}/QTR{n}/master.zip` contains one entry, `master.idx`. The header calls it a "Master Index of EDGAR Dissemination Feed". Every data row is five fields:

```
CIK|Company Name|Form Type|Date Filed|Filename
```

2026 Q1 has 370,328 data rows. `edgar2.filing.quarterly-index/parse-line` keeps those five fields and nothing else. There is no accepted date and no dissemination date, so the feed file cannot be chosen from the index.

The other quarterly indexes SEC publishes (`company.idx`, `form.idx`, `crawler.idx`) use the same Date Filed column. They are not in this tree. Acceptance time is on the SEC filing-detail page and, once the `.nc` is found, in the submission header as `<DATE-OF-FILING-DATE-CHANGE>`. That tag equals the feed filename. `<FILING-DATE>` equals the index.

## The three filings

Each row is a real 13F-HR. The member name is the usual `./{accession}.nc`. It is absent from the filing-date tar and present in the next on-disk feed. Weekends and holidays have no feed file, so Friday's next file is Monday.

| Accession | Filer | CIK | Date filed | Accepted | In this feed | Not in |
| --- | --- | --- | --- | --- | --- | --- |
| `0000033250-26-000001` | EQUITABLE TRUST CO | 33250 | 2026-02-02 | 2026-02-03 08:19:07 | `20260203.nc.tar.gz` | `20260202.nc.tar.gz` |
| `0001173460-26-000001` | DAIDO LIFE INSURANCE CO | 1173460 | 2026-02-06 | 2026-02-09 07:47:11 | `20260209.nc.tar.gz` | `20260206.nc.tar.gz` |
| `0001444864-26-000001` | STRAIGHTLINE GROUP LLC | 1444864 | 2026-02-09 | 2026-02-10 07:35:56 | `20260210.nc.tar.gz` | `20260209.nc.tar.gz` |

Master index lines:

```
33250|EQUITABLE TRUST CO|13F-HR|2026-02-02|edgar/data/33250/0000033250-26-000001.txt
1173460|DAIDO LIFE INSURANCE CO|13F-HR|2026-02-06|edgar/data/1173460/0001173460-26-000001.txt
1444864|STRAIGHTLINE GROUP LLC|13F-HR|2026-02-09|edgar/data/1444864/0001444864-26-000001.txt
```

Equitable's submission header, which is the shape of all three:

```
<FILING-DATE>20260202
<DATE-OF-FILING-DATE-CHANGE>20260203
```

The line endings inside that header are bare CR. `edgar2.filing.complete` already stops a tag value at `<`, CR, or LF, so that does not matter.

`file-structure` and `get-holdings` were run on the Daido member (`20260209.nc.tar.gz`, 6,580 bytes). The structure is a normal 13F: cover `primary_doc.xml` of type `13F-HR`, information table `form13fInfoTable2512.xml` of type `INFORMATION TABLE`. `get-holdings` returns filer 1173460, filing date 2026-02-06, report period 2025-12-31, and 6 holdings. The other two have the same document shape (`primary_doc.xml` plus an `.xml` information table). The filenames are not `form13fInfoTable.xml`. `load-file` matches on extension and `<TYPE>`, so that is already handled.

Checked every 13F-HR in the index for 2026-01-30 through 2026-02-10 against the member names in those feed files: 2,186 filings. Every one except these three was in the archive named for its filing date. Each of the three was in the next on-disk feed and in no earlier feed. Monday 2026-02-02 was also compared with Friday 2026-01-30. Nothing filed on Monday lived in Friday's tar. Search forward only.

## Fix later

Leave the first pass as it is: one `read-accessions` per filing-date archive, in `load-holdings`. Stop printing `missing` in that pass. Return the index rows whose accession was not in the archive, separate from rows that parsed to nil or threw.

Then a second pass over those rows only.

Add the date walk to `src/edgar2/filing/day.clj`.

`later-filing-days` is pure. Arguments are a `YYYY-MM-DD` day, a count `n`, a calendar horizon, and an `exists?` predicate. Walk strictly forward, one calendar day at a time, at most `horizon` days. Collect days for which `exists?` is true, and stop at `n` of them. Days that do not exist still consume the horizon, so a weekend does not eat the count.

`archives-after` calls `later-filing-days` with `archive-file` as `exists?`, `n` of 3, and a horizon of 14. It returns the `File`s, oldest first. Three feeds covers the observed case (always the next file) plus a filing that stays unpublished across a holiday and one more suspend day. Fourteen calendar days is enough for a long weekend inside that window. Do not scan the rest of the year.

`archive-file`'s docstring says the file is the filing date. Change it. The file is the dissemination day, which usually matches the filing date and sometimes is the next feed.

In `load-holdings`, group the absent rows by each later archive from `archives-after` and call `read-accessions` once per archive with every accession that might be in it. Parse hits with the same `get-holdings` path as `load-day`, including the `skip` line on an exception. Repeat for the accessions still absent, against the next archive in that filing's list, up to the three. Print `missing` only for accessions that are still absent, or whose later feeds are not on disk.

Do not add the leftovers to the first pass's wanted set. `read-accessions` reads a gzip from the start and stops only when every requested name has been found. A name that is not in that tar forces a full scan, and the bodies would be read twice for every filing. The second pass re-reads a tar only when an earlier day left a leftover that can live there. In the February sample that is three extra reads of archives the first pass already opened for their own filing date.

`collapse-filer` sorts with `filing-order` before it reduces a CIK, so the recovered filings can be appended. Order out of `pmap` does not matter.

Days with no archive at all stay skipped. This pass does not download anything. It only looks in feed files already on disk. `track-filing!` can still count an absent row during the first pass so the progress total does not change. The second pass should not count those rows again.

## Test

In `test/edgar2/filing/day_test.clj`, test `later-filing-days` without the data disk.

- From `2026-02-02`, with `2026-02-02`, `2026-02-03`, and `2026-02-04` existing, the next day is `2026-02-03`. The start day itself is not included.
- From `2026-02-06`, with only `2026-02-06`, `2026-02-09`, and `2026-02-10` existing, the next two days are `2026-02-09` and `2026-02-10`.

No test of `edgar2.filing.complete` is required for this. The Daido submission already parses.
