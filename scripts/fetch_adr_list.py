#!/usr/bin/env python3
"""Fetch ADR lists from Citi, BNY, and JPM → data/adr-list.edn.

One row per :ticker. :source is a set of providers that listed it
(e.g. #{:jpm :bny}).

Ratio is ordinary shares per one DR (Citi ORD:DR as a/b; BNY/JPM DR:ORD as b/a).
"""
from __future__ import annotations

import json
import re
import sys
import time
import urllib.request
from collections import Counter, defaultdict
from concurrent.futures import ThreadPoolExecutor, as_completed
from datetime import date
from html import unescape
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
OUT_LISTED = ROOT / "data" / "adr-list.edn"
OUT_OTC = ROOT / "data" / "adr-list-otc.edn"

UA = {"User-Agent": "Mozilla/5.0 (compatible; edgar2-adr-list/1.0)"}

# Prefer these when picking company-name / resolving ratio ties.
SOURCE_PREF = ("jpm", "bny", "citi")


def is_otc_ticker(ticker: str) -> bool:
    """OTCBB / OTC ADR tickers conventionally end in Y."""
    return bool(ticker) and ticker.upper().endswith("Y")


def http_get(url: str, timeout: int = 120) -> bytes:
    req = urllib.request.Request(url, headers=UA)
    with urllib.request.urlopen(req, timeout=timeout) as resp:
        return resp.read()


def http_json(url: str):
    return json.loads(http_get(url).decode("utf-8"))


def parse_colon_ratio(s: str):
    m = re.match(r"^\s*(\d+(?:\.\d+)?)\s*:\s*(\d+(?:\.\d+)?)\s*$", str(s or ""))
    if not m:
        return None
    return float(m.group(1)), float(m.group(2))


def ord_per_dr_from_ord_dr(s: str):
    """Citi ORD:DR → ordinary shares per one DR."""
    ab = parse_colon_ratio(s)
    if not ab:
        return None
    a, b = ab
    if b == 0:
        return None
    return a / b


def ord_per_dr_from_dr_ord(s: str):
    """BNY/JPM DR:ORD → ordinary shares per one DR."""
    ab = parse_colon_ratio(s)
    if not ab:
        return None
    a, b = ab
    if a == 0:
        return None
    return b / a


def edn_num(x: float) -> str:
    if x is None:
        return "nil"
    if abs(x - round(x)) < 1e-9:
        return str(int(round(x)))
    return repr(float(f"{x:.10g}"))


def edn_str(s: str) -> str:
    return json.dumps(s, ensure_ascii=False)


def edn_source_set(sources: set[str]) -> str:
    ordered = [s for s in SOURCE_PREF if s in sources]
    ordered.extend(sorted(sources - set(SOURCE_PREF)))
    return "#{" + " ".join(":" + s for s in ordered) + "}"


def strip_html(s: str) -> str:
    s = re.sub(r"<[^>]+>", " ", s)
    s = unescape(s)
    s = s.replace("\xa0", " ")
    return re.sub(r"\s+", " ", s).strip()


def fetch_citi():
    url = (
        "https://depositaryreceipts.citi.com/adr/guides/"
        "unspresource.aspx?pageId=3&subpageId=173"
    )
    print(f"citi  ← {url}")
    html = http_get(url).decode("latin-1", "ignore")
    rows = []
    for tr in re.findall(r"<tr[\s>].*?</tr>", html, re.I | re.S):
        tds = re.findall(r"<td[^>]*>(.*?)</td>", tr, re.I | re.S)
        if len(tds) < 7:
            continue
        cells = [strip_html(td) for td in tds]
        ratio_i = next(
            (i for i, c in enumerate(cells) if parse_colon_ratio(c)), None
        )
        if ratio_i is None or ratio_i < 3:
            continue
        # cells: [cc, region, country, company, cusip, ticker, ratio, ord-ticker]
        ratio_s = cells[ratio_i]
        ticker = cells[ratio_i - 1]
        company = cells[ratio_i - 3]
        if not ticker or not re.match(r"^[A-Z0-9./-]{1,15}$", ticker):
            continue
        if company.lower() in {"company", "issuer"}:
            continue
        if re.fullmatch(r"[0-9A-Z]{9}", company):
            continue
        ratio = ord_per_dr_from_ord_dr(ratio_s)
        if ratio is None:
            continue
        rows.append(
            {
                "ticker": ticker,
                "company-name": company,
                "ratio": ratio,
                "source": "citi",
            }
        )
    print(f"citi  → {len(rows)} rows")
    return rows


def fetch_bny():
    url = (
        "https://www.adrbny.com/bin/adr/export/drDirectoryList"
        "?count=5000&start=0&drPage=directory&searchType=1"
    )
    print("bny   ← directory API")
    raw = http_get(url).decode("utf-8")
    d = json.loads(raw)
    if isinstance(d, str):
        d = json.loads(d)
    items = d["data"]["drdirectory"]
    rows = []
    for it in items:
        ticker = (it.get("drTicker") or "").strip()
        if not ticker:
            continue
        ratio = ord_per_dr_from_dr_ord(it.get("drRatio") or "")
        if ratio is None:
            continue
        name = (it.get("drTx") or "").lstrip("^").strip()
        rows.append(
            {
                "ticker": ticker,
                "company-name": name,
                "ratio": ratio,
                "source": "bny",
            }
        )
    print(f"bny   → {len(rows)} rows (API count={d.get('count')})")
    return rows


def jpm_program_ratio(cusip: str):
    try:
        xid_j = http_json(
            f"https://adr.api.wsod.com/jpmadr-public/v1/dr/xid/{cusip}"
        )
        xid = xid_j["data"]["xid"]
        det = http_json(
            f"https://adr.api.wsod.com/jpmadr-public/v1/dr/programDetails/{xid}"
        )["data"]
        ratio = ord_per_dr_from_dr_ord(det.get("ratio") or "")
        return {
            "ticker": (det.get("ticker") or "").strip(),
            "company-name": (det.get("issuer") or "").strip(),
            "ratio": ratio,
            "source": "jpm",
            "cusip": cusip,
        }
    except Exception as e:
        return {"cusip": cusip, "error": str(e)}


def fetch_jpm(workers: int = 24):
    url = (
        "https://adr.api.wsod.com/jpmadr-public/v1/drUniverse"
        "?offset=0&limit=5000&sortBy=name&sortOrder=asc"
    )
    print("jpm   ← universe + programDetails")
    items = http_json(url)["data"]["items"]
    cusips = [i["cusip"] for i in items if i.get("cusip")]
    by_cusip = {i["cusip"]: i for i in items if i.get("cusip")}
    print(f"jpm   fetching ratios for {len(cusips)} CUSIPs ({workers} workers)…")
    rows = []
    errors = 0
    done = 0
    t0 = time.time()
    with ThreadPoolExecutor(max_workers=workers) as ex:
        futs = {ex.submit(jpm_program_ratio, c): c for c in cusips}
        for fut in as_completed(futs):
            done += 1
            if done % 200 == 0 or done == len(cusips):
                print(
                    f"jpm   {done}/{len(cusips)} "
                    f"({time.time() - t0:.0f}s, errors={errors})"
                )
                sys.stdout.flush()
            r = fut.result()
            if r.get("error") or r.get("ratio") is None or not r.get("ticker"):
                errors += 1
                continue
            u = by_cusip.get(r["cusip"], {})
            if not r["company-name"]:
                r["company-name"] = (u.get("name") or "").strip()
            rows.append(
                {
                    "ticker": r["ticker"],
                    "company-name": r["company-name"],
                    "ratio": r["ratio"],
                    "source": "jpm",
                }
            )
    print(f"jpm   → {len(rows)} rows (errors/skipped={errors})")
    return rows


def _ratio_key(x: float) -> float:
    return round(float(x), 10)


def merge_by_ticker(provider_rows):
    """Collapse to one row per ticker; :source becomes a set of providers."""
    by_ticker = defaultdict(list)
    for r in provider_rows:
        by_ticker[r["ticker"]].append(r)

    merged = []
    ratio_conflicts = 0
    for ticker, xs in by_ticker.items():
        sources = {x["source"] for x in xs}
        # company-name: prefer SOURCE_PREF, then longest
        names = []
        for src in SOURCE_PREF:
            for x in xs:
                if x["source"] == src and x.get("company-name"):
                    names.append(x["company-name"])
                    break
        if not names:
            names = [x["company-name"] for x in xs if x.get("company-name")]
        company = max(names, key=len) if names else ""

        # ratio: majority vote, tie-break by SOURCE_PREF
        ratio_votes = Counter(_ratio_key(x["ratio"]) for x in xs)
        if len(ratio_votes) > 1:
            ratio_conflicts += 1
        best_count = max(ratio_votes.values())
        candidates = {k for k, v in ratio_votes.items() if v == best_count}
        ratio = None
        for src in SOURCE_PREF:
            for x in xs:
                if x["source"] == src and _ratio_key(x["ratio"]) in candidates:
                    ratio = x["ratio"]
                    break
            if ratio is not None:
                break
        if ratio is None:
            ratio = xs[0]["ratio"]

        merged.append(
            {
                "ticker": ticker,
                "company-name": company,
                "ratio": ratio,
                "source": sources,
            }
        )

    merged.sort(key=lambda r: r["ticker"])
    print(
        f"merge → {len(merged)} unique tickers "
        f"(ratio conflicts={ratio_conflicts})"
    )
    return merged


def write_edn(path: Path, rows, *, market: str):
    lines = [
        "{:download-date " + edn_str(str(date.today())) + ",",
        " :market " + edn_str(market) + ",",
        " :sources [:citi :bny :jpm],",
        ' :ratio-unit "ordinary-shares-per-DR",',
        " :rows",
        " [",
    ]
    for r in rows:
        lines.append(
            "  {:ticker "
            + edn_str(r["ticker"])
            + "\n   :company-name "
            + edn_str(r["company-name"])
            + "\n   :ratio "
            + edn_num(r["ratio"])
            + "\n   :source "
            + edn_source_set(r["source"])
            + "}"
        )
    lines.append(" ]}")
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text("\n".join(lines) + "\n")

    src_counts = Counter()
    for r in rows:
        for s in r["source"]:
            src_counts[s] += 1
    multi = sum(1 for r in rows if len(r["source"]) > 1)
    print(
        f"wrote {path}  unique-tickers={len(rows)}  "
        f"multi-source={multi}  provider-hits={dict(src_counts)}"
    )
    return src_counts


def split_and_write(rows):
    otc = [r for r in rows if is_otc_ticker(r["ticker"])]
    listed = [r for r in rows if not is_otc_ticker(r["ticker"])]
    write_edn(OUT_LISTED, listed, market="exchange-listed")
    write_edn(OUT_OTC, otc, market="otc")
    print(f"totals  exchange-listed={len(listed)}  otc={len(otc)}  all={len(rows)}")
    return listed, otc


def main():
    citi = fetch_citi()
    bny = fetch_bny()
    jpm = fetch_jpm()
    rows = merge_by_ticker(citi + bny + jpm)
    split_and_write(rows)


if __name__ == "__main__":
    main()
