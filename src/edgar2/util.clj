(ns edgar2.util
  "SEC identity and ticker-to-CIK resolution shared by filings and demos."
  (:require [clojure.string :as str]
            [edgar.api :as e]))

(def identity-header
  (or (System/getenv "EDGAR_IDENTITY")
      "clojure-quant edgar2 research@clojure-quant.org"))

(def ^:private annual-form-names
  ["10-K" "20-F" "40-F"])

(def ^:private periodic-form-names
  #{"10-K" "10-Q" "20-F" "40-F" "10-K/A" "10-Q/A" "20-F/A" "40-F/A"})

(defn- has-annual-filing?
  [cik]
  (boolean (some #(seq (e/filings cik :form % :limit 1)) annual-form-names)))

(defn- accession-filer-cik
  "Leading CIK in an accession number (NNNNNNNNNN-YY-######)."
  [accession]
  (when-let [m (re-find #"^(\d+)-" (str accession))]
    (format "%010d" (Long/parseLong (second m)))))

(defn- operating-cik-from-submissions
  "When `mapped` is a shell/successor CIK with no annual report, find the
  operating filer from accession prefixes on periodic forms listed on its
  submissions (e.g. XOM → Holdings 0002115436 → EXXON MOBIL 0000034088)."
  [mapped]
  (->> (e/filings mapped :limit 100)
       (filter #(contains? periodic-form-names (:form %)))
       (keep #(accession-filer-cik (:accessionNumber %)))
       (remove #{mapped})
       distinct
       (some #(when (has-annual-filing? %) %))))

(defn resolve-filer
  "CIK to use for financials. Prefer the ticker's mapped CIK when it has a
  10-K/20-F/40-F; otherwise follow accession prefixes on its submissions to
  the operating registrant that actually files annual reports."
  [ticker]
  (let [mapped (e/cik ticker)]
    (cond
      (has-annual-filing? mapped) mapped
      :else (or (operating-cik-from-submissions mapped) mapped))))

;; SEC company_tickers maps XOM → ExxonMobil Holdings (0002115436), not CORP.
(def ^:private xom-operating-cik "0000034088")

(defn resolve-filer2
  "CIK to use for financials, with an explicit XOM override.

  Returns EXXON MOBIL CORP (`0000034088`) for XOM; every other ticker stays
  on `e/cik` — no annual-filing probe and no accession-prefix walk.

  Why the XOM hack is enough (and correct today)
  ----------------------------------------------
  `e/cik` / company_tickers.json binds each symbol to one registrant CIK.
  After Exxon’s holding-company reorganization, that file maps **XOM** to
  **ExxonMobil Holdings Corp** (`0002115436`): a new Exchange Act registrant
  that owns the operating company and carries the listed ticker, but does
  **not** file the historical 10-K series. Annual (and most XBRL) financials
  still live on **EXXON MOBIL CORP** (`0000034088`).

  So `(e/facts \"XOM\")` hits Holdings and yields empty or thin annual
  periods; `(e/facts \"0000034088\")` is the operating companyfacts the
  screens need.

  A universe scan of ~7.8k primary common tickers found that **only XOM**
  currently needs this remap: every other listed CIK either already has
  annual filings on the mapped registrant, or is a SPAC/fund/new listing
  with no operating successor that carries 10-Ks. Accession-prefix discovery
  (`operating-cik-from-submissions` / `resolve-filer`) generalizes the same
  Holdings→CORP link for XOM, but adds SEC round-trips for a case that is
  unique in the listing file. Hard-coding CORP for XOM is therefore a
  deliberate, documented exception — revisit if SEC retargets the ticker or
  another succession shows up the same way."
  [ticker]
  (if (= (str/upper-case (str ticker)) "XOM")
    xom-operating-cik
    (e/cik ticker)))

(defn ensure-identity!
  []
  (e/init! identity-header))
