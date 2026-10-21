(ns edgar2.financials.facts-ds
  (:require [clojure.string :as str]
            [edgar2.financials.concepts :as concepts]
            [tech.v3.dataset :as ds])
  (:import [java.time LocalDate]
           [java.time.temporal ChronoUnit]))
(def period-cols
  "Columns taken from companyfacts when building annual/quarterly periods."
  [:accn :filed :form :fy :fp :start :end :frame])

(def report-period-cols
  "Period columns kept on `select-data` / `report-table` rows."
  [:filed :period :end])

(def annual-forms
  #{"10-K" "10-K/A" "20-F" "20-F/A" "40-F" "40-F/A"})

(def quarterly-forms
  "10-Q for US issuers; 6-K for foreign private issuers' interim reports.
  Companyfacts often lacks 6-K XBRL (many FPIs only ship numeric facts on
  20-F), so quarterly coverage for those names may still be thin."
  #{"10-Q" "10-Q/A" "6-K" "6-K/A"})

(defn parse-date
  [d]
  (when d
    (try (LocalDate/parse (str d))
         (catch Exception _ nil))))

(defn period-days
  [start end]
  (when-let [s (parse-date start)]
    (when-let [e (parse-date end)]
      (.between ChronoUnit/DAYS s e))))

(defn annual-duration?
  "True for ~12-month periods. 10-Ks also repeat the last quarter
  (same :end, :fp FY) — those are ~90 days and must be dropped."
  [obs]
  (when-let [days (period-days (:start obs) (:end obs))]
    (<= 300 days 400)))

(defn quarterly-duration?
  "True for ~3-month periods. 10-Qs also carry year-to-date windows
  (6/9 months) — those must be dropped."
  [obs]
  (when-let [days (period-days (:start obs) (:end obs))]
    (<= 75 days 115)))

(defn- calendar-duration-frame
  "SEC-style duration frame for a period-end, e.g. 2025-09-27 → CY2025Q3."
  [end]
  (when-let [e (parse-date end)]
    (str "CY" (.getYear e)
         "Q" (inc (quot (dec (.getMonthValue e)) 3)))))

(defn- filing-periods
  "One period row per filing: forms in `forms`, duration via `duration?`,
  then latest `:end` per `:filed` + `:form`."
  [facts forms duration?]
  (let [rows (->> (ds/rows facts {:nil-missing? true})
                  (filter (fn [row]
                            (and (forms (str (:form row)))
                                 (duration? row))))
                  (map #(select-keys % period-cols)))]
    (->> rows
         (group-by (juxt #(str (:filed %)) #(str (:form %))))
         vals
         (map (fn [rs] (last (sort-by #(str (:end %)) rs))))
         (sort-by (juxt #(str (:filed %)) #(str (:form %))))
         vec)))

(defn- prior-quarter-end
  "Latest 10-Q period-end strictly before `annual-end` and on/after `annual-start`."
  [q-rows annual-start annual-end]
  (->> q-rows
       (map #(str (:end %)))
       (filter #(and (pos? (compare (str annual-end) %))
                     (not (pos? (compare (str annual-start) %)))))
       sort
       last))

(defn- period-label
  "Label `fy-fp` (e.g. 2025-Q2, 2025-FY)."
  [row]
  (str (:fy row) "-" (:fp row)))

(defn- with-period
  [row]
  (assoc row :period (period-label row)))

(defn- synthesize-q4
  "Q4 period for an annual filing when companyfacts omit the quarterly footnote.

  Start is the day after the matching Q3 10-Q end when available; otherwise
  ~91 days before the annual end."
  [annual-row q-rows]
  (when-let [end-d (parse-date (:end annual-row))]
    (let [q3-end (prior-quarter-end q-rows (:start annual-row) (:end annual-row))
          start-d (if-let [q3 (parse-date q3-end)]
                    (.plusDays q3 1)
                    (.minusDays end-d 91))]
      {:accn (:accn annual-row)
       :filed (:filed annual-row)
       :form (:form annual-row)
       :fy (:fy annual-row)
       :fp "Q4"
       :start (str start-d)
       :end (str end-d)
       :frame (calendar-duration-frame end-d)})))

(defn- period-ended-by-filing?
  "True when the period-end is on or before the filing date.

  Drops forward-looking FY windows (e.g. next-year tax rate) that share a
  filing with the completed annual and would otherwise win latest-`:start`."
  [row]
  (not (pos? (compare (str (:end row)) (str (:filed row))))))

(defn annual-periods
  "Reduce an `e/facts` dataset to one annual period row per fiscal year-end.

  Keeps only `:fp` FY facts with ~12-month windows (`annual-duration?`,
  300–400 days), on any form (10-K / 20-F / 40-F, and foreign 6-K annuals).
  Year-to-date or prior-year windows tagged Q2/Q3 on a 10-Q are excluded.
  Windows whose `:end` is after `:filed` are dropped (forward stubs).

  When one filing date carries several FY windows (current year plus
  comparatives), only the row with the latest `:start` is kept. Remaining
  rows are then uniqued by `:end` (latest `:filed` wins) so a restatement
  on a later 10-K replaces the earlier filing’s period.

  Returns a dataset of `:accn :filed :form :fy :fp :start :end :frame :period`
  (`:period` is `fy-fp`, e.g. 2025-FY)."
  [facts]
  (let [rows (->> (ds/rows facts {:nil-missing? true})
                  (filter #(and (= "FY" (str (:fp %)))
                                (annual-duration? %)
                                (period-ended-by-filing? %)))
                  (map #(select-keys % period-cols)))
        per-filing (->> rows
                        (group-by #(str (:filed %)))
                        vals
                        (map (fn [rs]
                               (last (sort-by #(str (:start %)) rs)))))
        best (->> per-filing
                  (group-by #(str (:end %)))
                  vals
                  (map (fn [rs] (last (sort-by #(str (:filed %)) rs))))
                  (sort-by #(str (:end %)))
                  (mapv with-period))]
    (when (empty? best)
      (throw (ex-info "no annual reports available" {})))
    (ds/->dataset best {:dataset-name "annual-filing-periods"})))

(defn- not-fy?
  "Quarterly tables never keep `:fp` FY (those are annual windows)."
  [row]
  (not= "FY" (str (:fp row))))

(defn quarterly-periods
  "Reduce an `e/facts` dataset to one quarterly period row per filing.

  10-Qs (and FPI 6-Ks when present in companyfacts) supply Q1–Q3 (~3-month
  windows). Annual reports supply Q4 when the 10-K/20-F carries an explicit
  quarterly footnote (`:fp` Q4), otherwise Q4 is synthesized from the annual
  period and the preceding 10-Q/6-K end. Rows tagged `:fp` FY are never kept
  — even ~90-day windows on a 10-K — so mid-year comparatives mislabeled FY
  do not appear as quarters.

  Returns a dataset of `:accn :filed :form :fy :fp :start :end :frame :period`
  (`:period` is `fy-fp`, e.g. 2025-Q2)."
  [facts]
  (let [from-10q (->> (filing-periods facts quarterly-forms quarterly-duration?)
                      (filterv not-fy?))
        from-10k (->> (filing-periods facts annual-forms quarterly-duration?)
                      (filterv not-fy?))
        annuals (filing-periods facts annual-forms annual-duration?)
        covered-ends (into #{} (map #(str (:end %)) from-10k))
        synthesized (->> annuals
                         (remove #(contains? covered-ends (str (:end %))))
                         (keep #(synthesize-q4 % from-10q)))
        rows (->> (concat from-10q from-10k synthesized)
                  (map with-period)
                  (sort-by (juxt #(str (:filed %)) #(str (:form %)) #(str (:end %))))
                  vec)]
    (when (empty? rows)
      (throw (ex-info "no quarterly reports available" {})))
    (ds/->dataset rows {:dataset-name "quarterly-filing-periods"})))

(defn- field-pair
  [[taxonomy concept]]
  [(name taxonomy) (name concept)])

(defn- blank-start?
  [row]
  (str/blank? (str (:start row))))

(defn- annual-stock-fact?
  "Instant facts for annual periods: FY / 10-K / 20-F / 40-F only.
  Later 10-Qs often restate year-end balances with rounded values."
  [fact]
  (or (= "FY" (str (:fp fact)))
      (annual-forms (str (:form fact)))))

(defn- matching-period
  "Period for one fact.

  Duration: same `:accn`+`:start`+`:end`, else the period with the same
  `:start`+`:end` (comparative year restated on a later 10-K).
  Instant (blank `:start`): annual filings only; `:accn`+`:end`, else same
  `:end`, else (cover-page DEI) the filing's period when `:end` matches none."
  [fact periods-by-accn-end periods-by-end periods-by-accn]
  (let [accn (str (:accn fact))
        end (str (:end fact))
        start (str (:start fact))]
    (if (blank-start? fact)
      (when (annual-stock-fact? fact)
        (or (first (get periods-by-accn-end [accn end]))
            (first (get periods-by-end end))
            (when-not (contains? periods-by-end end)
              (first (get periods-by-accn accn)))))
      (or (some #(when (= start (str (:start %))) %)
                (get periods-by-accn-end [accn end]))
          (when (annual-duration? fact)
            (some #(when (= start (str (:start %))) %)
                  (get periods-by-end end)))))))

(defn select-data
  "Facts for `fields` restricted to annual (`:y`) or quarterly (`:q`) periods.

  `fields` is a seq of `[taxonomy concept]` pairs, e.g.
  `[[:dei :EntityCommonStockSharesOutstanding]
    [:us-gaap :CostOfRevenue]]`.

  Returns a long-format dataset with period columns `:filed :period :end`,
  plus `:taxonomy :concept :unit :val` (and `:label` when present). Instant
  facts (no `:start`) match on `:accn` (and `:end` when it aligns); duration
  facts also require `:start`."
  [dss q-or-y fields]
  (let [periods (case q-or-y
                  :y (annual-periods dss)
                  :q (quarterly-periods dss)
                  (throw (ex-info "q-or-y must be :q or :y" {:q-or-y q-or-y})))
        period-rows (ds/rows periods {:nil-missing? true})
        by-accn-end (group-by (juxt #(str (:accn %)) #(str (:end %))) period-rows)
        by-end (group-by #(str (:end %)) period-rows)
        by-accn (group-by #(str (:accn %)) period-rows)
        wanted (set (map field-pair fields))
        out-cols (into report-period-cols [:taxonomy :concept :label :unit :val])
        rows (->> (ds/rows dss {:nil-missing? true})
                  (keep (fn [fact]
                          (when (contains? wanted
                                           [(str (:taxonomy fact))
                                            (str (:concept fact))])
                            (when-let [period (matching-period fact by-accn-end by-end by-accn)]
                              (-> fact
                                  (merge (select-keys period report-period-cols))
                                  ;; keep the fact's own :filed for restatement preference
                                  (assoc :filed (:filed fact))
                                  (select-keys out-cols))))))
                  (sort-by (juxt #(str (:end %))
                                 #(str (:taxonomy %))
                                 #(str (:concept %))
                                 #(str (:unit %)))))]
    (ds/->dataset rows {:dataset-name (str "select-data-" (name q-or-y))})))

(defn- field-rank
  [fields]
  (into {} (map-indexed (fn [i pair] [(field-pair pair) i]) fields)))

(defn- better-fact?
  "Prefer a later `:filed`, then earlier `fields` order."
  [a b ranks]
  (let [filed (compare (str (:filed a)) (str (:filed b)))]
    (cond
      (not= 0 filed) (pos? filed)
      :else (< (get ranks [(str (:taxonomy a)) (str (:concept a))] 9999)
               (get ranks [(str (:taxonomy b)) (str (:concept b))] 9999)))))

(defn- pivot-keeping-periods
  "Wide table: `:filed :period :end` plus one column per concept name.
  One value per [end, concept]; later `:filed` / `fields` order break ties."
  [long-ds fields]
  (let [ranks (field-rank fields)
        rows (ds/rows long-ds {:nil-missing? true})
        chosen (reduce (fn [acc r]
                         (let [k [(str (:end r)) (str (:concept r))]
                               cur (get acc k)]
                           (if (or (nil? cur) (better-fact? r cur ranks))
                             (assoc acc k r)
                             acc)))
                       {}
                       rows)
        wide (->> chosen
                  vals
                  (group-by #(str (:end %)))
                  (map (fn [[_ facts]]
                         (let [period (select-keys (first facts) report-period-cols)
                               vals (into {}
                                          (map (fn [f] [(str (:concept f)) (:val f)])
                                               facts))]
                           (merge period vals))))
                  (sort-by #(str (:end %)) #(compare %2 %1))
                  vec)]
    (ds/->dataset wide {:dataset-name "report-table"})))

(defn report-table
  "Wide report for `fields` over annual (`:y`) or quarterly (`:q`) periods.

  `select-data`, then pivot to one row per period (newest `:end` first) with
  period columns `:filed :period :end` and one column per concept."
  [dss q-or-y fields]
  (pivot-keeping-periods (select-data dss q-or-y fields) fields))

(defn- as-number
  [v]
  (when (number? v)
    (if (and (float? v)
             (== (double v) (Math/rint (double v)))
             (<= (Math/abs (double v)) (double Long/MAX_VALUE)))
      (long v)
      v)))

(defn calc-concept
  "Per-row value for `concept-id` from a wide report-table dataset.

  Looks up the concept's priority `[taxonomy tag]` list, then for each row
  walks that list and returns the first numeric column named by the tag's
  local name (same mixed-tag rule as `concepts/series-mixed`, but as a
  column aligned with `ds` — no filtering, no reordering). Missing concept
  or missing columns yield nil for that row.

  Returns a vector of values, one per row."
  [ds concept-id]
  (let [tag-pairs (or (:data (concepts/get-concept-id concept-id)) [])
        col-names (mapv (fn [[_tag concept]] (name concept)) tag-pairs)]
    (mapv (fn [row]
            (some (fn [col]
                    (as-number (get row col)))
                  col-names))
          (ds/rows ds {:nil-missing? true}))))
