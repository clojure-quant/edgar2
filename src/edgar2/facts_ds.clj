(ns edgar2.facts-ds
  (:require [clojure.string :as str]
            [edgar.api :as e]
            [tablecloth.api :as tc]
            [tech.v3.dataset :as ds])
  (:import [java.time LocalDate]
           [java.time.temporal ChronoUnit]))

(def period-cols
  [:accn :filed :form :fy :fp :start :end :frame])

(def annual-forms
  #{"10-K" "10-K/A" "20-F" "20-F/A" "40-F" "40-F/A"})

(def quarterly-forms
  #{"10-Q" "10-Q/A"})

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

(defn annual-periods
  "Reduce an `e/facts` dataset to one annual period row per yearly filing.

  Keeps 10-K / 20-F / 40-F (and amendments) via `annual-forms`.
  Keeps only ~12-month duration windows (`annual-duration?`, 300–400 days),
  then for each filing (`:filed` + `:form`) keeps the latest `:end` — the
  filing's own year, not comparative prior years also present in the report.

  Returns a dataset of `:accn :filed :form :fy :fp :start :end :frame`."
  [facts]
  (ds/->dataset (filing-periods facts annual-forms annual-duration?)
                {:dataset-name "annual-filing-periods"}))

(defn quarterly-periods
  "Reduce an `e/facts` dataset to one quarterly period row per filing.

  10-Qs supply Q1–Q3 (~3-month windows). Annual reports supply Q4: the
  quarterly footnote when present in companyfacts, otherwise synthesized from
  the annual period and the preceding 10-Q end (modern 10-Ks often omit the
  Q4 duration fact). Together that yields four quarterly periods per year.

  Returns a dataset of `:accn :filed :form :fy :fp :start :end :frame`."
  [facts]
  (let [from-10q (filing-periods facts quarterly-forms quarterly-duration?)
        from-10k (filing-periods facts annual-forms quarterly-duration?)
        annuals (filing-periods facts annual-forms annual-duration?)
        covered-ends (into #{} (map #(str (:end %)) from-10k))
        synthesized (->> annuals
                         (remove #(contains? covered-ends (str (:end %))))
                         (keep #(synthesize-q4 % from-10q)))
        rows (->> (concat from-10q from-10k synthesized)
                  (sort-by (juxt #(str (:filed %)) #(str (:form %)) #(str (:end %)))))]
    (ds/->dataset rows {:dataset-name "quarterly-filing-periods"})))

(defn- field-pair
  [[taxonomy concept]]
  [(name taxonomy) (name concept)])

(defn- blank-start?
  [row]
  (str/blank? (str (:start row))))

(defn- matching-period
  "Period for one fact. Duration facts need the same `:accn`, `:end`, and
  `:start`. Instant facts (blank `:start`) prefer the same `:accn`+`:end`,
  else the sole period on that accession (DEI share counts often use a
  cover-page date, not the fiscal period-end)."
  [fact periods-by-accn-end periods-by-accn]
  (if (blank-start? fact)
    (or (first (get periods-by-accn-end [(str (:accn fact)) (str (:end fact))]))
        (first (get periods-by-accn (str (:accn fact)))))
    (some #(when (= (str (:start fact)) (str (:start %))) %)
          (get periods-by-accn-end [(str (:accn fact)) (str (:end fact))]))))

(defn select-data
  "Facts for `fields` restricted to annual (`:y`) or quarterly (`:q`) periods.

  `fields` is a seq of `[taxonomy concept]` pairs, e.g.
  `[[:dei :EntityCommonStockSharesOutstanding]
    [:us-gaap :CostOfRevenue]]`.

  Returns a long-format dataset: period columns from `annual-periods` /
  `quarterly-periods`, plus `:taxonomy :concept :unit :val` (and `:label`
  when present). Instant facts (no `:start`) match on `:accn` (and `:end`
  when it aligns); duration facts also require `:start`."
  [dss q-or-y fields]
  (let [periods (case q-or-y
                  :y (annual-periods dss)
                  :q (quarterly-periods dss)
                  (throw (ex-info "q-or-y must be :q or :y" {:q-or-y q-or-y})))
        period-rows (ds/rows periods {:nil-missing? true})
        by-accn-end (group-by (juxt #(str (:accn %)) #(str (:end %))) period-rows)
        by-accn (group-by #(str (:accn %)) period-rows)
        wanted (set (map field-pair fields))
        out-cols (into period-cols [:taxonomy :concept :label :unit :val])
        rows (->> (ds/rows dss {:nil-missing? true})
                  (keep (fn [fact]
                          (when (contains? wanted
                                           [(str (:taxonomy fact))
                                            (str (:concept fact))])
                            (when-let [period (matching-period fact by-accn-end by-accn)]
                              (-> fact
                                  (merge (select-keys period period-cols))
                                  (select-keys out-cols))))))
                  (sort-by (juxt #(str (:end %))
                                 #(str (:taxonomy %))
                                 #(str (:concept %))
                                 #(str (:unit %)))))]
    (ds/->dataset rows {:dataset-name (str "select-data-" (name q-or-y))})))

(defn report-table
  "Wide report for `fields` over annual (`:y`) or quarterly (`:q`) periods:
  `select-data` → `e/pivot` → newest `:end` first."
  [dss q-or-y fields]
  (-> (select-data dss q-or-y fields)
      e/pivot
      (tc/order-by :end :desc)))
