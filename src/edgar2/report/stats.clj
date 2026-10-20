(ns edgar2.report.stats
  "Universe fundamentals from companyfacts.zip: sales growth, margin, ROC."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [edgar.api :as e]
            [edgar2.financials.concepts :as concepts]
            [edgar2.financials.derived :as derived]
            [edgar2.filing :as filing]
            [edgar2.download.dataset :as dataset]
            [edgar2.download.fsds-universe :as fsds]
            [edgar2.adr :as adr]
            [edgar2.download.facts :as dl]
            [edgar2.download.fx :as fx]
            [edgar2.download.price :as price]
            [edgar2.download.facts-universe :as universe]
            [edgar2.financials.calc.concept :as calc-concept]
            [edgar2.financials.calc.quarterly :as calc-q]
            [edgar2.financials.facts-ds :as facts-ds]
            [edgar.xbrl :as xbrl]
            [jsonista.core :as json]
            [tech.v3.dataset :as ds])
  (:import [java.util.zip ZipFile]))

(def stats-path "data/stats.edn")

(defn- concept-data
  [id]
  (:data (concepts/get-concept-id id)))

(def net-income-tags (concept-data :net-income))
(def eps-tags (concept-data :eps))
(def dps-cash-tags (concept-data :dps-cash))
(def dps-other-tags (concept-data :dps-other))
(def dividends-paid-tags (concept-data :dividends-paid))
(def stock-repurchase-tags (concept-data :stock-repurchase))
(def asset-tags (concept-data :assets))
(def total-equity-tags (concept-data :total-equity))
(def parent-equity-tags (concept-data :parent-equity))
(def nci-tags (concept-data :nci))
(def ebit-tags (concept-data :operating-income))
(def interest-expense-tags (concept-data :interest-expense))
(def income-tax-tags (concept-data :income-tax))
(def cash-tag-pairs (concept-data :cash))
(def preferred-tags (concept-data :preferred-stock))
(def debt-tags (concept-data :debt))
(def revenue-tags (concept-data :revenue))

(def sales-growth-window
  "Number of trailing YoY revenue changes averaged into :sales-growth-yoy."
  7)

(def profit-margin-window
  "Number of trailing annual profit margins averaged into :avg-profit-margin."
  10)

(def stats-fields
  "All concept pairs pulled into the annual report-table."
  (vec (distinct
        (concat revenue-tags
                net-income-tags
                eps-tags
                dps-cash-tags
                dps-other-tags
                dividends-paid-tags
                stock-repurchase-tags
                asset-tags
                total-equity-tags
                parent-equity-tags
                nci-tags
                ebit-tags
                interest-expense-tags
                income-tax-tags
                cash-tag-pairs
                preferred-tags
                debt-tags
                (mapcat second derived/statement-lines)))))

(defn yoy-pct
  [prev cur]
  (when (and (number? prev) (number? cur) (not (zero? (double prev))))
    (* 100.0 (/ (- (double cur) (double prev)) (double prev)))))

(defn avg
  [xs]
  (when (seq xs)
    (/ (double (reduce + xs)) (count xs))))

(defn direction-score
  "Share of years that did not fall versus the prior year, from 0 to 100.

  `vals` is oldest-first numbers. Each value after the first scores 1 when
  value − prior >= 0, otherwise 0. The score is 100 times that count divided
  by the number of year-to-year steps. The first year has no prior, so it is
  omitted."
  [vals]
  (let [steps (keep (fn [[prev cur]]
                      (when (and (number? prev) (number? cur))
                        (if (>= (double cur) (double prev)) 1 0)))
                    (map vector vals (rest vals)))]
    (when (seq steps)
      (* 100.0 (/ (double (reduce + steps)) (count steps))))))

(defn avg-sales-growth
  "Mean of the last `n` year-over-year revenue changes (%).

  `vals` is oldest-first revenue numbers."
  [vals n]
  (let [growths (vec (keep identity (map yoy-pct vals (rest vals))))
        window (take-last n growths)]
    (when (seq window)
      {:sales-growth-yoy (avg window)
       :sales-growth-years (count window)})))

(defn latest-num
  "Newest numeric value and its end from parallel newest-first vectors."
  [ends vals]
  (some (fn [[e v]] (when (number? v) {:end e :val v}))
        (map vector ends vals)))

(defn chron-nums
  "Oldest-first numbers from a newest-first `calc-concept` vector."
  [vals]
  (->> vals reverse (filterv number?)))

(defn table-rows
  [table]
  (vec (ds/rows table {:nil-missing? true})))

(defn- as-number
  "Keep integral doubles as longs so stats match JSON integer facts."
  [v]
  (when (number? v)
    (if (and (float? v)
             (== (double v) (Math/rint (double v)))
             (<= (Math/abs (double v)) (double Long/MAX_VALUE)))
      (long v)
      v)))

(defn concept-val
  [row concept]
  (as-number (get row (name concept))))

(defn first-concept-val
  [row tag-pairs]
  (some (fn [[_ c]] (concept-val row c)) tag-pairs))

(defn ratio
  [num den]
  (when (and (number? num) (number? den) (not (zero? (double den))))
    (/ (double num) (double den))))

(defn latest-ratio
  "Newest num/den from parallel newest-first vectors (row-aligned).

  Skips periods where either side is missing or the denominator is zero."
  ([nums dens]
   (some (fn [[n d]]
           (when-let [r (ratio n d)]
             {:num n :den d :ratio r}))
         (map vector nums dens)))
  ([ends nums dens]
   (some (fn [[e n d]]
           (when-let [r (ratio n d)]
             {:end e :num n :den d :ratio r}))
         (map vector ends nums dens))))

(defn yearly-ratios
  "num/den per row-aligned period, oldest first.

  `nums` and `dens` are newest-first value vectors from the same dataset.
  Periods where either side is missing, or the denominator is zero, are
  omitted."
  [nums dens]
  (->> (map ratio (reverse nums) (reverse dens))
       (filterv some?)))

(defn avg-profit-margin
  "Mean of the last `n` annual profit margins (net income / revenue).

  `ni` and `rev` are newest-first value vectors."
  [ni rev n]
  (let [window (take-last n (yearly-ratios ni rev))]
    (when (seq window)
      {:avg-profit-margin (avg window)
       :avg-profit-margin-years (count window)})))

(defn usd-unit?
  "True for USD or USD/shares (US listing price is in dollars)."
  [u]
  (let [s (str/upper-case (str u))]
    (or (= s "USD")
        (str/starts-with? s "USD/"))))

(defn later-end
  "Later of two period-end strings."
  [a b]
  (->> [a b] (remove str/blank?) sort last))

(defn- row-at-end
  [rows end]
  (when-not (str/blank? (str end))
    (some #(when (= (str end) (str (:end %))) %) rows)))

(defn- whole-dollar?
  "True when v is a positive whole number. A split ($.98 …) tag lands here as 98."
  [v]
  (and (number? v)
       (>= (double v) 1.0)
       (< (Math/abs (- (double v) (Math/rint (double v)))) 1e-6)))

(defn- primary-html
  "Primary filing document, cached under data/ix-html."
  [cik accn]
  (let [f (io/file "data/ix-html" (str accn ".htm"))]
    (or (when (.exists f) (slurp f))
        (when-let [html (e/html {:cik (universe/pad-cik cik)
                                 :accessionNumber (str accn)})]
          (.mkdirs (.getParentFile f))
          (spit f html)
          html))))

(defn- absorb-currency-decimal
  "Read ($.98 per share) as 0.98 when the tagged fact is the whole number 98."
  [cik {:keys [val end accn taxonomy tag] :as fact}]
  (if-not (and cik accn taxonomy tag (whole-dollar? val))
    fact
    (try
      (if-let [html (primary-html cik accn)]
        (if-let [v (filing/leading-currency-decimal
                    html
                    (str (name taxonomy) ":" (name tag))
                    end
                    val)]
          (assoc fact :val v)
          fact)
        fact)
      (catch Throwable _
        fact))))

(defn- col-vec
  "Values of column `k` on `ds` as a vector (empty when missing)."
  [dss k]
  (if (ds/has-column? dss k)
    (vec (ds/column dss k))
    []))

(defn annual-dps
  "Per-share dividend at period-end `end`. Cash paid wins. Missing → 0.

  A filing that shows ($.98 per share) with only 98 inside the tag is
  read as 0.98."
  [rows end cik]
  (let [row (row-at-end rows end)
        pair (or (some (fn [p] (when (number? (concept-val row (second p))) p))
                       dps-cash-tags)
                 (some (fn [p] (when (number? (concept-val row (second p))) p))
                       dps-other-tags))
        fact (if pair
               {:val (concept-val row (second pair))
                :end (str end)
                :accn (:accn row)
                :taxonomy (first pair)
                :tag (second pair)}
               {:val 0.0 :end (str end)})]
    (absorb-currency-decimal cik fact)))

(defn company-stats
  "Fundamentals from an `e/facts`-shaped dataset via `report-table`.

  Reporting currency is `:revenue-unit` on `row` (from revenue). First table
  row is the current year; trailing rows feed growth and average margins."
  [dss {:keys [revenue-unit] :as row}]
  (let [table (facts-ds/report-table dss :y stats-fields revenue-unit)
        rows (table-rows table)
        ends (mapv #(str (:end %)) rows)
        current (first rows)
        annual-count (ds/row-count (facts-ds/annual-periods dss))
        concepts (calc-concept/calc-concepts table)
        rev (col-vec concepts :revenue)
        ni (col-vec concepts :net-income)
        assets (col-vec concepts :assets)
        oi (col-vec concepts :operating-income)
        equity (col-vec concepts :total-equity)
        debt-v (col-vec concepts :debt)
        eps-v (col-vec concepts :eps)
        paid-v (col-vec concepts :dividends-paid)
        repurchase-v (col-vec concepts :stock-repurchase)
        ebit-v (col-vec concepts :ebit)
        bs-end (:end (latest-num ends assets))
        last-end (later-end (:end (latest-num ends rev)) bs-end)
        eps (latest-num ends eps-v)
        paid (:val (latest-num ends paid-v))
        repurchase (:val (latest-num ends repurchase-v))
        ebit (latest-num ends ebit-v)
        dps (annual-dps rows last-end (:cik row))
        growth (avg-sales-growth (chron-nums rev) sales-growth-window)
        ni-qoy (calc-q/net-income-change-q-on-y
                (facts-ds/report-table dss :q net-income-tags))
        margins (avg-profit-margin ni rev profit-margin-window)
        rev-score (direction-score (chron-nums rev))
        oi-score (direction-score (chron-nums oi))
        margin (latest-ratio ends ni rev)
        roa (latest-ratio ends ni assets)
        roc (latest-ratio ends ni equity)
        bs-row (or (row-at-end rows bs-end) current)
        debt (let [i (or (some identity
                               (map-indexed (fn [i e] (when (= e bs-end) i)) ends))
                         0)]
               (nth debt-v i nil))
        cash (first-concept-val bs-row cash-tag-pairs)
        preferred (first-concept-val bs-row preferred-tags)
        nci (or (concept-val bs-row :MinorityInterest)
                (concept-val bs-row :NoncontrollingInterests))
        coverage-end (or (:end margin) last-end)
        coverage-row (or (row-at-end rows coverage-end) current)
        coverage-ni (if margin
                      (:num margin)
                      (first-concept-val coverage-row net-income-tags))
        coverage (when paid (ratio coverage-ni paid))]
    (cond-> (assoc (select-keys row [:ticker :exchange :cik :entityName :revenue-unit])
                   :annual-count annual-count)
      (:sales-growth-yoy growth)
      (assoc :sales-growth-yoy (:sales-growth-yoy growth)
             :sales-growth-years (:sales-growth-years growth))
      (number? ni-qoy) (assoc :net-income-change-q-on-y ni-qoy)
      (number? rev-score) (assoc :revenue-score rev-score)
      (number? oi-score) (assoc :operating-income-score oi-score)
      margin
      (assoc :profit-margin (:ratio margin)
             :net-profit (:num margin)
             :revenue (:den margin))
      (:avg-profit-margin margins)
      (assoc :avg-profit-margin (:avg-profit-margin margins)
             :avg-profit-margin-years (:avg-profit-margin-years margins))
      roa
      (assoc :return-on-assets (:ratio roa)
             :assets (:den roa))
      roc
      (assoc :return-on-capital (:ratio roc)
             :equity (:den roc)
             :roc-end (:end roc))
      (and eps (number? (:val eps)))
      (assoc :eps (:val eps))
      (and dps (number? (:val dps)))
      (assoc :dps (:val dps))
      (number? paid) (assoc :dividends-paid paid)
      (number? repurchase) (assoc :stock-repurchase repurchase)
      (number? debt) (assoc :debt debt :debt-end bs-end)
      (number? cash) (assoc :cash cash)
      (number? preferred) (assoc :preferred-stock preferred)
      (number? nci) (assoc :noncontrolling-interest nci)
      (and ebit (number? (:val ebit)))
      (assoc :ebit (:val ebit) :ebit-end (:end ebit))
      (number? coverage) (assoc :dividend-coverage coverage))))

(defn load-price-by-ticker
  "ticker → close from data/prices.edn."
  []
  (into {}
        (map (fn [{:keys [ticker close]}]
               [(str/upper-case (str ticker)) (double close)])
             (price/load-prices))))

(defn market-cap-adjusted
  "USD `:marketcap` into the reporting currency, divided by ordinary-shares-per-ADR.

  `marketcap` is ADR (or share) price × shares outstanding. When `:adr-ratio`
  is ordinary shares per one ADR, divide so the cap matches whole-company
  financials. Non-USD units use the latest `from-usd` rate. Nil when market
  cap is missing or FX is unknown for a foreign unit."
  ([row]
   (market-cap-adjusted (:marketcap row) (:revenue-unit row) (:adr-ratio row)))
  ([marketcap revenue-unit adr-ratio]
   (when (number? marketcap)
     (let [usd? (usd-unit? revenue-unit)
           rate (if usd? 1.0 (fx/from-usd revenue-unit))
           ratio (let [r (when (number? adr-ratio) (double adr-ratio))]
                   (if (and r (pos? r)) r 1.0))]
       (when rate
         (/ (* (double marketcap) (double rate)) ratio))))))

(defn needs-mcap-adjust?
  "True when valuation must use `:market-cap-adjusted` (foreign report or
  ADR ratio other than 1)."
  [revenue-unit adr-ratio]
  (or (not (usd-unit? revenue-unit))
      (and (number? adr-ratio)
           (not= 1.0 (double adr-ratio)))))

(defn with-valuation
  "Add :shares, :price, :marketcap, :market-cap-adjusted, :price-sales,
  :price-earnings, :enterprise-value, and :ev-ebit.

  When the report is not USD or `:adr-ratio` is not 1, price-sales, PE,
  dividend-yield, buyback-yield, EV, and EV/EBIT use `:market-cap-adjusted`
  (reporting-currency cap after ADR) against same-currency fundamentals.
  Otherwise PE is market cap / Net Income, else USD price / USD EPS;
  dividend yield is dividends paid / market cap, else USD DPS / price."
  [st price-by-ticker shares]
  (let [price (get price-by-ticker (str/upper-case (str (:ticker st))))
        mcap (when (and price (number? shares))
               (* (double price) (double shares)))
        ratio (:adr-ratio st)
        mcap-adj (market-cap-adjusted mcap (:revenue-unit st) ratio)
        need-adj? (needs-mcap-adjust? (:revenue-unit st) ratio)
        vmcap (if need-adj? mcap-adj mcap)
        rev (when (number? (:revenue st)) (:revenue st))
        ni (when (number? (:net-profit st)) (:net-profit st))
        eps (when (number? (:eps st)) (:eps st))
        dps (when (number? (:dps st)) (:dps st))
        report-usd? (usd-unit? (:revenue-unit st))
        paid (when (number? (:dividends-paid st)) (:dividends-paid st))
        repurchase (when (number? (:stock-repurchase st)) (:stock-repurchase st))
        pe (cond
             (and vmcap (number? ni) (not (zero? (double ni))))
             (/ (double vmcap) (double ni))
             (and (not need-adj?) report-usd? price (number? eps)
                  (not (zero? (double eps))))
             (/ (double price) (double eps)))
        dy (cond
             (and vmcap (pos? (double vmcap)) (number? paid) (pos? (double paid)))
             (* 100.0 (/ (double paid) (double vmcap)))
             (and (not need-adj?) report-usd? price (pos? (double price)) (number? dps))
             (* 100.0 (/ (double dps) (double price))))
        by (when (and vmcap (pos? (double vmcap))
                      (number? repurchase) (pos? (double repurchase)))
             (* 100.0 (/ (double repurchase) (double vmcap))))
        ebit (when (number? (:ebit st)) (:ebit st))
        ev (when vmcap
             (+ (double vmcap)
                (double (or (:debt st) 0))
                (double (or (:preferred-stock st) 0))
                (double (or (:noncontrolling-interest st) 0))
                (- (double (or (:cash st) 0)))))]
    (cond-> (dissoc st :price-sales :price-earnings :dividend-yield
                    :buyback-yield :enterprise-value :ev-ebit
                    :market-cap-adjusted)
      (number? shares) (assoc :shares shares)
      price (assoc :price price)
      mcap (assoc :marketcap mcap)
      mcap-adj (assoc :market-cap-adjusted mcap-adj)
      (and vmcap rev (not (zero? (double rev))))
      (assoc :price-sales (/ (double vmcap) (double rev)))
      pe (assoc :price-earnings pe)
      dy (assoc :dividend-yield dy)
      by (assoc :buyback-yield by)
      ev (assoc :enterprise-value ev)
      (and ev (number? ebit) (not (zero? (double ebit))))
      (assoc :ev-ebit (/ ev (double ebit))))))

(defn load-universe
  []
  (when-not (.exists (io/file universe/universe-path))
    (throw (ex-info "Missing universe file; run clj -X:universe-facts first"
                    {:path universe/universe-path})))
  (dataset/read-rows universe/universe-path))

(defn read-cik-json
  [zf cik]
  (when cik
    (let [entry (.getEntry zf (str "CIK" cik ".json"))]
      (when entry
        (with-open [in (.getInputStream zf entry)]
          (json/read-value in json/keyword-keys-object-mapper))))))

(defn load-sic-index
  "Lookups from data/universe-fsds.edn: :by-cik and :by-ticker.
  Each value carries :sic, :sector, :industry, and :subindustry.
  Empty when that file is missing."
  []
  (let [f (io/file fsds/filer-info-path)]
    (if-not (.exists f)
      (do (println (format "  no %s; sic left blank" fsds/filer-info-path))
          {:by-cik {} :by-ticker {}})
      (reduce (fn [idx row]
                (let [text (fn [k]
                             (some-> (get row k) str str/trim not-empty))
                      info (cond-> {}
                             (text :sic) (assoc :sic (text :sic))
                             (text :sector) (assoc :sector (text :sector))
                             (text :industry) (assoc :industry (text :industry))
                             (text :subindustry) (assoc :subindustry (text :subindustry))
                             (text :sic-description) (assoc :sic-description (text :sic-description)))
                      cik (text :cik)
                      ticker (some-> (text :ticker) str/upper-case)]
                  (cond-> idx
                    (and cik (seq info)) (update :by-cik assoc cik info)
                    (and ticker (seq info)) (update :by-ticker assoc ticker info))))
              {:by-cik {} :by-ticker {}}
              (dataset/read-rows f)))))

(defn with-sic
  "Copy SIC names from universe-fsds onto a stats row.
  CIK wins; ticker is the fallback."
  [st {:keys [by-cik by-ticker]}]
  (let [info (or (get by-cik (some-> (:cik st) str str/trim))
                 (get by-ticker (some-> (:ticker st) str str/trim str/upper-case)))]
    (cond-> st
      (:sic info) (assoc :sic (:sic info))
      (:sector info) (assoc :sector (:sector info))
      (:industry info) (assoc :industry (:industry info))
      (:subindustry info) (assoc :subindustry (:subindustry info))
      (:sic-description info) (assoc :sic-description (:sic-description info)))))

(def stats-field-order
  [:ticker :exchange :cik :entityName :sic :sector :industry :subindustry :price
   :shares :adr-ratio :marketcap :market-cap-adjusted :annual-count
   :revenue :revenue-unit
   :net-profit
   :eps
   :assets
   :return-on-assets
   :equity
   :return-on-capital
   :sales-growth-yoy
   :net-income-change-q-on-y
   :revenue-score
   :operating-income-score
   :price-earnings
   :dividends-paid
   :dividend-yield
   :dividend-coverage
   :stock-repurchase
   :buyback-yield
   :enterprise-value
   :ev-ebit
   :ebit
   :debt
   :cash])

(defn select-fields
  "Map with `ks` first (present keys only); remaining keys last, sorted."
  [m ks]
  (let [seen (set ks)
        front (mapcat (fn [k] (when (contains? m k) [k (get m k)])) ks)
        back (->> (keys m)
                  (remove seen)
                  sort
                  (mapcat (fn [k] [k (get m k)])))]
    (apply array-map (concat front back))))

(defn save-stats!
  [rows]
  (.mkdirs (io/file "data"))
  (let [tmp (str stats-path ".tmp")
        ordered (mapv #(select-fields % stats-field-order) rows)]
    (spit tmp (with-out-str (pprint/pprint ordered)))
    (.renameTo (io/file tmp) (io/file stats-path))))

(defn revalue-stats!
  "Recompute market-cap-adjusted and valuation ratios on data/stats.edn
  from existing :price, :shares, fundamentals, and :adr-ratio — no zip reread.

  Usage: clj -X:stats-revalue"
  ([] (revalue-stats! {}))
  ([_]
   (let [prices (load-price-by-ticker)
         before (edn/read-string (slurp stats-path))
         ps-before (count (filter :price-sales before))
         rows (mapv (fn [st]
                      (let [shares (:shares st)
                            ticker (some-> (:ticker st) str str/upper-case)
                            px (cond-> prices
                                 (and ticker (number? (:price st))
                                      (not (contains? prices ticker)))
                                 (assoc ticker (double (:price st))))]
                        (with-valuation st px shares)))
                    before)
         ps-after (count (filter :price-sales rows))]
     (save-stats! rows)
     (println (format "Wrote %s  (%d companies; price-sales %d → %d; pe=%d  ev-ebit=%d  dy=%d  buyback=%d  mcap-adj=%d)"
                      stats-path
                      (count rows)
                      ps-before
                      ps-after
                      (count (filter :price-earnings rows))
                      (count (filter :ev-ebit rows))
                      (count (filter :dividend-yield rows))
                      (count (filter :buyback-yield rows))
                      (count (filter :market-cap-adjusted rows))))
     rows)))

(defn fmt-growth
  [x]
  (when x (format "%.1f" (double x))))

(defn round1
  [x]
  (when x (/ (Math/round (* (double x) 10.0)) 10.0)))

(defn round4
  [x]
  (when x (/ (Math/round (* (double x) 10000.0)) 10000.0)))

(defn- universe-row-by-cik
  [cik]
  (let [want (universe/pad-cik cik)]
    (some #(when (= (universe/pad-cik (:cik %)) want) %)
          (load-universe))))

(defn stats-one
  "One stats row for `cik` from companyfacts dataset `dss` (nil when missing).

  Runs `company-stats` (when `dss` is present), ADR ratio, market valuation,
  and SIC enrichment. Looks up the universe row by `cik` unless `:row` is
  passed. Optional opts for the batch path: `:row`, `:prices`, `:sic-index`,
  `:shares`, `:data` (raw companyfacts map for shares-outstanding)."
  ([dss cik]
   (stats-one dss cik {}))
  ([dss cik {:keys [row prices sic-index shares data]}]
   (let [row (or row (universe-row-by-cik cik) {:cik (str cik)})
         prices (or prices (load-price-by-ticker))
         sic-index (or sic-index (load-sic-index))
         shares (or shares
                    (when data (universe/shares-outstanding (:facts data)))
                    (:shares-outstanding row))]
     (with-sic
       (try
         (let [base (if dss
                      (company-stats dss row)
                      (select-keys row [:ticker :exchange :cik :entityName
                                        :revenue-unit]))
               ratio (adr/get-ratio (:ticker base))]
           (-> base
               (cond-> ratio (assoc :adr-ratio ratio))
               (with-valuation prices shares)))
         (catch Exception ex
           (println (format "  stats-fail %s (%s)"
                            (or (:ticker row) cik) (.getMessage ex)))
           (select-keys row [:ticker :exchange :cik :entityName :revenue-unit])))
       sic-index))))

(defn stats
  "From data/universe-facts.edn + companyfacts.zip + prices.edn, write data/stats.edn
  with 7-year mean YoY sales growth (%),
  :net-income-change-q-on-y (latest quarter NI vs NI 4 quarters earlier, %,
  |prior| denominator), profit margin, 10-year mean
  profit margin (Net Income / revenue), return on assets (Net Income / assets),
  return on capital (Net Income / total equity), plus :shares (most recent 10-Q
  when that quarter is at least as recent as the annual report; otherwise
  the annual report), :annual-count (distinct annual periods in companyfacts),
  :price, :marketcap, :price-sales,
  :price-earnings (USD market cap / USD Net Income, else USD price / USD EPS),
  :dividend-yield (USD dividends paid / USD market cap, else USD dividend per share / price),
  :dividend-coverage (net income / dividends paid; omitted when none was paid),
  :buyback-yield (USD stock repurchase / USD market cap),
  :enterprise-value (USD market cap + debt + preferred + NCI − cash),
  :ev-ebit, :revenue-score, and :operating-income-score
  (0–100 share of annual years that did not fall versus the prior year).
  When operating income is not tagged, a year whose operating expenses were
  derived uses gross profit minus those expenses.
  :adr-ratio (ordinary shares per ADR from data/adr.edn) when known.
  :market-cap-adjusted (reporting-currency market cap after ADR) when FX allows.
  Non-USD or non-1 ADR names use that cap for price-sales, PE, yields, and EV.
  :sic, :sector, :industry, and :subindustry come from data/universe-fsds.edn.

  Usage: clj -X:stats
         clj -X:stats :limit 25"
  ([] (stats {}))
  ([{:keys [limit force]}]
   (e/init! dl/identity-header)
   (let [zip-path (dl/ensure-zip! {:force force})
         all (load-universe)
         prices (load-price-by-ticker)
         sic-index (load-sic-index)
         filers (vec (cond->> all limit (take (long limit))))
         total (count filers)]
     (println (format "stats  %d of %d universe  ← %s"
                      total (count all) zip-path))
     (flush)
     (with-open [zf (ZipFile. (str zip-path))]
       (loop [i 0
              acc (transient [])
              rows filers]
         (if-let [row (first rows)]
           (let [data (try (read-cik-json zf (:cik row))
                           (catch Exception ex
                             (println (format "  skip %s (%s)" (:cik row) (.getMessage ex)))
                             nil))
                 dss (when data
                       (try (xbrl/facts->dataset data)
                            (catch Exception ex
                              (println (format "  facts-ds %s (%s)" (:cik row) (.getMessage ex)))
                              nil)))
                 shares (or (when data
                              (universe/shares-outstanding (:facts data)))
                            (:shares-outstanding row))
                 st (stats-one dss (:cik row)
                               {:row row
                                :prices prices
                                :sic-index sic-index
                                :shares shares
                                :data data})
                 i' (inc i)]
             (when (or (zero? (mod i' 250)) (= i' total))
               (println (format "  %d/%d  %s  growth=%s  margin=%s  roa=%s  roc=%s"
                                i' total
                                (or (:ticker st) (:cik st))
                                (fmt-growth (:sales-growth-yoy st))
                                (some-> (:profit-margin st) (* 100) (#(format "%.1f" %)))
                                (some-> (:return-on-assets st) (* 100) (#(format "%.1f" %)))
                                (some-> (:return-on-capital st) (* 100) (#(format "%.1f" %)))))
               (flush))
             (recur i' (conj! acc st) (next rows)))
           (let [out (vec (persistent! acc))]
             (save-stats! out)
             (println)
             (pprint/print-table
              [:ticker :price :marketcap :price-sales :price-earnings
               :sales-growth-yoy :profit-margin :return-on-assets :return-on-capital]
              (take 25
                    (map (fn [r]
                           (-> r
                               (update :sales-growth-yoy round1)
                               (update :profit-margin round4)
                               (update :return-on-assets round4)
                               (update :return-on-capital round4)
                               (update :price-sales round4)
                               (update :price-earnings round4)))
                         out)))
             (println (format "Wrote %s  (%d companies; growth=%d  margin=%d  avg-margin=%d  roa=%d  roc=%d  price=%d  mcap=%d  adr=%d  pe=%d  ev=%d  ev-ebit=%d  div-coverage=%d  buyback=%d  rev-score=%d  oi-score=%d  sic=%d)"
                              stats-path
                              (count out)
                              (count (filter :sales-growth-yoy out))
                              (count (filter :profit-margin out))
                              (count (filter :avg-profit-margin out))
                              (count (filter :return-on-assets out))
                              (count (filter :return-on-capital out))
                              (count (filter :price out))
                              (count (filter :marketcap out))
                              (count (filter :adr-ratio out))
                              (count (filter :price-earnings out))
                              (count (filter :enterprise-value out))
                              (count (filter :ev-ebit out))
                              (count (filter :dividend-coverage out))
                              (count (filter :buyback-yield out))
                              (count (filter :revenue-score out))
                              (count (filter :operating-income-score out))
                              (count (filter :sic out))))
             out)))))))
