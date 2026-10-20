(ns edgar2.financials.calc.valuation
  "Market-cap and valuation ratios from stats fundamentals."
  (:require [clojure.string :as str]
            [edgar2.adr :as adr]
            [edgar2.download.fx :as fx]))

(defn usd-unit?
  "True for USD or USD/shares (US listing price is in dollars)."
  [u]
  (let [s (str/upper-case (str u))]
    (or (= s "USD")
        (str/starts-with? s "USD/"))))

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

(defn calculate-marketcap
  "Listing price × shares, plus adjusted / valuation market cap.

  Looks up `:adr-ratio` from `data/adr.edn` when known. Returns a map with
  present keys among `:adr-ratio`, `:shares`, `:price`, `:marketcap`,
  `:market-cap-adjusted`, and `:vmcap` (the cap later passed to
  `calculate-valuation` — adjusted when `needs-mcap-adjust?`)."
  [stats-map price-by-ticker shares]
  (let [price (get price-by-ticker (str/upper-case (str (:ticker stats-map))))
        mcap (when (and price (number? shares))
               (* (double price) (double shares)))
        ratio (or (:adr-ratio stats-map) (adr/get-ratio (:ticker stats-map)))
        mcap-adj (market-cap-adjusted mcap (:revenue-unit stats-map) ratio)
        need-adj? (needs-mcap-adjust? (:revenue-unit stats-map) ratio)
        vmcap (if need-adj? mcap-adj mcap)]
    (cond-> {}
      ratio (assoc :adr-ratio ratio)
      (number? shares) (assoc :shares shares)
      price (assoc :price price)
      mcap (assoc :marketcap mcap)
      mcap-adj (assoc :market-cap-adjusted mcap-adj)
      vmcap (assoc :vmcap vmcap))))

(defn calculate-valuation
  "Valuation ratios from `stats-map` and `market-cap` (vmcap).

  Returns a map of keys to merge onto the stats row: `:price-sales`,
  `:price-earnings`, `:dividend-yield`, `:buyback-yield`,
  `:enterprise-value`, and `:ev-ebit` when computable.

  PE is market cap / Net Income; dividend yield is dividends paid / market
  cap. Missing debt / preferred / NCI / cash legs count as 0 in EV."
  [stats-map market-cap]
  (let [vmcap (when (number? market-cap) market-cap)
        rev (when (number? (:revenue stats-map)) (:revenue stats-map))
        ni (when (number? (:net-profit stats-map)) (:net-profit stats-map))
        paid (when (number? (:dividends-paid stats-map)) (:dividends-paid stats-map))
        repurchase (when (number? (:stock-repurchase stats-map))
                     (:stock-repurchase stats-map))
        ebit (when (number? (:ebit stats-map)) (:ebit stats-map))
        pe (when (and vmcap (number? ni) (not (zero? (double ni))))
             (/ (double vmcap) (double ni)))
        dy (when (and vmcap (pos? (double vmcap)) (number? paid) (pos? (double paid)))
             (* 100.0 (/ (double paid) (double vmcap))))
        by (when (and vmcap (pos? (double vmcap))
                      (number? repurchase) (pos? (double repurchase)))
             (* 100.0 (/ (double repurchase) (double vmcap))))
        ev (when vmcap
             (+ (double vmcap)
                (double (or (:debt stats-map) 0))
                (double (or (:preferred-stock stats-map) 0))
                (double (or (:noncontrolling-interest stats-map) 0))
                (- (double (or (:cash stats-map) 0)))))]
    (cond-> {}
      (and vmcap rev (not (zero? (double rev))))
      (assoc :price-sales (/ (double vmcap) (double rev)))
      pe (assoc :price-earnings pe)
      dy (assoc :dividend-yield dy)
      by (assoc :buyback-yield by)
      ev (assoc :enterprise-value ev)
      (and ev (number? ebit) (not (zero? (double ebit))))
      (assoc :ev-ebit (/ ev (double ebit))))))
