(ns edgar2.quarterly-6k-test
  "Study: 10-Q-only vs current `quarterly-forms` (10-Q + 6-K).

  Baseline redefs to 10-Q only; current uses production (includes 6-K).
  US 10-Q filers should be unchanged — domestic issuers do not furnish 6-K."
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.test :refer [deftest is testing]]
            [edgar.api :as e]
            [edgar.xbrl :as xbrl]
            [edgar2.download.facts :as dl]
            [edgar2.financials.facts-ds :as facts-ds]
            [jsonista.core :as json]
            [tech.v3.dataset :as ds])
  (:import [java.util.zip ZipFile]))

(def companyfacts-zip "data/companyfacts.zip")

(def quarterly-forms-10q-only
  "Pre-6-K baseline for comparison against current `facts-ds/quarterly-forms`."
  #{"10-Q" "10-Q/A"})

(def sample-tickers
  "10 US 10-Q/10-K filers + 10 foreign private issuers (20-F / 6-K)."
  [{:ticker "AAPL" :cik "0000320193" :group :us}
   {:ticker "MSFT" :cik "0000789019" :group :us}
   {:ticker "XOM" :cik "0002115436" :group :us}
   {:ticker "JNJ" :cik "0000200406" :group :us}
   {:ticker "ABT" :cik "0000001800" :group :us}
   {:ticker "AMD" :cik "0000002488" :group :us}
   {:ticker "AIR" :cik "0000001750" :group :us}
   {:ticker "APD" :cik "0000002969" :group :us}
   {:ticker "SWKS" :cik "0000004127" :group :us}
   {:ticker "LNG" :cik "0000003570" :group :us}
   {:ticker "TAK" :cik "0001395064" :group :fpi}
   {:ticker "SONY" :cik "0000313838" :group :fpi}
   {:ticker "BP" :cik "0000313807" :group :fpi}
   {:ticker "TM" :cik "0001094517" :group :fpi}
   {:ticker "NVO" :cik "0000353278" :group :fpi}
   {:ticker "ASML" :cik "0000937966" :group :fpi}
   {:ticker "SAP" :cik "0001000184" :group :fpi}
   {:ticker "SHEL" :cik "0001306965" :group :fpi}
   {:ticker "DEO" :cik "0000835403" :group :fpi}
   {:ticker "BABA" :cik "0001577552" :group :fpi}])

(defn- zip-available?
  []
  (.exists (io/file companyfacts-zip)))

(defn- load-facts
  [zf cik]
  (let [entry (.getEntry zf (str "CIK" cik ".json"))]
    (when-not entry
      (throw (ex-info "Missing CIK json in companyfacts.zip" {:cik cik})))
    (with-open [in (.getInputStream zf entry)]
      (xbrl/facts->dataset
       (json/read-value in json/keyword-keys-object-mapper)))))

(defn- form-counts
  "form → distinct accession count in companyfacts."
  [dss]
  (->> (ds/rows dss {:nil-missing? true})
       (map (juxt #(str (:form %)) #(str (:accn %))))
       distinct
       (map first)
       frequencies))

(defn- period-signature
  "Comparable fingerprint of quarterly-periods rows."
  [periods]
  (->> (ds/rows periods {:nil-missing? true})
       (map (fn [r]
              {:filed (str (:filed r))
               :form (str (:form r))
               :fy (:fy r)
               :fp (str (:fp r))
               :start (str (:start r))
               :end (str (:end r))
               :period (:period r)
               :accn (str (:accn r))}))
       (sort-by (juxt :end :filed :form :accn))
       vec))

(defn- quarterly-periods-10q-only
  [dss]
  (try
    (with-redefs [facts-ds/quarterly-forms quarterly-forms-10q-only]
      (facts-ds/quarterly-periods dss))
    (catch Exception _ nil)))

(defn- quarterly-periods-current
  "Current production set (10-Q + 6-K)."
  [dss]
  (try
    (facts-ds/quarterly-periods dss)
    (catch Exception _ nil)))

(defn- compare-ticker
  [dss {:keys [ticker cik group]}]
  (let [forms (form-counts dss)
        base (quarterly-periods-10q-only dss)
        with6 (quarterly-periods-current dss)
        sig-b (when base (period-signature base))
        sig-6 (when with6 (period-signature with6))
        ends-b (into #{} (map :end) sig-b)
        ends-6 (into #{} (map :end) sig-6)]
    {:ticker ticker
     :cik cik
     :group group
     :forms forms
     :has-6k? (boolean (or (get forms "6-K") (get forms "6-K/A")))
     :has-10q? (boolean (or (get forms "10-Q") (get forms "10-Q/A")))
     :n-base (count sig-b)
     :n-with-6k (count sig-6)
     :unchanged? (= sig-b sig-6)
     :added-ends (sort (set/difference ends-6 ends-b))
     :removed-ends (sort (set/difference ends-b ends-6))
     :added-rows (vec (remove (set sig-b) sig-6))}))

(defn- print-summary
  [rows]
  (println)
  (println (format "%-6s %-4s %-5s %-5s %4s %4s %-10s added-ends"
                   "ticker" "grp" "10-Q?" "6-K?" "n0" "n6" "unchanged?"))
  (doseq [{:keys [ticker group has-10q? has-6k? n-base n-with-6k unchanged?
                  added-ends]}
          rows]
    (println (format "%-6s %-4s %-5s %-5s %4d %4d %-10s %s"
                     ticker (name group) has-10q? has-6k?
                     n-base n-with-6k unchanged?
                     (pr-str added-ends)))))

(deftest quarterly-forms-with-6k-vs-baseline-20-tickers
  "Compare 10-Q-only periods to production (10-Q + 6-K) for 20 tickers.

  Expectation: US 10-Q filers are unchanged (no Form 6-K in companyfacts).
  FPIs may gain periods only when companyfacts carries 6-K XBRL with
  ~75–115 day windows and non-FY `:fp`. Note: companyfacts often lacks
  6-K numeric data even when EDGAR has HTML 6-K filings."
  (when (zip-available?)
    (e/init! dl/identity-header)
    (with-open [zf (ZipFile. companyfacts-zip)]
      (let [rows (mapv (fn [t]
                         (compare-ticker (load-facts zf (:cik t)) t))
                       sample-tickers)
            us (filterv #(= :us (:group %)) rows)
            fpi (filterv #(= :fpi (:group %)) rows)]
        (print-summary rows)

        (testing "sample size"
          (is (= 20 (count rows)))
          (is (= 10 (count us)))
          (is (= 10 (count fpi))))

        (testing "US 10-Q filers: no 6-K in companyfacts, periods unchanged"
          (doseq [r us]
            (is (:has-10q? r) (str (:ticker r) " should have 10-Q facts"))
            (is (not (:has-6k? r))
                (str (:ticker r) " unexpected 6-K in companyfacts: "
                     (:forms r)))
            (is (:unchanged? r)
                (str (:ticker r) " quarterly periods changed with 6-K: "
                     "added=" (:added-ends r)
                     " removed=" (:removed-ends r)))))

        (testing "FPI side: report who gains periods (no hard assertion on gain)"
          (let [changed (filterv (complement :unchanged?) fpi)
                with-6k-facts (filterv :has-6k? fpi)]
            (println "\nFPI with 6-K facts in companyfacts:"
                     (mapv :ticker with-6k-facts))
            (println "FPI whose quarterly periods change:"
                     (mapv (fn [r]
                             {:ticker (:ticker r)
                              :n-base (:n-base r)
                              :n-with-6k (:n-with-6k r)
                              :added-ends (:added-ends r)
                              :added-forms (frequencies (map :form (:added-rows r)))})
                           changed))
            ;; Soft check: if companyfacts has no 6-K XBRL at all for a name,
            ;; periods must be unchanged.
            (doseq [r (remove :has-6k? fpi)]
              (is (:unchanged? r)
                  (str (:ticker r)
                       " has no 6-K facts but periods changed")))))))))
