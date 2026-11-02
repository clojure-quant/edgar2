(ns edgar2.download.fsds-universe-test
  (:require [clojure.test :refer [deftest is testing]]
            [edgar2.db.sector :as sector]
            [edgar2.download.fsds-universe :as fsds]))

(defn- row
  [cik adsh form period filed]
  {:cik cik
   :name "Example Filer"
   :adsh adsh
   :form form
   :sic "4512"
   :afs "LAF"
   :period period
   :filed filed})

(defn- sub
  [cik adsh form period filed]
  (fsds/sub-from-row (row cik adsh form period filed)))

(deftest annual-forms-cover-reports-and-amendments
  (is (= #{"10-K" "10-K/A" "20-F" "20-F/A" "40-F" "40-F/A"}
         fsds/annual-forms))
  (doseq [form ["10-K" "10-K/A" "20-F" "20-F/A" "40-F" "40-F/A"]]
    (testing form
      (is (true? (fsds/annual-sub? (row "1" "a" form "20251231" "20260226"))))))
  (doseq [form ["10-Q" "10-Q/A" "6-K" "8-K" "" nil]]
    (testing (pr-str form)
      (is (false? (fsds/annual-sub? (row "1" "a" form "20251231" "20260226")))))))

(deftest annual-sub-requires-cik-and-accession
  (is (false? (fsds/annual-sub? {:form "10-K" :cik "" :adsh "a"})))
  (is (false? (fsds/annual-sub? {:form "10-K" :cik "1" :adsh "  "})))
  (is (false? (fsds/annual-sub? {:form "10-K" :cik nil :adsh "a"}))))

(deftest sub-from-row-pads-and-trims
  (is (= {:cik "0001345105"
          :name "COPA HOLDINGS, S.A."
          :adsh "0001345105-26-000019"
          :form "20-F"
          :sic "4512"
          :afs "LAF"
          :period "20251231"
          :filed "20260226"}
         (fsds/sub-from-row
          {:cik "1345105"
           :name "COPA HOLDINGS, S.A.  "
           :adsh "0001345105-26-000019"
           :form "20-F\r"
           :sic "4512"
           :afs "LAF"
           :period "20251231"
           :filed "20260226"}))))

(deftest collect-annual-subs-keeps-foreign-annual-filers
  (let [rows [(row "1345105" "cpa20" "20-F" "20251231" "20260226")
              (row "1345105" "cpa20a" "20-F/A" "20251231" "20260310")
              (row "320193" "forty" "40-F" "20251231" "20260226")
              (row "789019" "q" "10-Q" "20250930" "20251030")
              (row "789019" "k" "10-K" "20250630" "20250801")]
        collected (binding [*out* (java.io.StringWriter.)]
                    (with-redefs [fsds/reduce-zip-tsv
                                  (fn [_zip _entry rf init] (reduce rf init rows))]
                      (fsds/collect-annual-subs ["2026q3.zip"])))]
    (is (= {"0001345105" ["cpa20" "cpa20a"]
            "0000320193" ["forty"]
            "0000789019" ["k"]}
           (update-vals collected #(mapv :adsh %))))))

(deftest select-prefers-newer-period-over-late-amendment
  (let [selected (fsds/select-annual-subs
                  {"0000000001"
                   [(sub "0000000001" "old" "10-K" "20241231" "20250215")
                    (sub "0000000001" "old-a" "10-K/A" "20241231" "20260601")
                    (sub "0000000001" "new" "10-K" "20251231" "20260220")]})]
    (is (= "new" (get-in selected ["0000000001" :adsh])))
    (is (= ["new"] (get-in selected ["0000000001" :annual-adshs])))))

(deftest select-prefers-same-period-amendment
  (let [selected (fsds/select-annual-subs
                  {"0000000001"
                   [(sub "0000000001" "orig" "20-F" "20251231" "20260226")
                    (sub "0000000001" "amend" "20-F/A" "20251231" "20260310")]})]
    (is (= "amend" (get-in selected ["0000000001" :adsh])))
    (is (= "20-F/A" (get-in selected ["0000000001" :form])))
    (is (= ["amend" "orig"] (get-in selected ["0000000001" :annual-adshs])))))

(deftest select-prefers-amendment-on-a-tied-filed-date
  (let [selected (fsds/select-annual-subs
                  {"0000000001"
                   [(sub "0000000001" "orig" "40-F" "20251231" "20260310")
                    (sub "0000000001" "amend" "40-F/A" "20251231" "20260310")]})]
    (is (= "amend" (get-in selected ["0000000001" :adsh])))
    (is (= ["amend" "orig"] (get-in selected ["0000000001" :annual-adshs])))))

(deftest select-falls-back-to-filed-for-blank-periods
  (let [selected (fsds/select-annual-subs
                  {"0000000001"
                   [(sub "0000000001" "older" "10-K" "" "20240101")
                    (sub "0000000001" "newer" "10-K" "" "20250101")]})]
    (is (= "newer" (get-in selected ["0000000001" :adsh])))
    (is (= ["newer"] (get-in selected ["0000000001" :annual-adshs])))))

(defn- revenue-fact
  [tag val]
  {tag {:val val :ddate "20251231" :qtrs 4}})

(deftest annual-revenue-falls-back-within-the-selected-period
  (let [tag (first fsds/revenue-tags)
        facts {"amend" (revenue-fact tag 10.0)
               "orig" (revenue-fact tag 20.0)
               "old" (revenue-fact tag 30.0)}]
    (is (= 10.0 (fsds/annual-revenue facts ["amend" "orig"])))
    (is (= 20.0 (fsds/annual-revenue facts ["amend-empty" "orig"])))
    (is (nil? (fsds/annual-revenue facts ["amend-empty"])))
    (is (nil? (fsds/annual-revenue facts ["amend-empty" "orig-empty"])))
    (is (= 30.0 (fsds/annual-revenue facts ["old"])))))

(deftest build-row-writes-annual-keys
  (let [tag (first fsds/revenue-tags)
        facts {"amend" (revenue-fact tag 12.0)}
        row (with-redefs [sector/get-sector-data (constantly nil)]
              (fsds/build-filer-info-row
               (assoc (sub "1345105" "amend" "20-F/A" "20251231" "20260310")
                      :annual-adshs ["amend"])
               facts))]
    (is (= "20-F/A" (:last-annual-form row)))
    (is (= "2026-03-10" (:last-annual-date row)))
    (is (= "2025-12-31" (:last-annual-period row)))
    (is (= "amend" (:last-annual-accession row)))
    (is (= 12.0 (:last-annual-revenue row)))
    (is (nil? (some #(re-find #"^last-10k" (name %)) (keys row))))))
