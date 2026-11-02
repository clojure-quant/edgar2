(ns edgar2.web.industry
  "Browse data/stats.edn by SIC sector, industry, and subindustry."
  (:require [clojure.string :as str]
            [hyper.core :as h]
            [edgar2.algo.growth :as growth]
            [edgar2.algo.industry :as algo]
            [edgar2.db.sector :as sic]
            [edgar2.web.controls :as controls]
            [edgar2.web.nav :refer [nav]]
            [edgar2.web.overview :as overview]
            [edgar2.web.screen :as screen-web]))

(defn- prompt-options
  "`prompt` as the blank option, followed by `values` as string options."
  [prompt values]
  (into [{:value "" :label prompt}] (controls/options values)))

(defn- filter-value
  "Trimmed, non-blank filter string, or nil."
  [value]
  (some-> value str str/trim not-empty))

(defn- apply-filter!
  "Apply one cascading selection from `filters*`.

  A sector change resets the children. An industry or subindustry change
  is ignored unless its parents are already set, and clears any deeper
  selection."
  [filters* {:keys [param value]}]
  (let [value (filter-value value)]
    (case param
      "sector"
      (reset! filters* (if value {:sector value} {}))

      "industry"
      (let [sector (:sector @filters*)]
        (when sector
          (reset! filters* (cond-> {:sector sector}
                             value (assoc :industry value)))))

      "subindustry"
      (let [{:keys [sector industry]} @filters*]
        (when (and sector industry)
          (reset! filters* (cond-> {:sector sector :industry industry}
                             value (assoc :subindustry value)))))

      nil)))

(defn- effective-filters
  "Only the filters whose parents are present."
  [filters]
  (let [sector (:sector filters)
        industry (when sector (:industry filters))
        subindustry (when industry (:subindustry filters))]
    (cond-> {}
      sector (assoc :sector sector)
      industry (assoc :industry industry)
      subindustry (assoc :subindustry subindustry))))

(defn- selection-line
  "Company count and the selection path for `filters`."
  [n filters]
  (let [parts (keep filters [:sector :industry :subindustry])]
    (str n " " (if (= 1 n) "company" "companies")
         (when (seq parts)
           (str " · " (str/join " / " parts))))))

(defn- controls-row
  [filters* filters all-sic]
  (let [sector (:sector filters)
        industry (:industry filters)]
    [:div.industry-controls
     (controls/param-select
       {:label "Sector"
        :param "sector"
        :value (or sector "")
        :options (prompt-options "Choose sector" (sic/sectors all-sic))
        :data-on:param-select-change
        (h/action (apply-filter! filters* $detail))})
     (controls/param-select
       (cond-> {:label "Industry"
                :param "industry"
                :value (or industry "")
                :options (if sector
                           (prompt-options "All industries"
                                           (sic/industries all-sic sector))
                           [{:value "" :label "Choose sector first"}])
                :data-on:param-select-change
                (h/action (apply-filter! filters* $detail))}
         (nil? sector) (assoc :disabled true)))
     (controls/param-select
       (cond-> {:label "Subindustry"
                :param "subindustry"
                :value (or (:subindustry filters) "")
                :options (if (and sector industry)
                           (prompt-options
                            "All subindustries"
                            (sic/subindustries all-sic sector industry))
                           [{:value "" :label "Choose industry first"}])
                :data-on:param-select-change
                (h/action (apply-filter! filters* $detail))}
         (or (nil? sector) (nil? industry)) (assoc :disabled true)))]))

(defn- results
  [filters]
  (let [loaded (try {:rows (overview/load-rows)}
                    (catch Exception e
                      {:error (or (ex-message e) (str e))}))]
    (if-let [error (:error loaded)]
      [:p.error error]
      (let [effective (effective-filters filters)
            rows (algo/rows (:rows loaded) effective)]
        [:div
         [:p.industry-count (selection-line (count rows) effective)]
         (screen-web/screen-table {:columns growth/columns :rows rows})]))))

(defn industry-page
  "SIC sector, industry, and subindustry browser over the stats rows."
  [_req]
  (let [filters* (h/tab-cursor :industry/filters {})]
    (fn [req]
      (let [filters @filters*
            sector (:sector filters)]
        [:div.page
         (nav req)
         [:h1 "Industry"]
         (controls-row filters* filters (sic/sic-rows))
         (if sector
           (results filters)
           [:p.industry-prompt "Choose a sector first."])]))))
