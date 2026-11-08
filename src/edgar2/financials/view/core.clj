(ns edgar2.financials.view.core
  "Which rows a financials view shows.

  A field map is one of:
    :field     one [taxonomy concept] pair
    :concept   an id from edgar2.financials.concepts, via facts-ds/calc-concept
    :edgarjure a statement label from edgarjure
    :our       a column key from calc.concept/calc-concepts

  :style is the row background (:background-pl-green,
  :background-balance-active, or :background-balance-passive), written on
  the field map and carried as column metadata. :format is the value format
  (:millions, :percent, :dec2, ...) and :section is :income, :balance, or
  :ratio.

  Views: :default (Our rows: P&L + balance sheet + ratios), :all (every
  concept on a wide report-table), :edgarjure-standard / :edgarjure-insurance
  / :edgarjure-bank / :edgarjure-reit (pure edgarjure income labels plus
  that view's own balance-sheet lines).

  Each view's rows are a static vector of field maps, already in display
  order."
  (:require [clojure.string :as str]
            [edgar2.financials.view.edgarjure :as edgarjure]
            [edgar2.financials.view.our :as our]))

(def view-choices
  [[:default "Our"]
   [:all "All fields"]
   [:edgarjure-standard "edgarjure Standard"]
   [:edgarjure-insurance "edgarjure Insurance"]
   [:edgarjure-bank "edgarjure Bank"]
   [:edgarjure-reit "edgarjure REIT"]])

(defn parse-view
  "One of `view-choices`, or :default."
  [view]
  (let [s (str/lower-case
           (cond
             (nil? view) "default"
             (keyword? view) (name view)
             :else (str view)))
        k (keyword s)]
    (if (some #(= k (first %)) view-choices) k :default)))

(defn view-label
  [view]
  (or (second (first (filter #(= (parse-view view) (first %)) view-choices)))
      "Our"))

(defn view-fields
  "Field specs for `view`. `industry` is unused for :default (Our rows) and
  for :all. :edgarjure-* uses that view's industry labels plus its own copy
  of the balance-sheet lines."
  [view industry]
  (case (parse-view view)
    :all []
    :edgarjure-standard (edgarjure/edgarjure-views :standard)
    :edgarjure-insurance (edgarjure/edgarjure-views :insurance)
    :edgarjure-bank (edgarjure/edgarjure-views :bank)
    :edgarjure-reit (edgarjure/edgarjure-views :reit)
    (our/our-view)))
