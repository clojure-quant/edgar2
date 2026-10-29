(ns edgar2.financials.view.core
  "Which rows a financials view shows.

  A field map is one of:
    :field     one [taxonomy concept] pair
    :concept   an id from edgar2.financials.concepts, via facts-ds/calc-concept
    :edgarjure a statement label from edgarjure
    :math      a key in edgar2.financials.math/math

  :style and :format are the row background (:background-pl-green,
  :background-balance-active, or :background-balance-passive), written on
  the field map and carried as column metadata. :section is :income or
  :balance.

  Views: :default (static P&L plus balance sheet), :all (every concept on
  a wide report-table), :edgarjure-standard / :edgarjure-insurance /
  :edgarjure-bank / :edgarjure-reit (pure edgarjure income labels plus
  the shared balance-sheet lines).

  Each view's rows are a static vector of field maps, already in display
  order."
  (:require [clojure.string :as str]
            [edgar2.financials.view.balance :as balance]
            [edgar2.financials.view.edgarjure :as edgarjure]
            [edgar2.financials.view.pl :as pl]))

(def view-choices
  [[:default "Current"]
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
      "Current"))

(defn- edgarjure-view-fields
  [industry]
  (vec (concat (edgarjure/edgarjure-income-industry industry)
               balance/balance-fields)))

(defn view-fields
  "Field specs for `view`. `industry` is unused for :default (fixed P&L)
  and for :edgarjure-* (that view's industry labels). :all has no specs."
  [view industry]
  (case (parse-view view)
    :all []
    :edgarjure-standard (edgarjure-view-fields :standard)
    :edgarjure-insurance (edgarjure-view-fields :insurance)
    :edgarjure-bank (edgarjure-view-fields :bank)
    :edgarjure-reit (edgarjure-view-fields :reit)
    (vec (concat pl/pl-fields balance/balance-fields))))
