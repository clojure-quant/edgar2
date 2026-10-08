(ns edgar2.web.nav
  (:require [hyper.core :as h]))

(defn nav
  []
  [:nav.app-nav
   [:a (h/navigate :stats) "Stats"]
   " · "
   [:a (h/navigate :screen) "Screen"]
   " · "
   [:a (h/navigate :financials-home) "Financials"]])
