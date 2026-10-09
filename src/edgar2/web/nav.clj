(ns edgar2.web.nav
  (:require [hyper.core :as h]
            [hyper.effects :as effects]
            [edgar2.web.ticker :as ticker]))

(def company-routes
  #{:company-financials
    :company-financials-n
    :company-financials-period
    :company-google
    :company-filings
    :company-annual
    :company-description})

(def subnav
  "Second row, shown only on company pages. The value is the route names
  that count as that item."
  [["financials" :company-financials
    #{:company-financials :company-financials-n :company-financials-period}]
   ["google-finance" :company-google #{:company-google}]
   ["filings" :company-filings #{:company-filings}]
   ["current-annual" :company-annual #{:company-annual}]
   ["description" :company-description #{:company-description}]])

(defn- top-link
  [route-name label active? query]
  [:a (cond-> (h/navigate route-name {} query)
        active? (assoc :class "active"))
   label])

(defn- ticker-form
  [req]
  (let [current (ticker/current)
        ;; Captured at render. An action request does not carry :hyper/route.
        route-name (get-in req [:hyper/route :name])
        path-params (get-in req [:hyper/route :path-params])]
    [:form.ticker-form.company-ticker
     {:data-on:submit__prevent
      (h/action
        (let [next (ticker/normalize (:ticker $form-data))]
          (reset! (ticker/ticker*) next)
          (effects/navigate! route-name
                             path-params
                             (when next {:ticker next}))))}
     [:label "Ticker "
      [:input (cond-> {:type "text"
                       :name "ticker"
                       :value (or current "")}
                (not current) (assoc :autofocus true))]]
     [:button {:type "submit"} "Show"]]))

(defn- sub-link
  [req label route-name active-names]
  (let [here (get-in req [:hyper/route :name])
        q (ticker/query)
        active? (contains? active-names here)
        ;; Stay on the current financials count and period when that item
        ;; is already showing.
        link (if (and (= route-name :company-financials)
                      (#{:company-financials-n :company-financials-period} here))
               (h/navigate here (get-in req [:hyper/route :path-params]) q)
               (h/navigate route-name {} q))]
    [:a (cond-> link
          active? (assoc :class "active")
          true (assoc :key label))
     label]))

(defn nav
  "Stats, Screen, and Company. Company adds the ticker field and a second
  row of company pages. The ticker lives in tab state, so those links keep it."
  [req]
  (let [here (get-in req [:hyper/route :name])
        company? (contains? company-routes here)
        q (ticker/query)]
    [:div.app-nav-wrap
     [:nav.app-nav
      (top-link :stats "Stats" (= here :stats) nil)
      " · "
      (top-link :screen "Screen" (= here :screen) nil)
      " · "
      (top-link :company-financials "Company" company? q)]
     (when company?
       [:div
        (ticker-form req)
        (into [:nav.app-subnav]
              (interpose " · "
                         (for [[label route-name active-names] subnav]
                           (sub-link req label route-name active-names))))])]))
