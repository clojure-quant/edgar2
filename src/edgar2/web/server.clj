(ns edgar2.web.server
  (:require [clojure.string :as str]
            [hyper.core :as h]
            [edgar2.web.company :refer [current-annual-page description-page google-page]]
            [edgar2.web.filing :refer [annual-report-page raw-filing-page]]
            [edgar2.web.filings :refer [filings-page]]
            [edgar2.web.financials :refer [financials-page]]
            [edgar2.web.overview :refer [overview-page]]
            [edgar2.web.screen :refer [screen-page]]
            [edgar2.web.segment :refer [segment-page]])
  (:import [java.net URLEncoder]))

(defn- redirect
  [location]
  {:status 302
   :headers {"Location" location}
   :body ""})

(defn- legacy-financials
  "Old /financials URLs. The ticker moves to the query string so the company
  tab can pick it up."
  [req]
  (let [p (get-in req [:hyper/route :path-params])
        t (some-> (:ticker p) str str/trim not-empty str/upper-case)
        n (:n p)
        period (:period p)
        path (cond
               (and n period) (str "/company/financials/" n "/" period)
               n (str "/company/financials/" n)
               :else "/company/financials")]
    (redirect (if t
                (str path "?ticker=" (URLEncoder/encode t "UTF-8"))
                path))))

(defn- legacy-filings
  [req]
  (redirect (str "/company/filings"
                 (when-let [q (some-> (:query-string req) not-empty)]
                   (str "?" q)))))

(def routes
  [["/" {:name :stats
         :title "Stats"
         :get #'overview-page}]
   ["/screen" {:name :screen
               :title "Screen"
               :get #'screen-page}]
   ["/segment" {:name :segment
                :title "Segment"
                :get #'segment-page}]
   ["/company/financials" {:name :company-financials
                           :title "Financials"
                           :get #'financials-page}]
   ["/company/financials/:n" {:name :company-financials-n
                              :title "Financials"
                              :get #'financials-page}]
   ["/company/financials/:n/:period" {:name :company-financials-period
                                      :title "Financials"
                                      :get #'financials-page}]
   ["/company/google-finance" {:name :company-google
                               :title "Google Finance"
                               :get #'google-page}]
   ["/company/filings" {:name :company-filings
                        :title "Filings"
                        :get #'filings-page}]
   ["/company/current-annual" {:name :company-annual
                               :title "Latest annual report"
                               :get #'current-annual-page}]
   ["/company/description" {:name :company-description
                            :title "Description"
                            :get #'description-page}]
   ["/financials" {:get #'legacy-financials}]
   ["/financials/:ticker" {:get #'legacy-financials}]
   ["/financials/:ticker/:n" {:get #'legacy-financials}]
   ["/financials/:ticker/:n/:period" {:get #'legacy-financials}]
   ["/filings" {:get #'legacy-filings}]
   ["/filing/:ticker" {:name :annual-report
                       :title "Annual report"
                       :get #'annual-report-page}]
   ["/filing/:cik/:accession" {:name :filing-raw
                               :title "Filing"
                               :get #'raw-filing-page}]
   ["/filing/:cik/:accession/:doc" {:name :filing-doc
                                    :title "Filing"
                                    :get #'raw-filing-page}]])

(defn web
  "Start the Hyper UI and block until the process stops.

  Usage: clj -X:web
         clj -X:web :port 8080"
  [{:keys [port] :or {port 3000}}]
  (let [port (long port)
        stop (h/start! (h/create-handler #'routes
                                         :head [[:link {:rel "stylesheet"
                                                        :href "/css/edgar2.css"}]]
                                         :static-resources "public")
                       {:port port})]
    (.addShutdownHook (Runtime/getRuntime) (Thread. (fn [] (stop))))
    (println (format "http://localhost:%d/" port))
    @(promise)))
