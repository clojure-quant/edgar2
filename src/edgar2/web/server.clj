(ns edgar2.web.server
  (:require [hyper.core :as h]
            [edgar2.web.filing :refer [annual-report-page raw-filing-page]]
            [edgar2.web.filings :refer [filings-page]]
            [edgar2.web.financials :refer [financials-page]]
            [edgar2.web.overview :refer [overview-page]]
            [edgar2.web.screen :refer [screen-page]]
            [edgar2.web.segment :refer [segment-page]]))

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
   ["/financials" {:name :financials-home
                   :title "Financials"
                   :get #'financials-page}]
   ["/financials/:ticker" {:name :financials
                           :title "Financials"
                           :get #'financials-page}]
   ["/financials/:ticker/:n" {:name :financials-n
                               :title "Financials"
                               :get #'financials-page}]
   ["/financials/:ticker/:n/:period" {:name :financials-period
                                          :title "Financials"
                                          :get #'financials-page}]
   ["/filings" {:name :filings
                :title "Filings"
                :get #'filings-page}]
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
