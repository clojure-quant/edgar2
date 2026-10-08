(ns edgar2.web.server
  (:require [hyper.core :as h]
            [edgar2.web.filing :refer [annual-report-page]]
            [edgar2.web.financials :refer [financials-page]]
            [edgar2.web.overview :refer [overview-page]]
            [edgar2.web.screen :refer [screen-page]]))

(def routes
  [["/" {:name :stats
         :title "Stats"
         :get #'overview-page}]
   ["/screen" {:name :screen
               :title "Screen"
               :get #'screen-page}]
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
   ["/filing/:ticker" {:name :annual-report
                       :title "Annual report"
                       :get #'annual-report-page}]])

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
