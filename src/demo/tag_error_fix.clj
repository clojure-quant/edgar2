(ns demo.tag-error-fix
  "Detect XBRL tag errors where a currency decimal sits outside the fact.

  Example: a filing shows ($.98 per share) but the tagged value is 98.
  `absorb-currency-decimal` corrects that by reading the primary HTML.

  Not used on the stats.edn path — fetching filings for every company
  would slow the universe rebuild dramatically."
  (:require [clojure.java.io :as io]
            [edgar.api :as e]
            [edgar2.filing :as filing]
            [edgar2.util.ticker-info :as ticker-info]))

(defn whole-dollar?
  "True when v is a positive whole number. A split ($.98 …) tag lands here as 98."
  [v]
  (and (number? v)
       (>= (double v) 1.0)
       (< (Math/abs (- (double v) (Math/rint (double v)))) 1e-6)))

(defn primary-html
  "Primary filing document, cached under data/ix-html."
  [cik accn]
  (let [f (io/file "data/ix-html" (str accn ".htm"))]
    (or (when (.exists f) (slurp f))
        (when-let [html (e/html {:cik (ticker-info/pad-cik cik)
                                 :accessionNumber (str accn)})]
          (.mkdirs (.getParentFile f))
          (spit f html)
          html))))

(defn absorb-currency-decimal
  "Read ($.98 per share) as 0.98 when the tagged fact is the whole number 98."
  [cik {:keys [val end accn taxonomy tag] :as fact}]
  (if-not (and cik accn taxonomy tag (whole-dollar? val))
    fact
    (try
      (if-let [html (primary-html cik accn)]
        (if-let [v (filing/leading-currency-decimal
                    html
                    (str (name taxonomy) ":" (name tag))
                    end
                    val)]
          (assoc fact :val v)
          fact)
        fact)
      (catch Throwable _
        fact))))
