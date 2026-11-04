(ns edgar2.regression.financial
  "Earnings regression summary statistics."
  (:require [edgar2.regression.core :as regression]))

(defn earnings-regression
  "Fit earnings and return summary statistics.

  `opts` selects the fit (`:regression-type`, `:error`); `vals` is the
  oldest-first earnings vector. Returns `:earnings-stabilized` (fitted
  value at offset 0), `:earnings-stabilized-yoy` (fitted growth percent),
  and `:earnings-variation` (residual variation). Returns nil when the fit
  has too few points."
  [opts vals]
  (when-let [earn (regression/regression opts vals)]
    {:earnings-stabilized (regression/estimate opts earn 0)
     :earnings-stabilized-yoy (regression/slope-percent opts earn)
     :earnings-variation (:variation earn)}))
