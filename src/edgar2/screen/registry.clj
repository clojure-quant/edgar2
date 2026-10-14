(ns edgar2.screen.registry
  "The screens the page and the CLI can run.

  Each entry is the `screen` var of one edgar2.algo namespace. Add a
  screen by adding its symbol here.")

(def screens
  '[edgar2.algo.cheap-growth/screen
    edgar2.algo.cheap-growth-ev-ebit/screen
    edgar2.algo.cheap-consistent/screen
    edgar2.algo.cheap-consistent-margin/screen
    edgar2.algo.high-revenue-growth/screen
    edgar2.algo.dividend-yield/screen
    edgar2.algo.lt-breakout/screen
    edgar2.algo.ltfsr/screen])

(defn all
  "Screen maps, in registry order."
  []
  (mapv (fn [sym]
          (if-let [v (requiring-resolve sym)]
            (deref v)
            (throw (ex-info "Cannot resolve screen" {:screen sym}))))
        screens))

(defn by-id
  "The screen map whose :id is `id`, or nil."
  [id]
  (some (fn [screen]
          (when (= id (:id screen)) screen))
        (all)))
