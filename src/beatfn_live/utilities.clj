(ns beatfn-live.utilities
  (:use
    [beatfn-live.globals]
    [beatfn-live.DB]
    [beatfn-live.launchpad :only [draw-grid]]))

;
; utilities
;

(defn no-print [& args]
  (println))

(defn get-sample-volume [] (/ @sample-volume-state LAUNCHPAD_LENGTH))

(defn clamp [n min max]
  (cond
    (> n max) max
    (< n min) min
    :else n))

(defn domap [& args]
  (doall (apply map args)))

(defn mod-beat-max [beat]
  (mod beat (* LAUNCHPAD_AREA @step-size)))
  ;(mod beat (* MAX_STEP LAUNCHPAD_AREA)))

(defn mod-beat-zoom [beat]
  (mod beat (* LAUNCHPAD_AREA @step-size)))

(defn get-beat-event [raw-beat]
  (str "beat-event" (double (mod-beat-max raw-beat))))

(defn get-action-handle
  [scheduled-action]
    (let [beat-event (:beat-event scheduled-action)
          name (:name scheduled-action)
          scene-state (:scene-state scheduled-action)]
      (keyword (str beat-event "_scene" scene-state "_" name))))

(defn get-action-bank
  ([] (get-action-bank @action-state))
  ([bank] (nth action-banks bank)))

; TODO: move this associating actions to somewhere sensical? like a function
;       which gathers all things that must be associated to new actions? maybe not.
(defn load-action [action bank pos]
  (no-print
    (swap! (get-action-bank bank) (fn [prev]
      (assoc prev pos (assoc action :action-state @action-state :bank-pos pos))))))

(defn get-active-actions []
  (map
    #(nth @(get-action-bank) %)
    @active-action-numbers))

(defn get-matching-actions [scene-state beat]
  (let [active-actions (get-active-actions)
        possible-actions (find-actions {
          :scene-state scene-state
          :beat-event (get-beat-event beat)
        })
        matching-actions (filter #(not (nil? %))
          (for [possible-action possible-actions
                active-action active-actions]

            ; only return matching actions
            (if (= (:name active-action) (:name possible-action))
              possible-action)))]

    ;(println "matching actions: " matching-actions)
    [possible-actions matching-actions]))

(defn xy->beat [x y]
  (let [beat (+ (* y LAUNCHPAD_LENGTH) x)]
    (* beat @step-size)))

(defn beat->xy [raw-beat]
  (let [beat (/ (mod-beat-zoom raw-beat) @step-size)
        x (mod beat LAUNCHPAD_LENGTH)
        y (/ (- beat x) LAUNCHPAD_LENGTH)]
    [(int x) (int y)]))

(defn prev-grid-pos [x y]
  (let [x1 (mod (dec x) LAUNCHPAD_LENGTH)
        y1 (if (zero? x) (mod (dec y) LAUNCHPAD_LENGTH) y)]
    [x1 y1]))

(defn set-atom!
  [atom val]
  (swap! atom (fn [x] val)))


; TODO: make a function from the following 3 that allows filling in tl->br given 2 spots

(defn set-line
  [lpad orientation line color intensity start end]
  (cond
    (= orientation "row")
    (domap #(draw-grid lpad % line color intensity) (range start end))
    (= orientation "column")
    (domap #(draw-grid lpad line % color intensity) (range start end))
    :else (println "ERROR: Unknown set-line orientation " orientation)))

(defn set-row
  "Sets the row of given lpad to given color, intensity defaults to :low."
  ([lpad row color]
    (set-row lpad row color :low))
  ([lpad row color intensity]
    (set-row lpad row color intensity 0 LAUNCHPAD_LENGTH))
  ([lpad row color start end]
    (set-row lpad row color :low start end))
  ([lpad row color intensity start end]
    (set-line lpad "row" row color intensity start end)))

(defn set-column
  "Sets the column of given lpad to given color, intensity defaults to :low."
  ([lpad column color]
    (set-column lpad column color :low))
  ([lpad column color intensity]
    (set-column lpad column color intensity 0 LAUNCHPAD_LENGTH))
  ([lpad column color start end]
    (set-column lpad column color :low start end))
  ([lpad column color intensity start end]
    (set-line lpad "column" column color intensity start end)))