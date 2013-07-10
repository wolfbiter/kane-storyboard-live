(ns beatfn-live.utilities
  (:use
    [beatfn-live.globals]
    [beatfn-live.actionDB]
    [beatfn-live.launchpad :only [draw-grid]]))

;
; utilities
;

(defn get-sample-volume [] (/ @sample-volume-state LAUNCHPAD_LENGTH))

(defn clamp [n min max]
  (cond
    (> n max) max
    (< n min) min
    :else n))

(defn domap [& args]
  (doall (apply map args)))

(defn mod-beat-max [beat]
  (mod beat (* MAX_ZOOM LAUNCHPAD_AREA)))

(defn mod-beat-zoom [beat]
  (mod beat (* LAUNCHPAD_AREA @zoom-state)))

(defn get-beat-event [raw-beat]
  (str "beat-event" (mod-beat-max raw-beat)))

(defn get-action-handle
  [scheduled-action]
    (let [beat-event (:beat-event scheduled-action)
          name (:name scheduled-action)
          scene-state @scene-state]
      (keyword (str beat-event "_scene" scene-state "_" name))))

(defn load-action [action i]
  (swap! loaded-actions (fn [prev] (assoc prev i (assoc action :bank-pos i)))))

(defn get-active-actions []
  (map
    #(nth @loaded-actions (+ (* LAUNCHPAD_LENGTH @action-state) %))
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
    (* beat @zoom-state)))

(defn beat->xy [raw-beat]
  (let [beat (/ (mod-beat-zoom raw-beat) @zoom-state)
        x (mod beat LAUNCHPAD_LENGTH)
        y (/ (- beat x) LAUNCHPAD_LENGTH)]
    [x y]))

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