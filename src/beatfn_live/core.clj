(ns beatfn-live.core
  (:use
    [overtone.live]
    [beatfn-live.globals]
    [beatfn-live.buttons]
    [beatfn-live.actions]
    [beatfn-live.samples]
    [beatfn-live.outputs]
    [beatfn-live.DB]
    [beatfn-live.ledAssertions]
    [beatfn-live.launchpad :only [draw-grid on-grid-pressed]]
    [beatfn-live.utilities]
    [overtone.inst.drum :only [quick-kick haziti-clap soft-hat open-hat]]))


; --------------- Storyboard Stuff --------------------

;
; callback stuff
;

(def callbacks
  (let [length (+ LAUNCHPAD_AREA 1)
        area (* length length)]
  (atom (vec (repeat area null-callback)))))

; quick indexing hack to account for the launchpad's function buttons
(defn i->xy [i]
  (let [length (+ 1 LAUNCHPAD_LENGTH)
        x (mod i length)
        y (/ (- i x) length)]
    [x y]))

(defn xy->i [x y]
  (+ (* y (+ LAUNCHPAD_LENGTH 1)) x))

(defn insert-callback [callback x y]
  (swap! callbacks
    (fn [prev] (assoc prev (xy->i x y) callback))))

(defn get-callback [x y]
  (nth @callbacks (xy->i x y)))

(defn button-press
  [x y pressed?]
  (do
    (println "x: " x " y: " y " pressed: " pressed?)
    ((get-callback x y) x y pressed?)))
    ;(if-let [grid-fn (get-grid-fn x y)]
    ;  (grid-fn x y pressed?))))

;
; startup stuff
;

; action state 0
(load-action rand-uplift4 0 0)
(load-action rand-uplift8 0 1)
(load-action rand-uplift16 0 2)
(load-action rand-downlift-crash 0 3)
(load-action rand-downlift-explode 0 4)
(load-action rand-downlift-fx 0 5)
(load-action toggle-deck1 0 6)
(load-action toggle-deck2 0 7)

; TODO: move this!
(def kick1 {
  :name "kick1"
  :sample? true
  :callback (fn [event] (sample "resources/kicks/1.wav"))
  })

(def closed-hat1 {
  :name "closed-hat1"
  :sample? true
  :callback (fn [event] (sample "resources/closed-hats/1.wav"))
  })

(def open-hat1 {
  :name "open-hat1"
  :sample? true
  :callback (fn [event] (sample "resources/open-hats/1.wav"))
  })

(def snare1 {
  :name "snare1"
  :sample? true
  :callback (fn [event] (sample "resources/snares/1.wav"))
  })

(def percussion1 {
  :name "percussion1"
  :sample? true
  :callback (fn [event] (sample "resources/percussions/1.wav"))
  })

(load-action kick1 1 0)
(load-action closed-hat1 1 1)
(load-action open-hat1 1 2)
(load-action snare1 1 3)
(load-action percussion1 1 4)

; action state 1
;(load-action rand-downlift-explode 8)

; action state 2
;(load-action rand-downlift-crash 16)

; action state 3
;(load-action rand-downlift-fx 24)

; set bank buttons
(no-print (domap #(insert-callback bank-button LAUNCHPAD_LENGTH %) (range LAUNCHPAD_LENGTH)))
(set-atom! volume-bank (vec (repeat LAUNCHPAD_LENGTH volume-button)))
(set-atom! action-buttons (vec (repeat LAUNCHPAD_LENGTH action-button)))

; set grid buttons
(no-print (doall
  (for [x (range LAUNCHPAD_LENGTH)
        y (range LAUNCHPAD_LENGTH)]
    (insert-callback grid-button x y))))

; set special buttons
(no-print (insert-callback step-size-button-up
  (:x step-size-up-loc) (:y step-size-up-loc)))

(no-print (insert-callback step-size-button-down
  (:x step-size-down-loc) (:y step-size-down-loc)))

(no-print (insert-callback scene-state-left-button
  (:x scene-state-left-loc) (:y scene-state-left-loc)))

(no-print (insert-callback scene-state-right-button
  (:x scene-state-right-loc) (:y scene-state-right-loc)))

(no-print (insert-callback tracker-state-button
  (:x tracker-state-loc) (:y tracker-state-loc)))

(no-print (insert-callback action-state-button
  (:x action-state-loc) (:y action-state-loc)))

(no-print (insert-callback repeat-state-button
  (:x repeat-state-loc) (:y repeat-state-loc)))

(no-print (insert-callback bank-state-button
  (:x bank-state-loc) (:y bank-state-loc)))

; final ready steps
(assert-leds)
(on-grid-pressed lpad button-press)