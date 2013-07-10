(ns beatfn-live.core
  (:use
    [overtone.live]
    [beatfn-live.globals]
    [beatfn-live.buttons]
    [beatfn-live.actions]
    [beatfn-live.samples]
    [beatfn-live.outputs]
    [beatfn-live.actionDB]
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
(load-action rand-uplift4 0)
(load-action rand-uplift8 1)
(load-action rand-uplift16 2)
(load-action rand-downlift-crash 3)
(load-action rand-downlift-explode 4)
(load-action rand-downlift-fx 5)
(load-action toggle-deck1 6)
(load-action toggle-deck2 7)

; action state 1
(load-action rand-downlift-explode 8)

; action state 2
(load-action rand-downlift-crash 16)

; action state 3
(load-action rand-downlift-fx 24)

; set bank buttons
(domap #(insert-callback bank-button LAUNCHPAD_LENGTH %) (range LAUNCHPAD_LENGTH))
(set-atom! action-bank (vec (repeat LAUNCHPAD_LENGTH action-button)))
(set-atom! volume-bank (vec (repeat LAUNCHPAD_LENGTH volume-button)))

; set grid buttons
(doall
  (for [x (range LAUNCHPAD_LENGTH)
        y (range LAUNCHPAD_LENGTH)]
    (insert-callback grid-button x y)))

; set special buttons
(insert-callback zoom-state-button-up
  (:x zoom-state-up-loc) (:y zoom-state-up-loc))

(insert-callback zoom-state-button-down
  (:x zoom-state-down-loc) (:y zoom-state-down-loc))

(insert-callback scene-state-left-button
  (:x scene-state-left-loc) (:y scene-state-left-loc))

(insert-callback scene-state-right-button
  (:x scene-state-right-loc) (:y scene-state-right-loc))

(insert-callback tracker-state-button
  (:x tracker-state-loc) (:y tracker-state-loc))

(insert-callback action-state-button
  (:x action-state-loc) (:y action-state-loc))

(insert-callback repeat-state-button
  (:x repeat-state-loc) (:y repeat-state-loc))

(insert-callback bank-state-button
  (:x bank-state-loc) (:y bank-state-loc))

; final ready steps
(assert-leds)
(on-grid-pressed lpad button-press)