(ns beatfn-live.buttons
  (:use
    [overtone.live]
    [beatfn-live.globals]
    [beatfn-live.tracker]
    [beatfn-live.samples]
    [beatfn-live.outputs]
    [beatfn-live.actions]
    [beatfn-live.ledAssertions]
    [beatfn-live.launchpad :only [draw-grid]]
    [beatfn-live.utilities]))

;
; special button methods
;

; TODO: make these the same button, sohould be easy with y*-1 or something
(defn zoom-state-button-up
  [x y pressed?]
    (if pressed?
      (let [scalar 0.5]
        (swap! zoom-state (partial * scalar))
        (draw-grid lpad x y :green :high)
        (assert-grid-leds))
    (assert-zoom-state-leds)))

(defn zoom-state-button-down
  [x y pressed?]
    (if pressed?
      (let [scalar 2]
        (swap! zoom-state (partial * scalar))
        (draw-grid lpad x y :red :high)
        (assert-grid-leds))
    (assert-zoom-state-leds)))

(defn bank-state-button
  [x y pressed?]
    (if pressed?
      (do
        (swap! bank-state (fn [prev] (mod (inc prev) NUM_BANKS)))
        (assert-bank-state-led)
        (assert-bank-leds))))

(defn bank-button
  [x y pressed?]
  (let [active-bank (nth @banks @bank-state)
        bank-fn (nth @active-bank y)]
    (bank-fn x y pressed?)))

(defn action-button
  [x y pressed?]

  (if pressed?
    (do
      ; first increment pressed actions and light led
      (swap! actions-pressed inc)
      ; then if another action button is pressed, add this action to active actions.
      (if (> @actions-pressed 1)
        (swap! active-action-numbers (fn [prev] (conj prev y)))
        ; else this is the only action press, so set it as active action
        (set-atom! active-action-numbers [y]))
      ; now that this action is active, assert its LED
      (assert-bank-leds))

    (do
      ; on release, first decrement pressed actions
      (swap! actions-pressed dec)
      ; then, if this is the last action being released, assert grid LEDs
      (if (= @actions-pressed 0)
        (assert-grid-leds)))))

(defn volume-button
  [x y pressed?]
  (if pressed?
    (do (set-column lpad LAUNCHPAD_LENGTH :off)
        (set-column lpad LAUNCHPAD_LENGTH :green :high y LAUNCHPAD_LENGTH))
    (do (set-atom! sample-volume-state (- LAUNCHPAD_LENGTH y))
        (set-atom! bank-state (- NUM_BANKS 1)) ; want mode 0 next
        ; TODO: debug why this happens immediately
        (at (+ (now) 1000) 
          (bank-state-button x y true)))))

(defn tracker-state-button
  [x y pressed?]
  (if pressed?
    (do
      (cond
        (= 0 @tracker-state)
          (set-atom! tracker-state 1)
        (= 1 @tracker-state)
          (set-atom! tracker-state 0)
        (= 2 @tracker-state)
          (set-atom! tracker-state 1))
      (assert-tracker-state-led))))

; TODO: make these the same button, sohould be easy with y*-1 or something
(defn scene-state-left-button
  [x y pressed?]
    (if pressed?
      (do
        (swap! scene-state dec)
        (draw-grid lpad x y :orange :high)
        (assert-grid-leds))
      (assert-scene-state-leds)))

(defn scene-state-right-button
  [x y pressed?]
    (if pressed?
      (do
        (swap! scene-state inc)
        (draw-grid lpad x y :orange :high)
        (assert-grid-leds))
      (assert-scene-state-leds)))

(defn action-state-button
  [x y pressed?]
    (if pressed?
      (do
        (swap! action-state (fn [prev] (mod (inc prev) NUM_ACTION_STATES)))
        (assert-action-state-led)
        (assert-bank-leds)
        (assert-grid-leds))))

(defn repeat-state-button
  [x y pressed?]
    (if pressed?
      (do
        (swap! repeat-state (fn [prev] (mod (inc prev) 2)))
        (assert-repeat-state-led))))

(defn grid-button
  [x y pressed?]
  (if pressed?

    ; if button is pressed,
    (let [beat (xy->beat x y)
          bank-state @bank-state]

      (cond
        ; and tracker is ready,
        (= 1 @tracker-state)
        (draw-grid lpad x y :green :high) ; light the pressed button green

        ; else, do something based on current bank
        (= bank-state 0) ; action bank
        (let [beat-event (get-beat-event beat)
              scene-state (get-scene-state-kw @scene-state)
              active-actions (get-active-actions)
              possible-actions (scene-state (beat-event @scheduled-actions))
              matching-actions
                (filter #(not (nil? %))
                  (map #((:name %) possible-actions) active-actions))]

          ; if there are no active actions scheduled here, schedule them
          (if (empty? matching-actions)
            (domap #(schedule-action % beat) active-actions)
            (do (domap unschedule-action matching-actions)
                (assert-grid-led x y))))

        ; TODO: make things for other banks!
        :else (println "HI! current bank state:" bank-state)))

    ; when button is released
    (cond
      (= 1 @tracker-state) (start-storyboard x y))))