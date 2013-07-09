(ns beatfn-live.core
  (:use
    [overtone.live]
    [beatfn-live.globals]
    [beatfn-live.samples]
    [beatfn-live.outputs]
    [beatfn-live.ledAssertions]
    [beatfn-live.launchpad :only [open draw-grid on-grid-pressed]]
    [beatfn-live.utilities]
    [overtone.inst.drum :only [quick-kick haziti-clap soft-hat open-hat]]))


; --------------- Storyboard Stuff --------------------

;
; actions
;

; TODO: make unscheduling all actions not so dumb!
(defn unschedule-action
  ([] (domap unschedule-action @scheduled-actions)) 
  ([scheduled-action]
    (let [action-handle (get-action-handle scheduled-action)
          beat-event (:beat-event scheduled-action)]
      (remove-handler action-handle) ; unstage event
      (swap! scheduled-actions ; remove action from scheduled actions
        (fn [prev]
          (let [scene-state (get-scene-state-kw @scene-state)
                prev-scenes (beat-event prev)
                prev-actions (scene-state prev-scenes)
                new-actions (dissoc prev-actions (:name scheduled-action))
                new-scenes (assoc prev-scenes scene-state new-actions)]
            (if (empty? new-actions)
              (dissoc prev beat-event)
              (assoc prev beat-event new-scenes))))))))

(defn make-event-fn [scheduled-action]
  (let [callback (:callback scheduled-action)
        sample? (:sample? scheduled-action)
        ; hack to give samples the volume at the time of their scheduling
        callback (if sample? (callback (get-sample-volume)) callback)
        repeat? (:repeat? scheduled-action)]
    (if repeat?
      callback
      #(do (callback %)
           (unschedule-action scheduled-action)))))

; TODO: debug why repeating scheduled actions sometimes disappear! concurrency issue?
(defn schedule-action ; scheduled actions are actions scheduled
  [action _beat]       ; for a beat-event 
  (let [beat
          ; if this action is to be scheduled in advance, do so
          (if-let [advance-beats (:in-advance action)]
            (mod (- _beat advance-beats) LAUNCHPAD_AREA)
            _beat)
        [x y] (beat->xy beat)
        beat-event (get-beat-event beat)
        _scheduled-action  (assoc action :beat-event beat-event :beat beat)
        scheduled-action  (if (= 1 @repeat-state) ; make this repeat if repeat is on
                            (assoc _scheduled-action :repeat? true)
                            _scheduled-action)
        action-handle (get-action-handle scheduled-action)
        event-fn (make-event-fn scheduled-action)]

    ; first schedule the overtone event call
    (on-event beat-event event-fn action-handle)

    ; TODO: test this, and is it even necessary...?
    ; then run this aciton's init if it has one
    ;(if (:init scheduled-action)
    ;  ((:init scheduled-action) x y pressed?))

    ; now store this scheduled action
    (swap! scheduled-actions
      (fn [prev]
        (let [scene-state (get-scene-state-kw @scene-state)
              prev-scenes (beat-event prev)
              prev-actions (scene-state prev-scenes)
              new-actions (assoc prev-actions (:name action) scheduled-action)
              new-scenes (assoc prev-scenes scene-state new-actions)]
          (assoc prev beat-event new-scenes))))

    ; and finally, assert the LED of this newly scheduled action
    (assert-grid-led x y)))


;
; core functionality
;

(defn run-tracker
  [lpad raw-beat]
  (let [next-raw-beat (+ raw-beat MIN_STEP)
       storyboard-on? (= @tracker-state 2)]

    ; TODO: move this if check elsewhere so it's not a bottleneck
    (if storyboard-on?
      (apply-by (m next-raw-beat) #'run-tracker [lpad next-raw-beat]))

    (apply assert-tracker-led (beat->xy raw-beat))
    (event (get-beat-event raw-beat))))

;    ; TODO: make this work correctly for things of less than a beat per button.
;    ; IDEA: https://github.com/overtone/overtone/blob/master/src/overtone/examples/timing/internal_metro.clj#L37
;    (domap
;      #(let [beat (+ raw-beat %)]
;        (apply-at (m beat) #'assert-tracker-led)
;        (at (m beat)
;          ;(quick-kick :amp 0.5)
;          ;(tracker-test :beat beat)))
;          ;(event :assert-tracker-led)
;          (event (get-beat-event beat)))) ; trigger any scheduled actions
;      (range 0 1 MIN_STEP))))

;(defsynth tracker-test [beat 0]
;  (event (get-beat-event beat))
;  (assert-tracker-led (beat->xy beat)))
;
;(defsynth step-tracker-synth [c-bus 0 rate 2 reset 0]
;  (let [trigger (impulse:kr rate)
;        count (stepper:kr trigger :min 0 :max 1 :reset reset)]
;    (send-trig:kr trigger count)
;    (out:kr c-bus trigger)))
;
;(on-sync-event
;  "/tr"
;  (fn [event]
;    (let [whole-beat (m)
;          beat-fraction (/ (nth (:args event) 1) 4)
;          beat (+ whole-beat beat-fraction)
;          [x y] (beat->xy beat)]
;          ;storyboard-on? (= @tracker-state 2)]
;      ;(if storyboard-on?
;        ;(do
;          (event (get-beat-event beat))
;
;          (draw-grid lpad x y :orange :high)
;          ;(draw-grid lpad (- x 1) (- y 1) :orange :high)
;          ;(assert-tracker-led (beat->xy beat))))
;          (println "beat: " whole-beat " fraction: " beat-fraction)))
;  ::tracker)
;
;(def step-tracker (step-tracker-synth))

; TODO: debug why the start is 1 beat off and feels wrong
(defn start-storyboard
  ([] (start-storyboard 0 0))
  ([x y]
    (let [beat (xy->beat x y)]
      ;(ctl step-tracker :reset 0)
      (metro-start m (- beat 1))
      ;(at (m (+ (m) 1)) (ctl step-tracker :reset 1))
      (set-atom! tracker-state 2)
      (assert-tracker-state-led)
      (run-tracker lpad beat))))


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
        (assert-grid-leds)))
    (assert-zoom-state-leds))

(defn zoom-state-button-down
  [x y pressed?]
    (if pressed?
      (let [scalar 2]
        (swap! zoom-state (partial * scalar))
        (draw-grid lpad x y :red :high)
        (assert-grid-leds)))
    (assert-zoom-state-leds))

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
      (assert-tracker-led x y)
      (assert-tracker-state-led))))

(defn scene-state-button
  [x y pressed?]
    (if pressed?
      (do
        (swap! scene-state (fn [prev] (mod (inc prev) NUM_SCENES)))
        (assert-scene-state-led)
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
          tracker-ready? (= 1 @tracker-state)
          bank-state @bank-state]

      (cond
        ; and tracker is ready,
        tracker-ready?
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
      tracker-ready? (start-storyboard x y))))


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

; load actions
(load-action rand-uplift4 0)
(load-action rand-uplift8 1)
(load-action rand-uplift16 2)
(load-action rand-downlift-crash 3)
(load-action rand-downlift-explode 4)
(load-action rand-downlift-fx 5)
(load-action toggle-deck1 6)
(load-action toggle-deck2 7)

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

(insert-callback tracker-state-button
  (:x tracker-state-loc) (:y tracker-state-loc))

(insert-callback scene-state-button
  (:x scene-state-loc) (:y scene-state-loc))

(insert-callback repeat-state-button
  (:x repeat-state-loc) (:y repeat-state-loc))

(insert-callback bank-state-button
  (:x bank-state-loc) (:y bank-state-loc))

; final ready steps
(assert-leds)
(on-grid-pressed lpad button-press)