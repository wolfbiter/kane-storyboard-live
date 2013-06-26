(ns beatfn-live.core
  (:use
    [overtone.live]
    [beatfn-live.globals]
    [beatfn-live.samples]
    [beatfn-live.outputs]
    [beatfn-live.launchpad]
    [overtone.inst.drum :only [quick-kick haziti-clap soft-hat open-hat]]))


; --------------- Storyboard Stuff --------------------

(def lpad (open))

;
; utilities
;

(defn get-sample-volume [] (/ @sample-volume-state 8))

(defn get-scene-state-kw [scene] (keyword (str "scene" scene)))

(defn get-action-handle
  [scheduled-action]
    (let [beat-event (:beat-event scheduled-action)
          name (:name scheduled-action)
          scene-state (get-scene-state-kw @scene-state)]
      (keyword (str beat-event "scene" scene-state name))))

(defn load-action [action i]
  (swap! loaded-actions (fn [prev] (assoc prev i action))))

(defn get-active-actions []
  (let [active-action-numbers @active-action-numbers]
   (map #(nth @loaded-actions %) active-action-numbers)))

(defn get-beat-event [beat]
  (keyword (str "beat-event" (mod beat LAUNCHPAD_AREA))))

(defn domap [& args]
  (doall (apply map args)))

(defn xy->beat [x y] (+ (* y LAUNCHPAD_LENGTH) x))

(defn beat->xy [beat]
  (let [x (mod beat LAUNCHPAD_LENGTH)
        y (/ (- beat x) LAUNCHPAD_LENGTH)]
    [x y]))

(defn set-atom!
  [atom val]
  (swap! atom (fn [x] val)))

(defn get-tracker-pos
  ([] (get-tracker-pos (m)))
  ([beat] (beat->xy (mod beat LAUNCHPAD_AREA))))

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


;
; non-grid buttons
;

; TODO: turn special buttons into maps of location and callback
(def tracker-state-loc {:x 4 :y 8})
(def repeat-state-loc {:x 5 :y 8})
(def scene-state-loc {:x 6 :y 8})
(def bank-state-loc {:x 7 :y 8})


;
; led assertions
;

(defn assert-tracker-state-led []
  (let [x (:x tracker-state-loc)
        y (:y tracker-state-loc)]
    (cond
      (= 0 @tracker-state) (draw-grid lpad x y :off)
      (= 1 @tracker-state) (draw-grid lpad x y :orange :low)
      (= 2 @tracker-state) (draw-grid lpad x y :green :low))))

(defn assert-scene-state-led []
  (let [x (:x scene-state-loc)
        y (:y scene-state-loc)]
    (cond
      (= 0 @scene-state) (draw-grid lpad x y :red :low)
      (= 1 @scene-state) (draw-grid lpad x y :orange :low))))

(defn assert-bank-state-led []
  (let [x (:x bank-state-loc)
        y (:y bank-state-loc)]
    (cond
      (= 0 @bank-state) (draw-grid lpad x y :off)
      (= 1 @bank-state) (draw-grid lpad x y :green :low)
      (= 2 @bank-state) (draw-grid lpad x y :red :low)
      (= 3 @bank-state) (draw-grid lpad x y :orange :low))))

(defn assert-repeat-state-led []
  (let [x (:x repeat-state-loc)
        y (:y repeat-state-loc)]
    (cond
      (= 0 @repeat-state) (draw-grid lpad x y :off)
      (= 1 @repeat-state) (draw-grid lpad x y :green :low))))

(defn assert-tracker-led
  ([] (apply assert-tracker-led (get-tracker-pos)))
  ([x y] (if (= 2 @tracker-state) (draw-grid lpad x y :orange :high))))

(defn assert-grid-led [x y]
  (let [active-actions (get-active-actions)
        scheduled-actions @scheduled-actions
        beat (xy->beat x y)
        beat-event (get-beat-event beat)
        active-scene @scene-state
        scene-state (get-scene-state-kw active-scene)
        possible-actions (scene-state (beat-event scheduled-actions))]

        ; if no actions are scheduled for this grid spot, turn the led off
        (if (empty? possible-actions)
          (draw-grid lpad x y :off)

          ; else check if any currently active actions are scheduled for this spot
          (let [matching-actions
                (filter #(not (nil? %))
                  (map #((:name %) possible-actions) active-actions))]

            ; if there are none, light the led red
            (if (empty? matching-actions)
             (draw-grid lpad x y :red :low)

             ; else light the led green
             (draw-grid lpad x y :green :low))))))

; TODO: make this be smarter by checking only the squares with scheduled actions
; TODO: maybe make it so the other scenes's scheduled events are present?
; TODO: should the inactive scheduled actions be red or red and orange?
(defn assert-grid-leds []
  (do
    (doall
      (for [x (range LAUNCHPAD_LENGTH)
            y (range LAUNCHPAD_LENGTH)]
        (assert-grid-led x y)))
    (assert-tracker-led)))

(defn assert-bank-leds []
  (let [x LAUNCHPAD_LENGTH
        bank-state @bank-state]
    (cond

      (= bank-state 0) ; action bank
      (let [on @active-action-numbers
            off (filter #(= -1 (.indexOf on %)) (range LAUNCHPAD_LENGTH))]
        (domap #(draw-grid lpad x % :red :low) off)
        (domap #(draw-grid lpad x % :green :high) on))

      (= bank-state 1) ; volume bank
      (do (set-column lpad LAUNCHPAD_LENGTH :off) ; first clear all
          (set-column lpad LAUNCHPAD_LENGTH :green :low (- LAUNCHPAD_LENGTH @sample-volume-state) LAUNCHPAD_LENGTH))

      (= bank-state 2) ; zoom select bank
      (domap #(draw-grid lpad x % :red :low) (range LAUNCHPAD_LENGTH))

      (= bank-state 3) ; grid editor bank
      (domap #(draw-grid lpad x % :orange :low) (range LAUNCHPAD_LENGTH)))))

(defn assert-leds []
  (assert-tracker-state-led)
  (assert-bank-state-led)
  (assert-scene-state-led)
  (assert-repeat-state-led)
  (assert-bank-leds)
  (assert-grid-leds))


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
; special button methods
;

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
        (bank-state-button x y true))))

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


;
; core functionality
;

(defn run-tracker
  [lpad beat]
  (let [next-beat (inc beat)
       storyboard-on? (= @tracker-state 2)
       [x y] (get-tracker-pos beat)
       [prevx prevy] (get-tracker-pos (- beat 1))]
    (assert-grid-led prevx prevy) ; revert previous tracker pos led

    (if storyboard-on?
      (do
        (at (m beat)
          (event (get-beat-event beat)) ; trigger any scheduled actions
          (assert-tracker-led x y)) ; turn on current pos LED
;          (quick-kick :amp 0.5))
        (apply-at (m next-beat) #'run-tracker [lpad next-beat])))))

(defn start-storyboard
  ([] (start-storyboard 0 0))
  ([x y]
    (let [beat (xy->beat x y)]
      (metro-start m beat)
      (set-atom! tracker-state 2)
      (assert-tracker-state-led)
      (run-tracker lpad beat))))


;
; grid callback stuff
;

(defn grid-press
  [x y pressed?]
  (let [beat (xy->beat x y)
        tracker-ready? (= 1 @tracker-state)
        bank-state @bank-state]
    (if pressed?

      ; if button is pressed,
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
        :else (println "HI! current bank state:" bank-state))

      ; when button is released
      (cond
        tracker-ready? (start-storyboard x y)))))
        ;:else (draw-grid lpad x y :off)))))

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
    (insert-callback grid-press x y)))

; set special buttons
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