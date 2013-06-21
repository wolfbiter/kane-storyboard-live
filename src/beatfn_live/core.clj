(ns beatfn-live.core
  (:use
    [overtone.live]
    [beatfn-live.globals]
    [beatfn-live.samples]
    [beatfn-live.launchpad]
    [overtone.inst.drum :only [quick-kick haziti-clap soft-hat open-hat]]))


; --------------- Output Stuff -----------------------

(definst output [amp 1]
  (* amp (sound-in [0 1])))

;(definst output [vol1 1.0 lpf1 1.0 rq 1.0]
;  (let [vol-env1 (env-gen (adsr) :gate vol1 :action NO-ACTION)
;        lpf-env1 (env-gen (adsr) :gate lpf1 :action NO-ACTION)]
;        (rlpf (* vol-env1 (sound-in [0 1]))
;              lpf-env1
;              rq)))

(def master-volume (output))

; (ctl master-volume :vol1 1.0)
;(def master-volume (output))

; (ctl lowpass :cutoff 10000 :res 1)
(def lowpass (inst-fx! output fx-rlpf))

; (ctl highpass :cutoff 10000 :res 1)
(def highpass (inst-fx! output fx-rhpf))


; --------------- Storyboard Stuff --------------------

(def lpad (open))

;
; utilities
;

(defn get-scene-state-kw [scene] (keyword (str "scene" scene)))

(defn get-action-handle 
  [scheduled-action]
    (let [beat-event (:beat-event scheduled-action)
          name (:name scheduled-action)
          scene-state (get-scene-state-kw @scene-state)]
      (keyword (str beat-event "scene" scene-state name))))

(defn set-action
  [action i]
  (swap! active-actions (fn [prev] (assoc prev i action))))

(defn get-action
  ([] (get-action @active-action-number)) ; if no args, return active action
  ([i] (nth @active-actions i)))

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
  (let [active-action (get-action)
        scheduled-actions @scheduled-actions
        beat (xy->beat x y)
        beat-event (get-beat-event beat)
        active-scene @scene-state
        scene-state (get-scene-state-kw active-scene)
        possible-actions (scene-state (beat-event scheduled-actions))
        matching-action ((:name active-action) possible-actions)]
    (cond 
      (empty? possible-actions) (draw-grid lpad x y :off)
      (not (nil? matching-action)) (draw-grid lpad x y :green :low)
        :else (draw-grid lpad x y :red :low))))

; TODO: make this be smarter by checking only the squares with scheduled actions
; TODO: make this know tracker pos so it doesn't wipe the tracker led
; TODO: maybe make it so the other scene's scheduled events are present?
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
      (let [on @active-action-number
            off (disj (set (range LAUNCHPAD_LENGTH)) on)]
        (domap #(draw-grid lpad x % :red :low) off)
        (draw-grid lpad x on :green :high))

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
      ;(assert-grid-led x y)))) ; TODO: can this be added?

(defn make-event-fn [scheduled-action]
  (let [callback (:callback scheduled-action)
        repeat? (:repeat? scheduled-action)]
    (if repeat?
      callback
      #(do (callback %)
           (unschedule-action scheduled-action)))))

; TODO: debug why repeating scheduled actions sometimes disappear! concurrency issue?
(defn schedule-action ; scheduled actions are actions scheduled
  [action beat]       ; for a beat-event 
  (let [beat-event (get-beat-event beat)
        _scheduled-action  (assoc action :beat-event beat-event :beat beat)
        scheduled-action  (if (= 1 @repeat-state) ; make this repeat if repeat is on
                            (assoc _scheduled-action :repeat? true)
                            _scheduled-action)
        action-handle (get-action-handle scheduled-action)
        callback (make-event-fn scheduled-action)]

    ; first schedule the overtone event call
    (on-event beat-event (make-event-fn scheduled-action) action-handle)

    ; now store this scheduled action
    (swap! scheduled-actions
      (fn [prev]
        (let [scene-state (get-scene-state-kw @scene-state)
              prev-scenes (beat-event prev)
              prev-actions (scene-state prev-scenes)
              new-actions (assoc prev-actions (:name action) scheduled-action)
              new-scenes (assoc prev-scenes scene-state new-actions)]
          (assoc prev beat-event new-scenes))))))


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
      (set-atom! active-action-number y)
      (assert-bank-leds)
      (assert-grid-leds))))

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
      (do (metro-start m beat)
          (set-atom! tracker-state 2)
          (assert-tracker-state-led)
          (run-tracker lpad beat)))))


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
              active-action (get-action)
              possible-actions (scene-state (beat-event @scheduled-actions))
              matching-action ((:name active-action) possible-actions)]
          (if (nil? matching-action)
            (schedule-action active-action beat)
            (unschedule-action matching-action))

          ; TODO: test this
          (if (:init active-action) ; run this action's init if it has one
            ((:init active-action) x y pressed?))
          (assert-grid-led x y))

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

; set actions
(set-action rand-uplift4 0)
(set-action rand-uplift8 1)
(set-action rand-uplift16 2)
(set-action rand-downlift-crash 3)
(set-action rand-downlift-explode 4)
(set-action rand-downlift-fx 5)

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