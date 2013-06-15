(ns beatfn-live.core
  (:use
    [overtone.live]
    [beatfn-live.launchpad]
    [overtone.inst.drum :only [quick-kick haziti-clap soft-hat open-hat]]))

(def m (atom (metronome 128)))

(def latency (atom 100))

(def raw-notes [:c3 :g3 :c3])

; TODO: turn this into a map of note and freq
(def notes (flatten (map #(list % ((comp midi->hz note) %)) [:c3 :g3 :c3])))

(on-event [:midi nil]
	(fn [e]
   (if (= (:name (:device e)) "VirMIDI [hw:4,0,1]")
    (do
     ;(println "e's name: " (:name (:device e)))
     ;(doall (map println e))
     (if (= (:status e) :start)
       (do
        (println "START")
   		  ;(doall (map println e))
  		  (println "message timestamp:" (:timestamp e))
        (println "now: " (now))
  		  ;(def m (atom (metronome 128)))
        (metro-start m 1)
  	    (println "END"))))))
	::keyboard-action)

(defn player
  [beat]
  (let [next-beat (inc beat)]
    (at (- (@m beat) @latency)
        (quick-kick :amp 0.5)
        (if (zero? (mod beat 2))
          (open-hat :amp 0.1)))
    (at (- (@m (+ 0.5 beat)) @latency)
        (haziti-clap :decay 0.05 :amp 0.3))

    (when (zero? (mod beat 3))
      (at (- (@m (+ 0.75 beat)) @latency)
          (soft-hat :decay 0.03 :amp 0.2)))

    (when (zero? (mod beat 8))
      (at (- (@m (+ 1.25 beat)) @latency)
          (soft-hat :decay 0.03)))

    (apply-at (@m next-beat) #'player [next-beat])))

; --------------- Output Stuff -----------------------

(definst dubstep [freq 100 wobble-freq 2]
  (let [sweep (lin-exp (lf-saw wobble-freq) -1 1 40 5000)
        son (mix (saw (* freq [0.99 1 1.01])))]
    (lpf son sweep)))

(def default-env (lin-exp (lf-saw 1) -1 1 40 5000))

(def freqTest (atom 1.0))

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


; --------------- Launchpad Stuff --------------------

;
; global definitions
;

(def NUM_SCENES 2) ; max of 4, check out assert-scene-state-led if this changes
(def NUM_BANKS 4) ; number of possible banks
(def LAUNCHPAD_LENGTH 8)
(def LAUNCHPAD_AREA (* LAUNCHPAD_LENGTH LAUNCHPAD_LENGTH))
(def BPM 128)
(def lpad (open))
(def m (metronome BPM))
(def scene-state (atom 0)) ; the number of the currently active scene
(def bank-state (atom 0)) ; 0 for action bank,
                          ; 1 for grid editor bank
                          ; 2 for zoom select bank
                          ; 3 unused atm

(def repeat-state (atom 0)) ; 0 for repeat off, 1 for repeat on
(def tracker-state (atom 1)) ; 0: paused with LED off
                             ; 1: ready with LED orange
                             ; 2: playing with LED green

; TODO: scrolling through active actions (implementation: scrollthrough active bank?)
; TODO: add zoom-select bank
; TODO: move scene-select to arrows
; TODO: looping. make it so that you can select an area over which to loop
; TODO: copy/paste regions. selection goes from top-left to bottom-right.
; TODO: to make scenes really awesome, they each need a crossfader "prop"!
; TODO: figure out underlying clojure-launchpad stuff to get yellow/more intensities
; TODO: need a way to specify volume of samples you are placing
; TODO: need some way to preview stuff (headphones)

(defn null-callback [x y pressed?] nil)

(def null-action
  {:name :null
   :callback (fn [event] (println "null-action called!"))})

; an endless vector of possible actions TODO: make it so this isn't finite length
(def active-actions (atom (vec (repeat LAUNCHPAD_LENGTH null-action))))

(defn make-bank []
  (do (println "Money.")
      (atom (vec (repeat LAUNCHPAD_LENGTH null-callback)))))

; a vector of action buttons
(def action-bank (make-bank))

; vector of grid editor buttons TODO: finish this, requires editing grid-press,
;                                     assert-grid-led, and writing fxns to fill this
(def grid-editor-bank (make-bank))

; vector of zoom select buttons TODO: finish this, requires editing grid-press,
;                                     assert-grid-led, and writing fxns to fill this
(def zoom-select-bank (make-bank))

; vector of null actions
(def null-bank (make-bank))

; the currently loaded bank (which is a vector of functions)
(def banks (atom [action-bank grid-editor-bank zoom-select-bank null-bank]))

; number which designates the currently active action.
(def active-action-number (atom 0))

; a map k/v'd with:
; {beat-event {scene0 {action-event1 scheduled-action1
;                      action-event2 scheduled-action2}
;              scene1 {action-event}
;}
; TODO: instead of a 3-nested map, how about 2-nested with an array for an outer key?
;       or maybe 3-nested, but with the outermost being scene? or maybe no change?
(def scheduled-actions (atom {}))


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

      (= bank-state 1) ; grid editor bank
      (domap #(draw-grid lpad x % :green :low) (range LAUNCHPAD_LENGTH))

      (= bank-state 2) ; zoom select bank
      (domap #(draw-grid lpad x % :red :low) (range LAUNCHPAD_LENGTH))

      (= bank-state 3) ; unused bank
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

(defn action-button
  [x y pressed?]
  (do
    (set-atom! active-action-number y)
    (assert-bank-leds)
    (assert-grid-leds)))

(defn bank-button
  [x y pressed?]
  (let [active-bank (nth @banks @bank-state)
        bank-fn (nth @active-bank y)]
    (bank-fn x y pressed?)))

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

(defn bank-state-button
  [x y pressed?]
    (if pressed?
      (do
        (swap! bank-state (fn [prev] (mod (inc prev) NUM_BANKS)))
        (assert-bank-state-led)
        (assert-bank-leds))))


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
          (assert-tracker-led x y) ; turn on current pos LED
          (quick-kick :amp 0.5))
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

; set bank buttons
(domap #(insert-callback bank-button LAUNCHPAD_LENGTH %) (range LAUNCHPAD_LENGTH))
(set-atom! action-bank (vec (repeat LAUNCHPAD_LENGTH action-button)))

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



;
; set actions
;

(def stop-action ; actions have a name and event callback, plus possible init fn
  {:name :stop
   :callback #(println "stop-action called!" %)})

(defn offset-sample [sample offset] ; offset is in units of milliseconds
  (fn [event]
    (let [start (m)]   ; start is in units of beats
      (at (+ (m start) offset)
          (sample)))))

(defsynth quick-washin [vol 0.4 rate 0.9]
    (let [sample-buf (load-sample "resources/uplifters/VEH2 FX - 071.wav")
          dry (play-buf 2 (buffer-id sample-buf) rate sample-buf)]
      (out [0 1] (* dry vol))))

(def quick-washin {
  :name :quick-washin
  :callback (offset-sample quick-washin 100)})

(defsynth washin1 [vol 0.4]
    (let [sample-buf (load-sample "resources/uplifters/VEH2 FX - 061.wav")
          dry (play-buf 2 sample-buf)]
      (out [0 1] (* dry vol))))

(def washin1 {
  :name :washin1
  :callback (offset-sample washin1 185)})

(defsynth washin2 [vol 0.4 rate 1.285]
    (let [sample-buf (load-sample "resources/uplifters/VEH2 FX - 067.wav")
          dry (play-buf 2 (buffer-id sample-buf) rate sample-buf)]
      (out [0 1] (* dry vol))))

(def washin2 {
  :name :washin2
  :callback washin2})

(defsynth upzip [vol 0.4 rate 1.55]
    (let [sample-buf (load-sample "resources/uplifters/VEH2 FX - 035.wav")
          dry (play-buf 2 (buffer-id sample-buf) rate sample-buf)]
      (out [0 1] (* dry vol))))

(def upzip {
  :name :upzip
  :callback upzip})

(defsynth washout1 [vol 0.35]
    (let [sample-buf (load-sample "resources/downlifters/VEH2 FX - 081.wav")
          dry (play-buf 2 sample-buf)]
      (out [0 1] (* dry vol))))

(def washout1 {
  :name :washout1
  :callback washout1})

(defsynth washout2 [vol 0.3]
    (let [sample-buf (load-sample "resources/downlifters/VEH2 FX - 084.wav")
          dry (play-buf 2 sample-buf)]
      (out [0 1] (* dry vol))))

(def washout2 {
  :name :washout2
  :callback washout2})

(defsynth dropout [vol 0.7]
    (let [sample-buf (load-sample "resources/downlifters/VEH2 FX - 009.wav")
          dry (play-buf 2 sample-buf)]
      (out [0 1] (* dry vol))))

(def dropout {
  :name :dropout
  :callback dropout})

(defsynth liquid-dropout [vol 0.15]
    (let [sample-buf (load-sample "resources/downlifters/VEH2 FX - 151.wav")
          dry (play-buf 2 sample-buf)]
      (out [0 1] (* dry vol))))

(def liquid-dropout {
  :name :liquid-dropout
  :callback liquid-dropout})

; uplifters
(set-action quick-washin 0)
(set-action washin1 1)
(set-action washin2 2)
(set-action upzip 3)

;downlifters
(set-action dropout 4)
(set-action washout1 5)
(set-action washout2 6)
(set-action liquid-dropout 7)