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
	::keyboard-handler)

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

; (ctl lowpass :cutoff 10000 :res 1)
(def lowpass (inst-fx! output fx-rlpf))

; (ctl highpass :cutoff 10000 :res 1)
(def highpass (inst-fx! output fx-rhpf))


; --------------- Launchpad Stuff --------------------

;
; global definitions
;

(def LAUNCHPAD_LENGTH 8)
(def LAUNCHPAD_AREA (* LAUNCHPAD_LENGTH LAUNCHPAD_LENGTH))
(def BPM 128)
(def lpad (open))
(def m (metronome BPM))
(def storyboard-on? (atom false))
(def tracker-state (atom 1)) ; 0: paused with LED off
                             ; 1: ready with LED orange
                             ; 2: playing with LED green
(def active-action (atom 0)) ; number which designates the currently active action.


;
; utilities
;

(defn get-event-name [x y] (str x y "action"))

(defn null-callback [x y pressed?] nil)

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

(defn get-tracker-pos [beat]
  (beat->xy (mod beat LAUNCHPAD_AREA)))

; TODO: make the following fucntions able to specify a range, ie part of a line

(defn set-line
  [lpad orientation line color intensity]
  (cond 
    (= orientation "row")
    (domap #(draw-grid lpad % line color intensity) (range LAUNCHPAD_LENGTH))
    (= orientation "column")
    (domap #(draw-grid lpad line % color intensity) (range LAUNCHPAD_LENGTH))
    :else (println "ERROR: Unknown set-line orientation " orientation)))

(defn set-row
  "Sets the row of given lpad to given color, intensity defaults to :low."
  ([lpad row color] (set-row lpad row color :low))
  ([lpad row color intensity] (set-line lpad "row" row color intensity)))

(defn set-column
  "Sets the column of given lpad to given color, intensity defaults to :low."
  ([lpad column color] (set-column lpad column color :low))
  ([lpad column color intensity] (set-line lpad "column" column color intensity)))


;
; non-grid buttons
;

; TODO: turn special buttons into maps of location and callback
(def tracker-state-loc {:x 4 :y 8})


;
; led assertions
;

(defn assert-tracker-state-leds []
  (let [x (:x tracker-state-loc)
        y (:y tracker-state-loc)]
    (cond 
      (= 0 @tracker-state) (draw-grid lpad x y :off)
      (= 1 @tracker-state) (draw-grid lpad x y :orange :low)
      (= 2 @tracker-state) (draw-grid lpad x y :green :low))))

 ; TODO: make this do something
(defn assert-grid-leds []
  1)

(defn assert-action-leds []
  (let [x LAUNCHPAD_LENGTH
        on @active-action
        off (disj (set (range LAUNCHPAD_LENGTH)) on)]
    (domap #(draw-grid lpad x % :red :low) off)
    (draw-grid lpad x on :green :high)))

(defn assert-leds []
  (do 
    (assert-grid-leds)
    (assert-tracker-state-leds)
    (assert-action-leds)))


;
; actions
;

; TODO: make it so actions can be scheduled in more than 1 place. the problem
;       currently is that I have overlapping event-id's. another problem is how do
;       i maintain state as to which actions are scheduled on which beats? that seems
;       to be internal to metronome, so i don't want to double-up on work/memory...
(def stop-action
  {:id "stop"
   :callback (fn [event] (println "stop-action called!"))})

(defn action-button
  [x y pressed?]
  (if pressed?
    (do 
      (set-atom! active-action y)
      (assert-action-leds))))

(def null-action
  {:id "null"
   :callback (fn [event] (println "null-action called!"))})

(def actions
  (atom (vec (repeat LAUNCHPAD_LENGTH null-action))))

(defn insert-action
  [action i]
  (swap! actions (fn [prev] (assoc prev i action))))

(insert-action stop-action 0)

(defn get-action [i]
  (nth @actions i))

;
; special button methods
;

(defn tracker-state-button
  [x y pressed?]
  (if pressed?
    (cond 
      (= 0 @tracker-state)
        (set-atom! tracker-state 1)
      (= 1 @tracker-state)
        (set-atom! tracker-state 0)
      (= 2 @tracker-state)
        (do
          (set-atom! tracker-state 1)
          (set-atom! storyboard-on? false))))
  (assert-tracker-state-leds))


;
; core functionality
;

(defn run-tracker
  [lpad beat]
  (let [next-beat (inc beat)
       [x y] (get-tracker-pos beat)
       [prevx prevy] (get-tracker-pos (- beat 1))]
    (if @storyboard-on?   

      (do ; storyboard on stuff
        (at (m beat)
          (event (get-event-name x y)) ; trigger any scheduled actions
          (draw-grid lpad prevx prevy :off) ; turn off previous pos LED
          (draw-grid lpad x y :orange :high)) ; turn on current pos LED
          ;(quick-kick :amp 0.5))
        (apply-at (m next-beat) #'run-tracker [lpad next-beat]))

      ; if storyboard is off, just turn off the previous LED
      (draw-grid lpad prevx prevy :off))))

(defn start-storyboard
  ([] (start-storyboard 0 0))
  ([x y]
    (let [beat (xy->beat x y)]
      (do (metro-start m beat)
          (set-atom! tracker-state 2)
          (set-atom! storyboard-on? true)
          ;(reset lpad)
          (assert-leds)
          (run-tracker lpad beat)))))


;
; grid callback stuff
;

(defn grid-press
  [x y pressed?]
  (let [beat (xy->beat x y)
        tracker-ready? (= 1 @tracker-state)]
    (if pressed?

      (cond ; when button is pressed
        tracker-ready? (draw-grid lpad x y :green :high)
        (not tracker-ready?)
        (let [event-name (get-event-name x y)
              action (get-action @active-action)
              action-callback (:callback action)
              action-id (:id action)]
          (oneshot-event event-name action-callback action-id)
          (assert-grid-leds)
          (draw-grid lpad x y :green :low)))

      (cond ; when button is released
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

; set grid buttons
(doall
  (for [x (range LAUNCHPAD_LENGTH)
        y (range LAUNCHPAD_LENGTH)]
    (insert-callback grid-press x y)))

; set special buttons
(insert-callback tracker-state-button
  (:x tracker-state-loc) (:y tracker-state-loc))

; set action buttons
(domap #(insert-callback action-button LAUNCHPAD_LENGTH %) (range LAUNCHPAD_LENGTH))

(assert-leds)
(on-grid-pressed lpad button-press)