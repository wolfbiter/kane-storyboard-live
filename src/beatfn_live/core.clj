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

; (ctl lowpass :cutoff 10000 :res 1)
(def lowpass (inst-fx! output fx-rlpf))

; (ctl highpass :cutoff 10000 :res 1)
(def highpass (inst-fx! output fx-rhpf))


; --------------- Launchpad Stuff --------------------

;
; global definitions
;

(def NUM_DECKS 2) ; max of 4, check out assert-deck-state-led and
                  ; assert-grid-led whenever you change this number!!!
(def LAUNCHPAD_LENGTH 8)
(def LAUNCHPAD_AREA (* LAUNCHPAD_LENGTH LAUNCHPAD_LENGTH))
(def BPM 128)
(def lpad (open))
(def m (metronome BPM))
(def deck-state (atom 0)) ; the number of the currently active deck
(def repeat-state (atom 0)) ; 0 for repeat off, 1 for repeat on
(def tracker-state (atom 1)) ; 0: paused with LED off
                             ; 1: ready with LED orange
                             ; 2: playing with LED green

; TODO: looping. make it so that you can select an area over which to loop
; TODO: copy/paste regions. selection goes from top-left to bottom-right.
; TODO: turn "decks" into "scenes". to make this really awesome, each scene needs
;       a crossfader "prop"
; TODO: instead of a 3-nested map, how about 2-nested with an array for an outer key?
;       or maybe 3-nested, but with the outermost being scene?
; TODO: figure out underlying clojure-launchpad stuff to get yellow/more intensities

(def null-action
  {:name :null
   :callback (fn [event] (println "null-action called!"))})

; vector of actions which are currently loaded
(def action-bank (atom (vec (repeat LAUNCHPAD_LENGTH null-action))))

; number which designates the currently active action.
(def active-action-number (atom 0))

; a map k/v'd with:
; {beat-event {deck0 {action-event1 scheduled-action1
;                    action-event2 scheduled-action2}
;              deck1 {action-event}
;}
(def scheduled-actions (atom {}))


;
; utilities
;

(defn get-deck-state-kw [deck] (keyword (str "deck" deck)))

(defn get-action-handle 
  [scheduled-action]
    (let [beat-event (:beat-event scheduled-action)
          name (:name scheduled-action)
          deck-state (get-deck-state-kw @deck-state)]
      (keyword (str beat-event "deck" deck-state name))))

(defn set-action
  [action i]
  (swap! action-bank (fn [prev] (assoc prev i action))))

(defn get-action
  ([] (get-action @active-action-number)) ; if no args, return active action
  ([i] (nth @action-bank i)))

(defn get-beat-event [beat]
  (keyword (str "beat-event" (mod beat LAUNCHPAD_AREA))))

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
(def deck-state-loc {:x 7 :y 8})
(def repeat-state-loc {:x 5 :y 8})


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

(defn assert-deck-state-led []
  (let [x (:x deck-state-loc)
        y (:y deck-state-loc)]
    (cond
      (= 0 @deck-state) (draw-grid lpad x y :red :low)
      (= 1 @deck-state) (draw-grid lpad x y :orange :low))))

(defn assert-repeat-state-led []
  (let [x (:x repeat-state-loc)
        y (:y repeat-state-loc)]
    (cond
      (= 0 @repeat-state) (draw-grid lpad x y :off)
      (= 1 @repeat-state) (draw-grid lpad x y :green :low))))

(defn assert-grid-led
  ([x y] (assert-grid-led x y (get-action) @scheduled-actions))
  ([x y active-action scheduled-actions]
    (let [beat (xy->beat x y)
          beat-event (get-beat-event beat)
          active-deck @deck-state
          deck-state (get-deck-state-kw @deck-state)
          possible-actions (deck-state (beat-event scheduled-actions))
          matching-action ((:name active-action) possible-actions)
          left-deck? (= 0 active-deck)]
      (cond 
        (empty? possible-actions) (draw-grid lpad x y :off)
        (not (nil? matching-action)) (draw-grid lpad x y :green :low)
        :else (draw-grid lpad x y (if left-deck? :red :orange) :low)))))

; TODO: make this be smarter by checking only the squares with scheduled actions
; TODO: make this know tracker pos so it doesn't wipe the tracker led
; TODO: maybe make it so the other deck's scheduled events are present?
; TODO: should the inactive scheduled actions be red or red and orange?
(defn assert-grid-leds []
  (let [active-action (get-action)
        scheduled-actions @scheduled-actions]
    (doall
      (for [y (range LAUNCHPAD_LENGTH)
            x (range LAUNCHPAD_LENGTH)]
        (assert-grid-led x y active-action scheduled-actions)))))

(defn assert-action-leds []
  (let [x LAUNCHPAD_LENGTH
        on @active-action-number
        off (disj (set (range LAUNCHPAD_LENGTH)) on)]
    (domap #(draw-grid lpad x % :red :low) off)
    (draw-grid lpad x on :green :high)))

(defn assert-leds []
  (assert-tracker-state-led)
  (assert-deck-state-led)
  (assert-repeat-state-led)
  (assert-action-leds)
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
          (let [deck-state (get-deck-state-kw @deck-state)
                prev-decks (beat-event prev)
                prev-actions (deck-state prev-decks)
                new-actions (dissoc prev-actions (:name scheduled-action))
                new-decks (assoc prev-decks deck-state new-actions)]
            (if (empty? new-actions)
              (dissoc prev beat-event)
              (assoc prev beat-event new-decks))))))))
      ;(assert-grid-led x y)))) ; TODO: can this be added?

(defn make-event-fn [scheduled-action]
  (let [callback (:callback scheduled-action)
        repeat? (:repeat? scheduled-action)]
    (if repeat?
      callback
      #(do (callback %)
           (unschedule-action scheduled-action)))))

(defn schedule-action ; scheduled actions are actions scheduled
  [action beat]       ; for a beat-event 
  (let [beat-event (get-beat-event beat)
        scheduled-action  (assoc action :beat-event beat-event :beat beat)
        scheduled-action  (if (= 1 @repeat-state) ; make this repeat if repeat is on
                            (assoc scheduled-action :repeat? true)
                            scheduled-action)
        action-handle (get-action-handle scheduled-action)
        callback (make-event-fn scheduled-action)]

    ; first schedule the overtone event call
    (on-event beat-event (make-event-fn scheduled-action) action-handle)

    ; now store this scheduled action
    (swap! scheduled-actions
      (fn [prev]
        (let [deck-state (get-deck-state-kw @deck-state)
              prev-decks (beat-event prev)
              prev-actions (deck-state prev-decks)
              new-actions (assoc prev-actions (:name action) scheduled-action)
              new-decks (assoc prev-decks deck-state new-actions)]
          (assoc prev beat-event new-decks))))))

(defn action-button
  [x y pressed?]
  (if pressed?
    (do
      (set-atom! active-action-number y)
      (assert-action-leds)
      (assert-grid-leds))))

(def stop-action ; actions have a name and event callback
  {:name :stop
   :callback (fn [event] (println "stop-action called!"))})


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
          (set-atom! tracker-state 1)))
  (assert-tracker-state-led))

(defn deck-state-button
  [x y pressed?]
  (do 
    (if pressed?
      (swap! deck-state (fn [prev] (mod (inc prev) NUM_DECKS))))
    (assert-deck-state-led)
    (assert-grid-leds)))

(defn repeat-state-button
  [x y pressed?]
  (do 
    (if pressed?
      (swap! repeat-state (fn [prev] (mod (inc prev) 2))))
    (assert-repeat-state-led)))


;
; core functionality
;

(defn run-tracker
  [lpad beat]
  (let [next-beat (inc beat)
       [x y] (get-tracker-pos beat)
       storyboard-on? (= @tracker-state 2)
       [prevx prevy] (get-tracker-pos (- beat 1))]
    (assert-grid-led prevx prevy)

    (if storyboard-on?
      (do
        (at (m beat)
          (event (get-beat-event beat)) ; trigger any scheduled actions
          (draw-grid lpad x y :orange :high)) ; turn on current pos LED
          ;(quick-kick :amp 0.5))
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
        tracker-ready? (= 1 @tracker-state)]
    (if pressed?

      (cond ; when button is pressed
        tracker-ready? (draw-grid lpad x y :green :high)
        (not tracker-ready?)
          (let [beat-event (get-beat-event beat)
                deck-state (get-deck-state-kw @deck-state)
                active-action (get-action)
                possible-actions (deck-state (beat-event @scheduled-actions))
                matching-action ((:name active-action) possible-actions)]
            (if (nil? matching-action)
              (schedule-action active-action beat)
              (unschedule-action matching-action))
            (assert-grid-led x y)))


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

; set actions
(set-action stop-action 0)


; set grid buttons
(doall
  (for [x (range LAUNCHPAD_LENGTH)
        y (range LAUNCHPAD_LENGTH)]
    (insert-callback grid-press x y)))

; set special buttons
(insert-callback tracker-state-button
  (:x tracker-state-loc) (:y tracker-state-loc))

(insert-callback deck-state-button
  (:x deck-state-loc) (:y deck-state-loc))

(insert-callback repeat-state-button
  (:x repeat-state-loc) (:y repeat-state-loc))

; set action buttons
(domap #(insert-callback action-button LAUNCHPAD_LENGTH %) (range LAUNCHPAD_LENGTH))

(assert-leds)
(on-grid-pressed lpad button-press)