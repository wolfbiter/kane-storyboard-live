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
(def LAUNCHPAD_SIZE (* LAUNCHPAD_LENGTH LAUNCHPAD_LENGTH))
(def BPM 128)
(def lpad (open))
(def m (metronome BPM))
(def storyboard-on? (atom false))
(def tracker-state (atom 1)) ; 0: paused with LED off
                             ; 1: ready with LED orange
                             ; 2: playing with LED green
(def active-action (atom 0)) ; number which designates the currently active action.
(def )

(def )


;
; utilities
;

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
  (beat->xy (mod beat LAUNCHPAD_SIZE)))

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
; core functionality
;

(defn run-leds
  [lpad beat]
  (let [next-beat (inc beat)
       [x y] (get-tracker-pos beat)
       [prevx prevy] (get-tracker-pos (- beat 1))]
    (at (m beat)
        (draw-grid lpad prevx prevy :off)
        (draw-grid lpad x y :orange :high)
        (quick-kick :amp 0.5))
    (if @storyboard-on?
      (apply-at (m next-beat) #'run-leds [lpad next-beat]))))

(defn start-storyboard
  [x y]
  (let [beat (- (xy->beat x y) 1)]
    (do (metro-start m beat)
        (set-atom! tracker-state 2)
        (set-atom! storyboard-on? true)
        (reset lpad)
        (assert-leds)
        (draw-grid lpad 4 8 :green :low)
        (run-leds lpad (m)))))


;
; non-grid button locations
;

; special buttons
(def tracker-state-loc {:x 4 :y 8})

; actions
(def stop-track-loc 8)


;
; led assertions
;

(defn assert-tracker-state []
  (let [x (:x tracker-state-loc)
        y (:y tracker-state-loc)]
    (cond 
      (= 0 @tracker-state) (draw-grid lpad x y :off)
      (= 1 @tracker-state) (draw-grid lpad x y :orange :low)
      (= 2 @tracker-state) (draw-grid lpad x y :green :low))))

(defn assert-action-leds []
  (let [x LAUNCHPAD_LENGTH
        on @active-action
        off (disj (set (range LAUNCHPAD_LENGTH)) on)]
    (domap #(draw-grid lpad x % :red :low) off)
    (draw-grid lpad x on :green :high)))

(defn assert-leds []
  (do 
    (assert-tracker-state)
    (assert-action-leds)))


;
; action button methods
;

; TODO: write stuff such that a grid press will set its state variable, and
;       will call from an array the appropriate function.
;       example array: [ stop-action lpf-action ... ].
;       This also makes for easy swapping / paging, sweet.
(def)

(defn stop-action
  [beat]
  (if pressed?
    (do
      (set-atom! active-action y)
      (assert-action-leds))))

(defn action-button
  [x y pressed?]
  (if pressed?
    (do 
      (set-atom! active-action y)
      (assert-action-leds))))



((get-callback @callbacks x y) x y pressed?)))

  (atom (vec (replicate area null-callback)))))

(defn grid-press
  [x y pressed?]
  (let [beat (xy->beat x y)]
    (if pressed?
      (cond
        ((at (m beat) 
          (println "hi, beat: " beat))
        :else (draw-grid lpad x y :green :high))
      (cond
        (= 1 @tracker-state) (start-storyboard x y)
        :else (draw-grid lpad x y :off)))))

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
  (assert-tracker-state))


;
; grid callback stuff
;

(def callbacks
  (let [length (+ LAUNCHPAD_SIZE 1)
        area (* length length)]
  (atom (vec (replicate area null-callback)))))

; quick indexing hack to account for the launchpad's function buttons
(defn i->xy [i]
  (let [length (+ 1 LAUNCHPAD_LENGTH)
        x (mod i length)
        y (/ (- i x) length)]
    [x y]))

(defn xy->i [x y]
  (+ (* y (+ LAUNCHPAD_LENGTH 1)) x))

(defn insert-callback [callback x y]
  (swap!
    callbacks
    (fn [prev] (assoc prev (xy->i x y) callback))))

(defn get-callback [callbacks x y]
  (nth callbacks (xy->i x y)))

(defn button-press
  [x y pressed?]
  (do
    (println "x: " x " y: " y " pressed: " pressed?)
    ((get-callback @callbacks x y) x y pressed?)))
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
(insert-callback stop-button
  (:x stop-button-loc) (:y stop-button-loc))

(domap #(insert-callback null-action 8 %) (range 8))

(assert-leds)
(on-grid-pressed lpad button-press)