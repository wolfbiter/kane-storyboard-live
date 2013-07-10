(ns beatfn-live.tracker
  (:use
    [overtone.live]
    [beatfn-live.globals]
    [beatfn-live.samples]
    [beatfn-live.outputs]
    [beatfn-live.ledAssertions]
    [beatfn-live.launchpad :only [draw-grid]]
    [beatfn-live.utilities]))

;
; tracker stuff
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