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
      (apply-at (m next-raw-beat) #'run-tracker [lpad next-raw-beat]))

    (assert-tracker-led raw-beat)
    (event (get-beat-event raw-beat))))

(defn start-storyboard
  ([] (start-storyboard 0 0))
  ([x y]
    (let [beat (xy->beat x y)]
      ;(ctl step-tracker :reset 0)
      (metro-start m beat)
      ;(at (m (+ (m) 1)) (ctl step-tracker :reset 1))
      (set-atom! tracker-state 2)
      (assert-tracker-state-led)
      (run-tracker lpad beat))))