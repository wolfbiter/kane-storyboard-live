(ns beatfn-live.actions
  (:use
    [overtone.live :only [remove-handler on-event]]
    [beatfn-live.actionDB]
    [beatfn-live.globals]
    [beatfn-live.outputs]
    [beatfn-live.utilities]
    [beatfn-live.ledAssertions :only [assert-grid-led assert-grid-leds assert-bank-leds]]))

;
; actions
;

(defn unschedule-action
  ([] (do (domap unschedule-action (find-actions))
          (assert-grid-leds)))
  ([scheduled-action]
    (let [action-handle (get-action-handle scheduled-action)]
      ; first unschedule the overtone event call
      (remove-handler action-handle)
      ; then remove any matches of the given action
      (remove-actions (dissoc scheduled-action :callback))
      (assert-bank-leds))))

(defn make-event-fn [scheduled-action]
  (let [callback (:callback scheduled-action)
        sample? (:sample? scheduled-action)
        ; hack to give samples the volume at the time of their scheduling
        callback (if sample? (callback (get-sample-volume)) callback)
        repeat? (== 1 @repeat-state)]
    (if repeat?
      callback
      #(do (callback %)
           (unschedule-action scheduled-action)))))

(defn schedule-action
  ([action _beat] (schedule-action action _beat false))
  ([action _beat checked-loop?]

    ; first, make sure this action loops properly based on current zoom
    ;(if (and (not checked-loop?) (< @zoom-state MAX_ZOOM))
    ;  (let [step (/ @zoom-state MAX_ZOOM)]
    ;    (println "step: " step)
    ;    (domap #(schedule-action action (+ _beat (* LAUNCHPAD_AREA %)) true) (range step MAX_ZOOM step))))

    ; make the scheduled action
    (let [beat
            ; hack to schedule actions in advance
            (if-let [advance-beats (:in-advance action)]
              (mod (- _beat advance-beats) LAUNCHPAD_AREA)
              _beat)
          [x y] (beat->xy beat)
          beat-event (get-beat-event beat)
          scheduled-action (assoc action :beat-event beat-event :beat beat :scene-state @scene-state)
          action-handle (get-action-handle scheduled-action)
          event-fn (make-event-fn scheduled-action)]

      ; schedule the overtone event call
      (on-event beat-event event-fn action-handle)

      ; TODO: test this, and is it even necessary...?
      ; then run this aciton's init if it has one
      ;(if (:init scheduled-action)
      ;  ((:init scheduled-action) x y pressed?))

      ; now store this scheduled action
      (insert-action (dissoc scheduled-action :callback))

      ; then assert the LED of this newly scheduled action
      (assert-grid-led x y))))
