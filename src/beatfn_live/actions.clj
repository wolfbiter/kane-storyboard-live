(ns beatfn-live.actions
  (:use
    [overtone.live]
    [beatfn-live.globals]
    [beatfn-live.samples]
    [beatfn-live.outputs]
    [beatfn-live.ledAssertions]
    [beatfn-live.launchpad :only [draw-grid on-grid-pressed]]
    [beatfn-live.utilities]
    [overtone.inst.drum :only [quick-kick haziti-clap soft-hat open-hat]]))

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
          (let [scene-state (get-scene-state-kw (:scene-state scheduled-action))
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

(defn schedule-action
  ([action _beat] (schedule-action action _beat false))
  ([action _beat checked-loop?]

    ; first, make sure this action loops properly based on current zoom
    (if (and (not checked-loop?) (< @zoom-state MAX_ZOOM))
      (let [step (/ @zoom-state MAX_ZOOM)]
        (println "step: " step)
        (domap #(schedule-action action (+ _beat (* LAUNCHPAD_AREA %)) true) (range step MAX_ZOOM step))))

    ; now schedule the action
    (let [beat
            ; if this action is to be scheduled in advance, do so
            (if-let [advance-beats (:in-advance action)]
              (mod (- _beat advance-beats) LAUNCHPAD_AREA)
              _beat)
          [x y] (beat->xy beat)
          beat-event (get-beat-event beat)
          _scheduled-action  (assoc action :beat-event beat-event :beat beat :scene-state @scene-state)
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
      (assert-grid-led x y))))
