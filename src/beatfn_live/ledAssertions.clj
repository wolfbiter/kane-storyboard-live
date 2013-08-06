(ns beatfn-live.ledAssertions
  (:use
    [beatfn-live.globals]
    [beatfn-live.outputs]
    [beatfn-live.utilities]
    [beatfn-live.actionDB]
    [beatfn-live.launchpad :only [draw-grid]]))

;
; led assertions
;

(defn assert-tracker-state-led []
  (let [x (:x tracker-state-loc)
        y (:y tracker-state-loc)]
    (cond
      (= 0 @tracker-state) (draw-grid lpad x y :off)
      (= 1 @tracker-state) (draw-grid lpad x y :orange :high)
      (= 2 @tracker-state) (draw-grid lpad x y :green :low))))

(defn assert-scene-state-leds []
  (let [x1 (:x scene-state-left-loc)
        y1 (:y scene-state-left-loc)
        x2 (:x scene-state-right-loc)
        y2 (:y scene-state-right-loc)]

    ; color arrows green to indicate we are in scene 0
    ;(if (= @scene-state 0)
    ;  (do
    ;    (draw-grid lpad x1 y1 :green :low)
    ;    (draw-grid lpad x2 y2 :green :low))

      ; TODO: add more colors for more differentiation?
      ;       with 2 arrows and 4 colors apiece, i count 4^2 configurations
      ; else, color them orange
        (if (<= @scene-state 0)
          (draw-grid lpad x1 y1 :off)
          (draw-grid lpad x1 y1 :orange :low))
        (if (>= @scene-state 3)
          (draw-grid lpad x2 y2 :off)
          (draw-grid lpad x2 y2 :orange :low))))

(defn assert-action-state-led []
  (let [x (:x action-state-loc)
        y (:y action-state-loc)]
    (cond
      (= 0 @action-state) (draw-grid lpad x y :off :low)
      (= 1 @action-state) (draw-grid lpad x y :green :low)
      (= 2 @action-state) (draw-grid lpad x y :red :low)
      (= 3 @action-state) (draw-grid lpad x y :orange :low))))

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
      ;(= 2 @repeat-state) (draw-grid lpad x y :red :low))))

(defn assert-zoom-state-leds []
  (let [x1 (:x zoom-state-up-loc)
        y1 (:y zoom-state-up-loc)
        x2 (:x zoom-state-down-loc)
        y2 (:y zoom-state-down-loc)]

    (if (<= @zoom-state (/ 1 4))
      (draw-grid lpad x1 y1 :off)
      (draw-grid lpad x1 y1 :green :low))

    (if (>= @zoom-state 1)
      (draw-grid lpad x2 y2 :off)
      (draw-grid lpad x2 y2 :red :low))))

(defn assert-grid-led [x y]
  (let [[possible-actions matching-actions] (get-matching-actions @scene-state (xy->beat x y))]
    (cond
      (empty? possible-actions) (draw-grid lpad x y :off)
      (empty? matching-actions) (draw-grid lpad x y :red :low)
      :else (draw-grid lpad x y :green :low))))

(defn assert-tracker-led [raw-beat]
  (let [[x y] (beat->xy raw-beat)]
    (cond

      ; if given beat is whole and tracker is running
      (and (== raw-beat (int raw-beat))
           (= 2 @tracker-state))
      (draw-grid lpad x y :green :high)

      ; if tracker is running
      (= 2 @tracker-state)
      (draw-grid lpad x y :orange :low)

      :else
      (draw-grid lpad x y :off))

    ; after sorting out the current LED, figure out prev LED
    (let [[prevx prevy] (prev-grid-pos x y)]
      (assert-grid-led prevx prevy))))

; TODO: make this be smarter by checking only the squares with scheduled actions
; TODO: maybe make it so the other scenes's scheduled events are present?
; TODO: should the inactive scheduled actions be red or red and orange?
(defn assert-grid-leds []
  (do
    (doall
      (for [x (range LAUNCHPAD_LENGTH)
            y (range LAUNCHPAD_LENGTH)]
        (assert-grid-led x y)))))

(defn assert-bank-leds []
  (let [x LAUNCHPAD_LENGTH
        bank-state @bank-state]
    (cond

      (= bank-state 0) ; action bank
      (let [green @active-action-numbers
            red (map :bank-pos
              (filter #(= @action-state (:action-state %))
                (find-actions {:scene-state @scene-state})))
            green-red (flatten (conj green red))
            off (filter #(= -1 (.indexOf green-red %)) (range LAUNCHPAD_LENGTH))]
        (domap #(draw-grid lpad x % :off) off)
        (domap #(draw-grid lpad x % :red :low) red)
        (domap #(draw-grid lpad x % :green :high) green))

      (= bank-state 1) ; volume bank
      (do (set-column lpad LAUNCHPAD_LENGTH :off) ; first clear all
          (set-column lpad LAUNCHPAD_LENGTH :green :low (- LAUNCHPAD_LENGTH @sample-volume-state) LAUNCHPAD_LENGTH))

      (= bank-state 2) ; zoom select bank
      (domap #(draw-grid lpad x % :red :low) (range LAUNCHPAD_LENGTH))

      (= bank-state 3) ; grid editor bank
      (domap #(draw-grid lpad x % :orange :low) (range LAUNCHPAD_LENGTH)))))

(defn assert-leds []
  (assert-tracker-state-led)
  (assert-zoom-state-leds)
  (assert-bank-state-led)
  (assert-scene-state-leds)
  (assert-action-state-led)
  (assert-repeat-state-led)
  (assert-bank-leds)
  (assert-grid-leds))