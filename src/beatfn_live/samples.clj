(ns beatfn-live.samples
  (:use
    [beatfn-live.metronome]
    [overtone.live :only [load-sample play-buf buffer-id out defsynth at]]))

;
; define samples
;

(defsynth play-sample [id 0 vol 0.4 rate 0.9]
    (let [dry (play-buf 2 id rate)]
      (out [0 1] (* dry vol))))

; TODO: bundle this into the play-buf function, check odoc
(defn offset-sample [sample offset] ; offset is in units of milliseconds
  (fn [event]
    (let [start (m)]   ; start is in units of beats
      (at (+ (m start) offset)
          (sample)))))

(defn make-sample
  ([uri vol] (make-sample uri vol 0 1))
  ([uri vol offset rate]
  (let [sample-buf (load-sample uri)
        id (buffer-id sample-buf)
        sample #(play-sample id vol rate)]
    (if (= 0 offset)
      (fn [event] (sample))
      (offset-sample sample offset)))))

(defn get-rand-action [actions]
  #((rand-nth actions) %))

; 4-beat uplifters
(def uplift4-1 (make-sample "resources/uplifters/VEH2 FX - 071.wav" 0.4 100 0.9))

(def rand-uplift4 {
  :name :rand-uplift4
  :callback (get-rand-action [uplift4-1])
  })

; 8-beat uplifters
(def uplift8-1 (make-sample "resources/uplifters/VEH2 FX - 061.wav" 0.4 185 1))
(def uplift8-2 (make-sample "resources/uplifters/VEH2 FX - 067.wav" 0.4 0 1.285))

(def rand-uplift8 {
  :name :rand-uplift8
  :callback (get-rand-action [
    uplift8-1
    uplift8-2
    ])
  })

; 16-beat uplifters
(def uplift16-1 (make-sample "resources/uplifters/VEH2 FX - 035.wav" 0.3 0 1.547))

(def rand-uplift16 {
  :name :rand-uplift16
  :callback (get-rand-action [uplift16-1])
  })

; crash downlifters
(def downlift-crash-1
  (make-sample "resources/downlifters/VEH2 FX - 081.wav" 0.35 0 1))
(def downlift-crash-2
  (make-sample "resources/downlifters/VEH2 FX - 084.wav" 0.3 0 1))

(def rand-downlift-crash {
  :name :rand-downlift-crash
  :callback (get-rand-action [
    downlift-crash-1
    downlift-crash-2
    ])
  })

; bass / mid downlifters
(def downlift-explode-1
  (make-sample "resources/explodes/VEH2 FX - 009.wav" 0.7 0 1))
(def downlift-explode-2
  (make-sample "resources/explodes/VEH2 FX - 065.wav" 0.6 0 1))

(def rand-downlift-explode {
  :name :rand-downlift-explode
  :callback (get-rand-action [
    downlift-explode-1
    downlift-explode-2
    ])
  })

; fx downlifters
(def downlift-fx-1
  (make-sample "resources/downlifters/VEH2 FX - 151.wav" 0.15 0 1))

(def rand-downlift-fx {
  :name :rand-downlift-fx
  :callback (get-rand-action [
    downlift-fx-1
    ])
  })
