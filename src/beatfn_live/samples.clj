(ns beatfn-live.samples
  (:use
    [beatfn-live.globals]
    [beatfn-live.outputs]
    [overtone.live :only [load-sample play-buf buffer-id out definst at ctl]]))

;
; utility methods
;

(definst play-sample [id 0 vol 1 rate 1]
    (let [dry (play-buf 2 id rate)]
      (out 0 (* dry vol))))

; TODO: bundle this into the play-buf function, check odoc
(defn offset-sample [sample offset] ; offset is in units of milliseconds
  (fn [event vol]
    (let [start (m)]   ; start is in units of beats
      (at (+ (m start) offset)
          (sample vol)))))

(defn make-sample
  ([uri vol] (make-sample uri vol 0 1))
  ([uri vol offset rate]
  (let [sample-buf (load-sample uri)
        id (buffer-id sample-buf)
        sample #(play-sample id (* vol %) rate)]
    (if (== 0 offset)
      (fn [event vol] (sample vol))
      (offset-sample sample offset)))))

(defn do-rand-action [actions]
  (fn [vol] #((rand-nth actions) % vol)))

;
; define actions
;

(def toggle-deck1 {
  :name "toggle-deck1"
  :callback (fn [event]
    (swap! deck-volumes (fn [prev-vols]
      (let [old-vol (nth prev-vols 0)
            new-vol (mod (+ old-vol 1) 2)]
        (ctl deck-outputs :deck1 new-vol)
        (assoc prev-vols 0 new-vol)))))})

(def toggle-deck2 {
  :name "toggle-deck2"
  :callback (fn [event]
    (swap! deck-volumes (fn [prev-vols]
      (let [old-vol (nth prev-vols 1)
            new-vol (mod (+ old-vol 1) 2)]
        (ctl deck-outputs :deck2 new-vol)
        (assoc prev-vols 1 new-vol)))))})

;
; define samples
;

; 4-beat uplifters
(def rand-uplift4 {
  :name "rand-uplift4"
  :sample? true
  :in-advance 4
  :callback (do-rand-action [

    (make-sample "resources/uplifters/4beat/1.wav" 0.6 100 0.9)

    ])
  })

; 8-beat uplifters
(def rand-uplift8 {
  :name "rand-uplift8"
  :sample? true
  :in-advance 8
  :callback (do-rand-action [

    (make-sample "resources/uplifters/8beat/1.wav" 0.5 185 1)
    (make-sample "resources/uplifters/8beat/2.wav" 0.5 0 1.285)

    ])
  })

; 16-beat uplifters
(def rand-uplift16 {
  :name "rand-uplift16"
  :sample? true
  :in-advance 16
  :callback (do-rand-action [

    (make-sample "resources/uplifters/16beat/1.wav" 0.3 0 1.548)

    ])
  })

; crash downlifters
(def rand-downlift-crash {
  :name "rand-downlift-crash"
  :sample? true
  :callback (do-rand-action [

    (make-sample "resources/downlifters/crash/1.wav" 0.5)
    (make-sample "resources/downlifters/crash/2.wav" 0.4)
    (make-sample "resources/downlifters/crash/3.wav" 0.5)
    (make-sample "resources/downlifters/crash/4.wav" 0.5)
    (make-sample "resources/downlifters/crash/5.wav" 0.5)
    (make-sample "resources/downlifters/crash/6.wav" 0.5)
    (make-sample "resources/downlifters/crash/7.wav" 0.5)
    (make-sample "resources/downlifters/crash/8.wav" 0.5)
    (make-sample "resources/downlifters/crash/9.wav" 0.45)
    (make-sample "resources/downlifters/crash/10.wav" 0.5)
    (make-sample "resources/downlifters/crash/11.wav" 0.5)
    (make-sample "resources/downlifters/crash/12.wav" 0.45)
    (make-sample "resources/downlifters/crash/13.wav" 0.5)

    ])
  })

; explode downlifters
(def rand-downlift-explode {
  :name "rand-downlift-explode"
  :sample? true
  :callback (do-rand-action [

    (make-sample "resources/downlifters/explode/1.wav" 0.6)
    (make-sample "resources/downlifters/explode/2.wav" 0.6)
    (make-sample "resources/downlifters/explode/3.wav" 0.6)
    (make-sample "resources/downlifters/explode/4.wav" 0.5)
    (make-sample "resources/downlifters/explode/5.wav" 0.6)
    (make-sample "resources/downlifters/explode/6.wav" 0.5)
    (make-sample "resources/downlifters/explode/7.wav" 0.6)
    (make-sample "resources/downlifters/explode/8.wav" 0.7)
    (make-sample "resources/downlifters/explode/9.wav" 0.6)
    (make-sample "resources/downlifters/explode/10.wav" 0.6)
    (make-sample "resources/downlifters/explode/11.wav" 0.7)

    ])
  })

; fx downlifters
(def rand-downlift-fx {
  :name "rand-downlift-fx"
  :sample? true
  :callback (do-rand-action [
    
    (make-sample "resources/downlifters/fx/1.wav" 0.3)
    (make-sample "resources/downlifters/fx/2.wav" 0.4)
    (make-sample "resources/downlifters/fx/3.wav" 0.3)
    (make-sample "resources/downlifters/fx/4.wav" 0.5)
    (make-sample "resources/downlifters/fx/5.wav" 0.6)
    (make-sample "resources/downlifters/fx/6.wav" 0.5)
    (make-sample "resources/downlifters/fx/7.wav" 0.55)
    (make-sample "resources/downlifters/fx/8.wav" 0.6)
    (make-sample "resources/downlifters/fx/9.wav" 0.3)
    (make-sample "resources/downlifters/fx/10.wav" 0.55)
    (make-sample "resources/downlifters/fx/11.wav" 0.6)
    (make-sample "resources/downlifters/fx/12.wav" 0.3)
    (make-sample "resources/downlifters/fx/13.wav" 0.6)
    (make-sample "resources/downlifters/fx/14.wav" 0.6)


    ])
  })
