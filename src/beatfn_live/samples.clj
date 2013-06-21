(ns beatfn-live.samples
  (:use
    [beatfn-live.globals]
    [overtone.live :only [load-sample play-buf buffer-id out definst at]]))

; utility methods

(definst play-sample [id 0 vol 1 rate 1]
    (let [dry (play-buf 2 id rate)]
      (out [0 1] (* dry vol))))

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
    (if (= 0 offset)
      (fn [event vol] (sample vol))
      (offset-sample sample offset)))))

(defn do-rand-action [actions]
  (let [vol (/ (+ @sample-volume-state 1) 8)])
  #((rand-nth actions) % vol))


;
; define samples
;

; 4-beat uplifters
(def rand-uplift4 {
  :name :rand-uplift4
  :callback (do-rand-action [

    (make-sample "resources/uplifters/4beat/1.wav" 0.6 100 0.9)

    ])
  })

; 8-beat uplifters
(def rand-uplift8 {
  :name :rand-uplift8
  :callback (do-rand-action [

    (make-sample "resources/uplifters/8beat/1.wav" 0.5 185 1)
    (make-sample "resources/uplifters/8beat/2.wav" 0.5 0 1.285)

    ])
  })

; 16-beat uplifters
(def rand-uplift16 {
  :name :rand-uplift16
  :callback (do-rand-action [

    (make-sample "resources/uplifters/16beat/1.wav" 0.3 0 1.548)

    ])
  })

; crash downlifters
(def rand-downlift-crash {
  :name :rand-downlift-crash
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
  :name :rand-downlift-explode
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
  :name :rand-downlift-fx
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
