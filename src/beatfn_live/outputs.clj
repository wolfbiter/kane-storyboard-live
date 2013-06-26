(ns beatfn-live.outputs
  (:use
    [overtone.live :only [defsynth sound-in out inst-fx! fx-rlpf fx-rhpf]]))

; --------------- Output Stuff -----------------------

(defsynth output [deck1 1 deck2 1]
  (out [2 3] (* deck1 (sound-in [2 3])))
  (out [4 5] (* deck2 (sound-in [4 5]))))

;(definst output [vol1 1.0 lpf1 1.0 rq 1.0]
;  (let [vol-env1 (env-gen (adsr) :gate vol1 :action NO-ACTION)
;        lpf-env1 (env-gen (adsr) :gate lpf1 :action NO-ACTION)]
;        (rlpf (* vol-env1 (sound-in [0 1]))
;              lpf-env1
;              rq)))

; (ctl deck-outputs :deck1 1.0)
(def deck-outputs (output))

;; (ctl lowpass :cutoff 10000 :res 1)
;(def lowpass (inst-fx! deck-outputs fx-rlpf))
;
;; (ctl highpass :cutoff 10000 :res 1)
;(def highpass (inst-fx! deck-outputs fx-rhpf))
