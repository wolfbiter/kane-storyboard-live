(ns beatfn-live.metronome
  (:use
    [overtone.live :only [metronome]]))

(def BPM 128)
(def m (metronome BPM))