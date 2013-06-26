(ns beatfn-live.globals
  (:use
    [overtone.live :only [metronome]]))

(def BPM 128)
(def m (metronome BPM))
(def NUM_SCENES 2) ; max of 4, check out assert-scene-state-led if this changes
(def NUM_BANKS 4) ; number of possible banks
(def LAUNCHPAD_LENGTH 8)
(def LAUNCHPAD_AREA (* LAUNCHPAD_LENGTH LAUNCHPAD_LENGTH))
(def scene-state (atom 0)) ; the number of the currently active scene
(def bank-state (atom 0)) ; 0 for action bank,
                          ; 1 for grid editor bank
                          ; 2 for zoom select bank
                          ; 3 unused atm

(def sample-volume-state (atom 3)) ; last pressed volume, starts at 4/8
(def repeat-state (atom 0)) ; 0 for repeat off, 1 for repeat on
(def tracker-state (atom 1)) ; 0: paused with LED off
                             ; 1: ready with LED orange
                             ; 2: playing with LED green

; TODO: scrolling through active actions (implementation: scrollthrough active bank?)
; TODO: add zoom-select bank
; TODO: move scene-select to arrows
; TODO: looping. make it so that you can select an area over which to loop
; TODO: copy/paste regions. selection goes from top-left to bottom-right.
; TODO: to make scenes really awesome, they each need a crossfader "prop"!
; TODO: figure out underlying clojure-launchpad stuff to get yellow/more intensities
; TODO: need a way to specify volume of samples you are placing
; TODO: need some way to preview stuff (headphones)

(defn null-callback [x y pressed?] nil)

(def null-action
  {:name :null
   :callback (fn [event] (println "null-action called!"))})

; an endless vector of possible actions TODO: make it so this isn't finite length
(def loaded-actions (atom (vec (repeat LAUNCHPAD_LENGTH null-action))))

(defn make-bank []
  (do (println "Money.")
      (atom (vec (repeat LAUNCHPAD_LENGTH null-callback)))))

; a vector of action buttons
(def action-bank (make-bank))

; vector of volume choices
(def volume-bank (make-bank))

; vector of grid editor buttons TODO: finish this, requires editing grid-press,
;                                     assert-grid-led, and writing fxns to fill this
(def grid-editor-bank (make-bank))

; vector of zoom select buttons TODO: finish this, requires editing grid-press,
;                                     assert-grid-led, and writing fxns to fill this
(def zoom-select-bank (make-bank))

; the currently loaded bank (which is a vector of functions)
(def banks (atom [action-bank volume-bank grid-editor-bank zoom-select-bank]))

; number which tells how many action buttons are currently held down
(def actions-pressed (atom 0))

; array of numbers which designate the currently active actions
(def active-action-numbers (atom [0]))

; a map k/v'd with:
; {beat-event {scene0 {action-event1 scheduled-action1
;                      action-event2 scheduled-action2}
;              scene1 {action-event}
;}
; TODO: instead of a 3-nested map, how about 2-nested with an array for an outer key?
;       or maybe 3-nested, but with the outermost being scene? or maybe no change?
(def scheduled-actions (atom {}))

; a vector of current deck volumes, currently engineered for 4 decks
(def deck-volumes (atom (vec [1 1 1 1])))