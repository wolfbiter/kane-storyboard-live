(ns beatfn-live.globals
  (:use
    [overtone.live :only [metronome]]))

(def BPM 128)
(def m (metronome BPM))
(def NUM_ACTION_STATES 4) ; currently handles 4 preset action banks
(def NUM_BANKS 4) ; number of possible banks
(def MIN_STEP (/ 1 4)) ; minimum possible step size
(def MAX_ZOOM 1.0) ; be wary of schedule-action when changing this!
(def LAUNCHPAD_LENGTH 8)
(def LAUNCHPAD_AREA (* LAUNCHPAD_LENGTH LAUNCHPAD_LENGTH))
(def scene-state (atom 0)) ; the number of the currently active scene
(def bank-state (atom 0)) ; 0 for action bank,
                          ; 1 for grid editor bank
                          ; 2 for zoom select bank
                          ; 3 unused atm
(def zoom-state (atom 1.0)) ; start off with zoom scale of 1

(def sample-volume-state (atom 4)) ; last pressed volume, starts at 4/8
(def repeat-state (atom 0)) ; 0 for repeat off, 1 for repeat on
(def tracker-state (atom 1)) ; 0: paused with LED off
                             ; 1: ready with LED orange
                             ; 2: playing with LED green

; TODO: scrolling through active actions (implementation: scrollthrough active bank?)
; TODO: looping. make it so that you can select an area over which to loop
; TODO: copy/paste regions. selection goes from top-left to bottom-right.
; TODO: to make scenes really awesome, they each need a crossfader "prop"!
; TODO: figure out underlying clojure-launchpad stuff to get yellow/more intensities

(defn null-callback [x y pressed?] nil)

(def null-action
  {:name "null"
   :callback (fn [event] (println "null-action called!"))})

; an endless vector of possible actions TODO: make it so this isn't finite length
(def loaded-actions (atom (vec (repeat (* NUM_ACTION_STATES LAUNCHPAD_LENGTH) null-action))))

(defn make-bank []
  (do (println "Money.")
      (atom (vec (repeat LAUNCHPAD_LENGTH null-callback)))))

; action state determines which buttons are currently loaded into the action bank
(def action-state (atom 0))

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

; a vector of current deck volumes, currently engineered for 4 decks
(def deck-volumes (atom (vec [1 1 1 1])))

;
; non-grid buttons
;

; TODO: turn special buttons into maps of location and callback
(def zoom-state-up-loc {:x 0 :y 8})
(def zoom-state-down-loc {:x 1 :y 8})
(def scene-state-left-loc {:x 2 :y 8})
(def scene-state-right-loc {:x 3 :y 8})
(def tracker-state-loc {:x 4 :y 8})
(def repeat-state-loc {:x 5 :y 8})
(def action-state-loc {:x 6 :y 8})
(def bank-state-loc {:x 7 :y 8})