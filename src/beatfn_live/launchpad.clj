; Adapted from:
;   1. https://github.com/moumar/clj-launchpad/
;   2. https://github.com/overtone/overtone.device.launchpad/

(ns beatfn-live.launchpad
  (:use [clojure.set :only [map-invert]]
        [overtone.midi])
  (:import [javax.sound.midi MidiSystem Receiver ShortMessage]))

(def #^{:private true} intensities
  { :off 0 :low 1 :medium 2 :high 3 })

(defn- send-midi [{:keys [out]} & args]
  (.send out
    (doto (ShortMessage.) (.setMessage (nth args 0) (nth args 1) (nth args 2)))
         -1))

(defn fix-y [y]
  (if true;(= y 8)
    y
    (- 7 y)))

(defn draw-grid
  "set a cell grid on or off.
x can be 0 to 8 inclusive (8 is for the top buttons)
y can be 0 to 8 inclusive (8 is for the most right buttons)
color description is a combination of the following possible values:
:off
:low :medium :high
:red :green :orange
:flashing
Examples:
(draw-grid lpad 1 2 :green :medium :flashing)
(draw-grid lpad 1 2 :off)
"
  [lpad x y1 & color-description]
  (let [y (fix-y y1)
        color-description (set color-description)
        color-intensity ( (or (some #{:off :low :medium :high} color-description) :high) intensities)
        color (some #{:red :green :orange} color-description)
        velocity (+
                            (if (:flashing color-description) 8 12)
                            (case color
                              :red color-intensity
                              :green (* 16 color-intensity)
                              :orange (+ color-intensity (* 16 color-intensity ))
                              0))
        midi-message (if (= 8 y) 0xB0 0x90)
        midi-position (if (= 8 y)
                            (+ x 0x68)
                            (+ x (* 16 y))) ]
    #_(println
             "x" x
             "y" y
             "velocity:" velocity)
  (send-midi lpad midi-message midi-position velocity)))

(defn reset
  "reset the launchpad (all lights off)"
  [lpad]
  (send-midi lpad 0xB0 0 0)
  (send-midi lpad 0xB0 0 0x28) ; activate flashing
  )

(defn clear-grid [lpad]
  "clear the launchpad grid (not the top and left buttons)"
  (dorun (for [x (range 8)
               y (range 8)] (draw-grid lpad x y :off))))

(defn test-leds
  "lights all the leds in one command. Intensity can be :low, :medium or :high"
  ([lpad] (test-leds lpad :high))
  ([lpad intensity]
    (send-midi lpad 0xb0 0 (+ (intensity intensities) 0x7c))))

(defn open []
  "find the launchpad in the available midi devices and return a launchpad object suitable
for the calls of this library"
  (let [[in-device out-device]
           (sort-by #(.getMaxTransmitters % )
                    (map #(MidiSystem/getMidiDevice %)
                  (filter #(re-find #"Launchpad" (.getName %)) (MidiSystem/getMidiDeviceInfo))))
        out (.getReceiver out-device)
        in (.getTransmitter in-device)
        lpad {:in-device in-device
              :out-device out-device
              :in in
              :out out}]
    (do
      (.open out-device)
      (.open in-device)
      (test-leds lpad)
      (Thread/sleep 100)
      (reset lpad))
    lpad))

(defn on-grid-pressed
  "Define a callback function when the grid is pressed.
The function takes 3 parameters: x y pressed?"
  [{:keys [in]} callback]
  (.setReceiver in
    (reify Receiver
      (send [this msg ts]
        (let [cmd (.getCommand msg)
              b (.getData1 msg)
              top-button? (= 0xb0 cmd)
              pressed? (= 127 (.getData2 msg))
              x (if top-button?
                  (- b 0x68)
                  (-> b (mod 16) (mod 9)))
              y (if top-button?
                  8
                  (quot b 16))]
            (callback x (fix-y y) pressed?))))))

(defn close [lpad]
  "close the launchpad device"
  (dorun (map #(.close %) (vals lpad))))

; ----- overtone.device.launchpad stuff

(defn coords->midi-note [x y]
  (+ x (* 16 y)))

(def midi-note->coords
  (into {} (for [x (range 8)
                 y (range 8)]
             [(+ x (* y 16)) [x y]])))

(def metakeys->midi
  {:up {:cmd :control-change :note 104}
   :down {:cmd :control-change :note 105}
   :left {:cmd :control-change :note 106}
   :right {:cmd :control-change :note 107}
   :session {:cmd :control-change :note 108}
   :user1 {:cmd :control-change :note 109}
   :user2 {:cmd :control-change :note 110}
   :mixer {:cmd :control-change :note 111}
   :vol {:cmd :note-on :note 8}
   :pan {:cmd :note-on :note 24}
   :snda {:cmd :note-on :note 40}
   :sndb {:cmd :note-on :note 56}
   :stop {:cmd :note-on :note 72}
   :trkon {:cmd :note-on :note 88}
   :solo {:cmd :note-on :note 104}
   :arm {:cmd :note-on :note 120}})

(def midi->metakeys
  (map-invert metakeys->midi))