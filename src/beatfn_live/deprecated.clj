
(def latency (atom 100))

(def raw-notes [:c3 :g3 :c3])

; TODO: turn this into a map of note and freq
(def notes (flatten (map #(list % ((comp midi->hz note) %)) [:c3 :g3 :c3])))

(on-event [:midi nil]
  (fn [e]
   (if (= (:name (:device e)) "VirMIDI [hw:4,0,1]")
    (do
     ;(println "e's name: " (:name (:device e)))
     ;(doall (map println e))
     (if (= (:status e) :start)
       (do
        (println "START")
        ;(doall (map println e))
        (println "message timestamp:" (:timestamp e))
        (println "now: " (now))
        ;(def m (atom (metronome 128)))
        (metro-start m 1)
        (println "END"))))))
  ::keyboard-action)

(defn player
  [beat]
  (let [next-beat (inc beat)]
    (at (- (@m beat) @latency)
        (quick-kick :amp 0.5)
        (if (zero? (mod beat 2))
          (open-hat :amp 0.1)))
    (at (- (@m (+ 0.5 beat)) @latency)
        (haziti-clap :decay 0.05 :amp 0.3))

    (when (zero? (mod beat 3))
      (at (- (@m (+ 0.75 beat)) @latency)
          (soft-hat :decay 0.03 :amp 0.2)))

    (when (zero? (mod beat 8))
      (at (- (@m (+ 1.25 beat)) @latency)
          (soft-hat :decay 0.03)))

    (apply-at (@m next-beat) #'player [next-beat])))

(definst dubstep [freq 100 wobble-freq 2]
  (let [sweep (lin-exp (lf-saw wobble-freq) -1 1 40 5000)
        son (mix (saw (* freq [0.99 1 1.01])))]
    (lpf son sweep)))

(def default-env (lin-exp (lf-saw 1) -1 1 40 5000))

(def freqTest (atom 1.0))