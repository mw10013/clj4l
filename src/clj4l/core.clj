(ns clj4l.core
  (:import (org.jfugue MovableDoNotation Pattern Note MusicStringParser ParserListener IntervalNotation MusicStringParser MidiRenderer)
           (javax.sound.midi Sequencer Sequence Transmitter Receiver ShortMessage MidiSystem)
           (java.net InetAddress)
           (com.illposed.osc OSCPort OSCPortOut OSCPortIn OSCBundle OSCMessage OSCListener)))

(defn sequence-for-pattern [pattern]
  (let [parser (MusicStringParser.)
        renderer (MidiRenderer. Sequence/PPQ 120)]
    (doto parser (.addParserListener renderer) (.parse pattern))
    (.getSequence renderer)))

(defn output-short-message [m type]
  (println type ": " (.getData1 m) " " (.getData2 m) " chan: " (.getChannel m)))

(defn decode-short-message [m osc-out]
  (condp = (.getCommand m)
      (ShortMessage/NOTE_ON) (do (.send osc-out (OSCMessage. "/mw10013/m4l/track/1/note" (object-array [(.getData1 m) (.getData2 m)])))
                                 (output-short-message m "noteon"))
      (ShortMessage/NOTE_OFF) (do (.send osc-out (OSCMessage. "/mw10013/m4l/track/1/note" (object-array [(.getData1 m) 0])))
                                  (output-short-message m "noteoff"))
      (ShortMessage/CONTROL_CHANGE) (output-short-message m "cc")
      (output-short-message m "unknown")))

(defn music-string-to-noteout
  ([music-string]
     (with-open [osc-out (OSCPortOut. (java.net.InetAddress/getLocalHost) 5432)]
       (let [sequence (sequence-for-pattern (Pattern. music-string))
             sequencer (MidiSystem/getSequencer false)]
         (.open sequencer)
         (.. sequencer (getTransmitter)
             (setReceiver (reify Receiver
                                 (send [this midi-message timestamp]
                                       (when (instance? ShortMessage midi-message) (decode-short-message midi-message osc-out))))))
         (doto sequencer (.setSequence sequence) (.start))
         (while (.isRunning sequencer) (Thread/sleep 20))
         (.close sequencer))))
  ([music-string root-note-num]
     (music-string-note-out (.. (org.jfugue.MovableDoNotation. music-string)
                               (getPatternForRootNote (Note. root-note-num)) (getMusicString)))))

(defn add-note [note note-time notes]
          (alter notes conj [(int (.getValue note))  (float (/ note-time 120.0))
                             (float (.getDecimalDuration note)) (int (.getAttackVelocity note)) 0]))

(comment
call replace_selected_notes
call notes count 
call note pitch time duration velocity muted 
call done
)
(defn music-string-to-m4l [music-string]
  (let [time (ref 0) note-time (ref 0) notes (ref [])]
    (doto (MusicStringParser.)
      (.addParserListener (reify ParserListener
                                 (noteEvent [this note]
                                            (println "noteEvent: " (.getVerifyString note)
                                                     " duration: " (.getDuration note) " isRest: " (.isRest note))
                                            (dosync
                                             (ref-set note-time @time)
                                             (when-not (.isRest note) (add-note note @note-time notes))
                                             (alter time + (.getDuration note))))
                                 (parallelNoteEvent [this note]
                                                    (println "parallelNoteEven: " (.getVerifyString note)
                                                             " duration: " (.getDuration note) " isRest: " (.isRest note))
                                                    (dosync
                                                     (when-not (.isRest note) (add-note note @note-time notes))
                                                     (ref-set time (+ @note-time (.getDuration note)))))
                                 (sequentialNoteEvent [this note]
                                                      (println "sequentialNoteEvent: " (.getVerifyString note)
                                                               " duration: " (.getDuration note) " isRest: " (.isRest note))
                                                      (dosync
                                                       (ref-set note-time @time)
                                                       (when-not (.isRest note) (add-note note @note-time notes))
                                                       (ref-set time (+ @note-time (.getDuration note)))))
                                 (tempoEvent [this tempo] (println "tempoEvent: " (.getVerifyString tempo)))
                                 (measureEvent [this measure]))
                          )
      (.parse (Pattern. music-string)))
    @notes))

(defn music-string-to-clip
  ([music-string]
      (let [notes (music-string-to-m4l music-string)
            bundle (OSCBundle.)]
        (doto bundle
          (.addPacket (OSCMessage. "/mw10013/m4l/track/1/path" (object-array ["path" "live_set" "tracks" 0 "clip_slots" 0 "clip"])))
          (.addPacket (OSCMessage. "/mw10013/m4l/track/1/object" (object-array ["call" "select_all_notes"])))
          (.addPacket (OSCMessage. "/mw10013/m4l/track/1/object" (object-array ["call" "replace_selected_notes"])))
          (.addPacket (OSCMessage. "/mw10013/m4l/track/1/object" (object-array ["call" "notes" (count notes)]))))
        (doseq [n notes]
          (.addPacket bundle (OSCMessage. "/mw10013/m4l/track/1/object" (object-array (apply conj ["call" "note"] n)))))
        (.addPacket bundle (OSCMessage. "/mw10013/m4l/track/1/object" (object-array ["call" "done"])))
        (with-open [osc-out (OSCPortOut. (java.net.InetAddress/getLocalHost) 5432)]
          (.send osc-out bundle))))
  ([music-string root-note-num]
     (music-string-to-clip (.. (org.jfugue.MovableDoNotation. music-string)
                               (getPatternForRootNote (Note. root-note-num)) (getMusicString)))))

