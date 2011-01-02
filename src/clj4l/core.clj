(ns clj4l.core
  (:require [clojure.string :as str])
  (:use [osc])
  (:import (org.jfugue MovableDoNotation Pattern Note MusicStringParser ParserListener
                       IntervalNotation MusicStringParser MidiRenderer)
           (javax.sound.midi Sequencer Sequence Transmitter Receiver ShortMessage MidiSystem)))

(defn create-client [] (osc-client "127.0.0.1" 5432))

(defonce *query-ctx* (let [result (atom {})
                           result-server (osc-server 3456)]
                       (osc-handle result-server "/clj4l/result/begin" (fn [msg] (reset! result {})))
                       (osc-handle result-server "/clj4l/result"
                                   (fn [msg]
                                     (let [key (keyword (first (:args msg)))
                                           value (next (:args msg))]
                                       (swap! result assoc key (conj (get @result key []) (if (= (count value) 1) (first value) value))))))
                       {:query-client (osc-client "127.0.0.1" 6543) :result-server result-server :result result }))

(defn sequence-for-pattern [pattern]
  (let [parser (MusicStringParser.)
        renderer (MidiRenderer. Sequence/PPQ 120)]
    (doto parser (.addParserListener renderer) (.parse pattern))
    (.getSequence renderer)))

(defn output-short-message [m type]
  (println type ": " (.getData1 m) " " (.getData2 m) " chan: " (.getChannel m)))

(defn decode-short-message [m client]
  (condp = (.getCommand m)
      (ShortMessage/NOTE_ON) (do (osc-send-bundle client (osc-bundle OSC-TIMETAG-NOW
                                                                     [(osc-msg-infer-types "/clj4l/track/1/note" (.getData1 m) (.getData2 m))]))
                                 (output-short-message m "noteon"))
      (ShortMessage/NOTE_OFF) (do (osc-send-bundle client (osc-bundle OSC-TIMETAG-NOW
                                                                      [(osc-msg-infer-types "/clj4l/track/1/note" (.getData1 m) 0)]))
                                  (output-short-message m "noteoff"))
      (ShortMessage/CONTROL_CHANGE) nil ; (output-short-message m "cc")
      (output-short-message m "unknown")))

(defn music-string-to-noteout
  ([music-string]
     (let [client (create-client)
           sequence (sequence-for-pattern (Pattern. music-string))
           sequencer (MidiSystem/getSequencer false)]
       (try
         (.open sequencer)
         (.. sequencer (getTransmitter)
             (setReceiver (reify Receiver
                                 (send [this midi-message timestamp]
                                       (when (instance? ShortMessage midi-message)
                                         (decode-short-message midi-message client))))))
         (doto sequencer (.setSequence sequence) (.start))
         (while (.isRunning sequencer) (Thread/sleep 20))
         (.close sequencer)
         (finally
          (osc-close client true)))))
  ([music-string root-note-num]
     (music-string-to-noteout (.. (org.jfugue.MovableDoNotation. music-string)
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
                                                             " duration: " (.getDuration note)
                                                             " isRest: " (.isRest note))
                                                    (dosync
                                                     (when-not (.isRest note) (add-note note @note-time notes))
                                                     (ref-set time (+ @note-time (.getDuration note)))))
                                 (sequentialNoteEvent [this note]
                                                      (println "sequentialNoteEvent: " (.getVerifyString note)
                                                               " duration: " (.getDuration note)
                                                               " isRest: " (.isRest note))
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
      (let [client (create-client)
            notes (music-string-to-m4l music-string)]
        (try
          (in-osc-bundle client OSC-TIMETAG-NOW
                         (osc-send client  "/clj4l/track/1/path" "path" "live_set" "tracks" 0 "clip_slots" 0 "clip")
                         (osc-send client  "/clj4l/track/1/object" "call" "select_all_notes")
                         (osc-send client  "/clj4l/track/1/object"  "call" "replace_selected_notes")
                         (osc-send client "/clj4l/track/1/object"  "call" "notes" (count notes))
                         (doseq [n notes]
                           (apply osc-send client "/clj4l/track/1/object" "call" "note" n))
                         (osc-send client "/clj4l/track/1/object" "call" "done"))
          (Thread/sleep 500)
          (finally
           (osc-close client true)))))
  ([music-string root-note-num]
     (music-string-to-clip (.. (org.jfugue.MovableDoNotation. music-string)
                               (getPatternForRootNote (Note. root-note-num)) (getMusicString)))))

(alter-var-root #'*out* (constantly *out*))

(comment (query [["begin"]
                 ["path" "path" "live_set" "tracks" 0 "clip_slots" 0 "clip"] ["object" "getinfo"]
                 ["end"]]))
(comment (query [["begin"]
                 ["path" "path" "live_set" "tracks" 0 "clip_slots" 0 "clip"] ["object" "call" "select_all_notes"] ["object" "call" "get_selected_notes"]
                 ["end"]]))
(comment (query [["begin"]
                 ["path" "path" "live_set"] ["object" "get" "tracks"]
                 ["end"]]))

;(query [["begin"] ["path" "path" "live_set" "tracks" 0 "clip_slots" 0 "clip"] ["object" "call" "select_all_notes"] ["object" "call" "get_selected_notes"] ["end"]])

(defmacro with-query-ctx [query-ctx & body]
  `(let [ctx# ~query-ctx
         ~'query-path #(apply osc-send (:query-client ctx#) "/clj4l/query/path" "path" (str/split % #"\s+"))
         ~'query-object #(apply osc-send (:query-client ctx#) "/clj4l/query/object" (str/split % #"\s+"))]
     (in-osc-bundle (:query-client ctx#) OSC-TIMETAG-NOW
                    (osc-send (:query-client ctx#) "/clj4l/query/begin")
                    ~@body
                    (osc-send (:query-client ctx#) "/clj4l/query/end")                   )
     (if (osc-recv (:result-server ctx#) "/clj4l/result/end" 1000)
       @(:result ctx#)
       (throw (Exception. "Timed out waiting for query result.")))))

(def m (with-query-ctx *query-ctx*
                    (query-path "live_set tracks 0 clip_slots 0 clip")
                    (query-object  "call select_all_notes")
                    (query-object  "call get_selected_notes")))

(println m)
