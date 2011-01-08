(ns clj4l.core
  (:require [clojure.string :as str])
  (:use [osc])
  (:import (org.jfugue MovableDoNotation Pattern Note MusicStringParser ParserListener
                       IntervalNotation MusicStringParser MidiRenderer)
           (javax.sound.midi Sequencer Sequence Transmitter Receiver ShortMessage MidiSystem)))

; TODO: query: nil, capture type

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

(comment (defmacro with-query-ctx [query-ctx & body]
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

         (println m))

(defn query [ctx & args]
  (println "query: " args)
  (in-osc-bundle (:query-client ctx) OSC-TIMETAG-NOW
                    (osc-send (:query-client ctx) "/clj4l/query/begin")
                    (doseq [cmd (map #(str/split % #"\s+") args)]
                      (apply osc-send (:query-client ctx) (str "/clj4l/query/" (if (= (first cmd) "path") "path" "object")) cmd))
                    (osc-send (:query-client ctx) "/clj4l/query/end"))
     (if (osc-recv (:result-server ctx) "/clj4l/result/end" 2000)
       @(:result ctx)
       (throw (Exception. "Timed out waiting for query result."))))

;(println (query *query-ctx* "path live_set tracks 0 clip_slots 0 clip" "call select_all_notes" "call get_selected_notes"))
;(println (query *query-ctx* "path live_set tracks 0 clip_slots 0 clip" "getinfo"))
;(println (query *query-ctx* "path live_set" "get tracks"))

;{:info [(id 3) (type Track) (children clip_slots ClipSlot) (children devices Device) (child canonical_parent Song) (child mixer_device MixerDevice) (child view View) (property arm bool) (property can_be_armed bool) (property color int) (property current_input_routing unicode) (property current_input_sub_routing unicode) (property current_monitoring_state int) (property current_output_routing unicode) (property current_output_sub_routing unicode) (property fired_slot_index int) (property has_audio_input bool) (property has_audio_output bool) (property has_midi_input bool) (property has_midi_output bool) (property input_meter_level float) (property input_routings tuple) (property input_sub_routings tuple) (property is_foldable bool) (property is_part_of_selection bool) (property is_visible bool) (property mute bool) (property name unicode) (property output_meter_level float) (property output_routings tuple) (property output_sub_routings tuple) (property playing_slot_index int) (property solo bool) (function jump_in_running_session_clip) (function stop_all_clips) done], :name [2-MIDI]}

; {:input_routings [("All" "Ins" "Automap" "MIDI" "Computer" "Keyboard" "from" "MaxMSP" 1 "from" "MaxMSP" 2 "1-Sylenth1" "No" "Input")], :fired_slot_index [-1], :arm [0], :is_visible [1], :output_meter_level [0.0], :solo [0], :current_input_routing ["Ext: All Ins"], :input_sub_routings [("All" "Channels" "Ch." 1 "Ch." 2 "Ch." 3 "Ch." 4 "Ch." 5 "Ch." 6 "Ch." 7 "Ch." 8 "Ch." 9 "Ch." 10 "Ch." 11 "Ch." 12 "Ch." 13 "Ch." 14 "Ch." 15 "Ch." 16)], :name ["2-MIDI"], :current_monitoring_state [1], :current_output_routing ["None"], :can_be_armed [1], :color [0], :has_audio_output [0], :input_meter_level [0.0], :output_routings [("Automap" "MIDI" "Numerology3" "1-Sylenth1" "No" "Output")], :playing_slot_index [-1], :has_midi_output [1], :is_foldable [0], :is_part_of_selection [0], :has_audio_input [0], :has_midi_input [1], :mute [0]}

(declare query-info)

(defn query-info-ids [ids ctx]
  (doall (map #(query-info {:id %1} ctx) (filter (complement string?) ids)))) ; (id 3 id 4)

(defn query-info-category [m category info ctx]
  ; Do not specify child as category
  ; [(id 3) (children clip_slots  ClipSlot) (property name unicode) (child canonical_parent Song)
  (let [specs (filter #(= category (first %)) info) 
          result (apply query ctx (map #(apply str %&) (repeat "get ") (map #(second %) specs)))]
    (reduce (if (= category "property") #(assoc %1 %2 (-> %2 result first)) #(assoc %1 %2 (-> %2 result first (query-info-ids ctx))))
            m (map #(-> % second keyword) specs))))

(defn query-info [m ctx]
  (let [info (:info (query ctx (str "id " (:id m)) "getinfo"))]
    (reduce #(query-info-category  %1 %2 info ctx) m ["property" "children"])))

(defn query-tracks [ctx]
  (prn (query-info-ids (first (:tracks (query ctx "path live_set" "get tracks"))) ctx)))

(query-tracks *query-ctx*)

