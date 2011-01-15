(ns clj4l.core
  (:require [clojure.string :as str])
  (:use [clojure.contrib.seq :only (find-first)])
  (:use [clojure.contrib.pprint :as pp])
  (:use [osc])
  (:import (org.jfugue MovableDoNotation Pattern Note MusicStringParser ParserListener
                       IntervalNotation MusicStringParser MidiRenderer)
           (javax.sound.midi Sequencer Sequence Transmitter Receiver ShortMessage MidiSystem)))

; TODO: 

(alter-var-root #'*out* (constantly *out*))

(defonce *control-client* (osc-client "127.0.0.1" 5432))

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
                                                                     [(osc-msg-infer-types "/clj4l/control/note" (.getData1 m) (.getData2 m))]))
                                 (output-short-message m "noteon"))
      (ShortMessage/NOTE_OFF) (do (osc-send-bundle client (osc-bundle OSC-TIMETAG-NOW
                                                                      [(osc-msg-infer-types "/clj4l/control/note" (.getData1 m) 0)]))
                                  (output-short-message m "noteoff"))
      (ShortMessage/CONTROL_CHANGE) nil ; (output-short-message m "cc")
      (output-short-message m "unknown")))

(defn music-string-to-noteout
  ([music-string]
     (let [client *control-client*
           sequence (sequence-for-pattern (Pattern. music-string))
           sequencer (MidiSystem/getSequencer false)]
       (.open sequencer)
       (.. sequencer (getTransmitter)
           (setReceiver (reify Receiver
                               (send [this midi-message timestamp]
                                     (when (instance? ShortMessage midi-message)
                                       (decode-short-message midi-message client))))))
       (doto sequencer (.setSequence sequence) (.start))
       (while (.isRunning sequencer) (Thread/sleep 20))
       (.close sequencer)))
  ([music-string root-note-num]
     (music-string-to-noteout (.. (org.jfugue.MovableDoNotation. music-string)
                                  (getPatternForRootNote (Note. root-note-num)) (getMusicString)))))

(comment
  (music-string-to-noteout "1 3 5" 60)
  )

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

(defn control [client & args]
  (in-osc-bundle client OSC-TIMETAG-NOW
                 (doseq [cmd (map #(str/split % #"\s+") args)]
                   (apply osc-send client (str "/clj4l/control/" (if (= (first cmd) "path") "path" "object")) cmd))))

(defn music-string-to-clip
  ([music-string]
      (let [client *control-client*
            notes (music-string-to-m4l music-string)]
        (apply control client (concat ["path live_set tracks 0 clip_slots 0 clip" "call select_all_notes"
                                 "call replace_selected_notes" (str "call notes " (count notes))]
                                      (map #(apply str "call note " (interpose " " %)) notes) ["call done"]))
        (comment (in-osc-bundle client OSC-TIMETAG-NOW
                                (osc-send client  "/clj4l/control/path" "path" "live_set" "tracks" 0 "clip_slots" 0 "clip")
                                (osc-send client  "/clj4l/control/object" "call" "select_all_notes")
                                (osc-send client  "/clj4l/control/object"  "call" "replace_selected_notes")
                                (osc-send client "/clj4l/control/object"  "call" "notes" (count notes))
                                (doseq [n notes]
                                  (apply osc-send client "/clj4l/control/object" "call" "note" n))
                                (osc-send client "/clj4l/control/object" "call" "done")))
        (Thread/sleep 500)))
  ([music-string root-note-num]
     (music-string-to-clip (.. (org.jfugue.MovableDoNotation. music-string)
                               (getPatternForRootNote (Note. root-note-num)) (getMusicString)))))


(comment

  (music-string-to-clip "1 1 4 8" 60)

  )

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
  (println "query-info-ids: " ids)
  (doall (map #(query-info {:id %1} ctx) (filter (complement string?) ids)))) ; (id 3 id 4)

(defn query-info-category [m category info ctx]
  (println "query-info-category: " category)
  ; [(id 3) (children clip_slots  ClipSlot) (property name unicode) (child canonical_parent Song)
  (if-let [specs (seq (filter #(= category (first %)) info))]
    ; {:canonical_parent [("id" 17)]}
    (let [result (apply query ctx (map #(apply str %&) (repeat "get ") (map #(second %) specs)))]
      (reduce (cond
               (= category "children") (fn [m attribute]
                                         (if (find-first #(= [(:type m) attribute] %) [["Song" :visited_tracks] ["Scene" :clip_slots]])
                                           (assoc m attribute (-> attribute result first))
                                           (do
                                             (println "query-info-category: attribute: " attribute ": " (-> attribute result))
                                             (assoc m attribute (-> attribute result first (query-info-ids ctx))))))
               (= category "child") (fn [m attribute]
                                      (println "query-info-category: attribute: " attribute ": " (-> attribute result))
                                      (if (find-first #(= [(:type m) attribute] %) [["ClipSlot" :clip]])
                                        (assoc m attribute (query-info {:id (-> attribute result first second)} ctx))
                                        (assoc m attribute (-> attribute result first))))
               :else #(assoc %1 %2 (-> %2 result first)))
              m (map #(-> % second keyword) specs)))
    m))

(defn query-info [m ctx]
  (let [info (:info (query ctx (str "id " (:id m)) "getinfo"))]
    (reduce #(query-info-category  %1 %2 info ctx)
            (assoc m :type (second (find-first #(= (first %) "type") info)))
            ["property" "children" "child"])))

(defn query-tracks [ctx]
  (query-info-ids (first (:tracks (query ctx "path live_set" "get tracks"))) ctx))

(defn query-song [ctx]
  (query-info {:id (-> (query ctx "path live_set") :id first)} ctx))

;(def t (query-tracks *query-ctx*))
;(def s (query-song *query-ctx*))
;(pp/pprint (-> s :tracks (nth 0) :clip_slots (nth 0)))
