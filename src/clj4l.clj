(ns clj4l
  "In max4l, to receive osc, use udpreceive without cnmat compatiblity, deferlow, skip OpenSoundControl,
   go directly to OSC-route." 
  (:require
   [clojure.string :as str]
   [clojure.contrib.logging :as log]
   [osc :as osc])
  (:use [clojure.java.shell :only [sh]])
  (:import (javax.sound.midi Sequencer Sequence Track MidiEvent MidiMessage ShortMessage MidiSystem)))

; (alter-var-root #'*out* (constantly *out*))

(def *tracks* nil)
(def *scenes* nil)

(defn set-log-level!
  "http://www.paullegato.com/blog/setting-clojure-log-level/"
  [level]
  "Sets the root logger's level, and the level of all of its Handlers, to level."
  (let [logger (log/impl-get-log "")]
    (.setLevel logger level)
    (doseq [handler (.getHandlers logger)]
      (. handler setLevel level))))

(set-log-level! java.util.logging.Level/FINEST)

(osc/osc-debug true)
(defonce *control-client* (osc/osc-client "127.0.0.1" 5432))

(defonce *query-ctx* (let [result (atom [])
                           result-server (osc/osc-server 3456)]
                       (osc/osc-handle result-server "/clj4l/result/begin" (fn [msg] (reset! result [])))
                       (osc/osc-handle result-server "/clj4l/result" (fn [msg] (swap! result conj (:args msg))))
                       {:query-client (osc/osc-client "127.0.0.1" 6543) :result-server result-server :result result }))

(defn- as-osc-msgs [path-prefix msgs]
  (->> msgs
       (map (fn [x]
              (if (string? x)
                (str/split x #"\s+")
                (concat (str/split (first x) #"\s+") (rest x)))))
       (map (fn [coll]
              (cons (str path-prefix (if (#{"path" "goto"} (first coll)) "path" "object")) coll)))))

(defn control [& msgs]
  (doseq [msg (as-osc-msgs "/clj4l/control/" msgs)]
    (apply osc/osc-send *control-client*  msg)))

(defn query [& msgs]
  (let [client (:query-client *query-ctx*)]
    (osc/osc-send client "/clj4l/query/begin")
    (doseq [msg (as-osc-msgs "/clj4l/query/" msgs)]
      (apply osc/osc-send client msg))
    (osc/osc-send client "/clj4l/query/end")
    (if (osc/osc-recv (:result-server *query-ctx*) "/clj4l/result/end" 2000)
      @(:result *query-ctx*)
      (throw (Exception. "Timed out waiting for query result.")))))

(defn query-props [path & props]
  (->> (apply query path (map #(str "get " (name %1)) props))
       (reduce #(assoc %1 (-> %2 first keyword)
                       (let [v (next %2)] (if (> (count v) 1) v (first v)))) {})))

(defn- as-key [s]
  "Live uses <empty> for scenes without names."
  (if (and s (not= s "<empty>"))
    (-> s
        (clojure.string/replace #"^\d*\s+" "")
        (clojure.string/replace #"\s+" "-")
        keyword)))

(defn query-children [path name & props]
  ":name is implicit in props"
  (->> (query-props path name) name (partition 2)
                       (map #(apply query-props (apply str (interpose \space %1)) (conj props :name)))
                       (map-indexed #(assoc %2 :index %1))
                       (map #(assoc % :key (as-key (:name %))))
                       (filter :key)
                       (reduce (fn [result {key :key :as m}]
                                 (if (find result key)
                                   (throw (Exception. (str "Duplicate key " key)))
                                   (assoc result key m))) {})))

(letfn [(get-index [m k err]
                   (if (number? k)
                     k
                     (if-let [index (-> k m :index)]
                       index
                       (throw (IllegalArgumentException. (str err ": dreadful key: " key))))))]
  (defn track-index [key] (get-index *tracks* key "track-index"))
  (defn scene-index [key] (get-index *scenes* key "scene-index")))

(defn scene-track-path [scene track] (str "goto live_set tracks " (track-index track) " clip_slots " (scene-index scene) " clip"))

(defmacro with-m4l [& body]
  `(binding [*tracks* (query-children "goto live_set" :tracks)
             *scenes* (query-children "goto live_set" :scenes)]
     (do ~@body)))

; pitch time duration velocity muted
(defn set-notes [scene track notes]
  (when (seq notes)
    #_(log/spy ["set-notes" (vec notes)])
    (control (scene-track-path scene track) "call select_all_notes" "call replace_selected_notes"
             (str "call notes " (count notes)))
    ; str hack since doubles don't go over the wire well.
    (doseq [{:keys [p t d v]} notes] (control ["call note" p (str t) (str d) v 0]))
    (control "call done")))

(defn get-notes [scene track]
  "m4l does not seem to order notes by time"
  (->> (query (scene-track-path scene track) "call select_all_notes" "call get_selected_notes")
       (filter #(= (first %) "get_selected_notes")) (drop 1) (drop-last) (map #(->> % (drop 2) vec))
       (map (fn [[p t d v]] {:p p :t t :d d :v v})) (sort #(< (:t %1) (:t %2)))))

(defn note-seq [length notes]
  (for [offset (iterate (partial + length) 0) n notes] (update-in n [:t] + offset)))

(defn take-time [t notes] (take-while #(< (:t %) t) notes))
(defn drop-time [t notes] (drop-while #(< (:t %) t) notes))
(defn offset-time [t notes] (map #(update-in % [:t] + t) notes))
(defn times-time [t times notes] (take-time (* t times) (note-seq t notes)))

; (take 5 (note-seq 4 [{:t 0.0 :p 64}]))
; (take 5 (repeat-time 4 2 [{:t 0 :p 64} {:t 3 :p 66}]))
; (take 5 (repeat-time 4 0 [{:t 0 :p 64}]))

(defn get-loop [scene track]
  (query-props (scene-track-path scene track) :loop_start :loop_end :looping :length))

(defn set-loop
  ([scene track end] (set-loop scene track 0.0 end))
  ([scene track start end]
     (control (scene-track-path scene track) (str "set loop_start " start) (str "set loop_end " end) "set looping 1")))

(defn get-note-seq [scene track] (note-seq (:loop_end (get-loop scene track)) (get-notes scene track)))

(defn set-matrix [matrix]
  #_(log/spy "set-matrix")
  (let [track-keys (map #(-> % val :key) *tracks*)]
    (doseq [[scene {:keys [scene-length] :as m}] matrix]
      (doseq [[track f] (select-keys m track-keys)]
        (set-notes scene track (take-time scene-length (f)))
        (set-loop scene track scene-length)))))

(defn scene-note-seqs [scene & tracks]
  (reduce (fn [m track] (assoc m track #(get-note-seq scene track))) {} tracks))

(def matrix-*
     [[:intro (merge (scene-note-seqs :seed-1 :synth :drums)
                     {:drums #(get-note-seq :seed-2 :drums)
                      :scene-length 16})]
      [:groove (merge (scene-note-seqs :seed-2 :synth :drums) {:scene-length 4})]])

(defn track-note-seq [track & specs]
  "spec is [scene take drop] where drop is optional"
  (let [m (reduce (fn [{:keys [t coll] :as m} [scene take times drop]]
                    (let [times (if times times 1)
                          drop (if drop drop 0)]
                      (-> m
                          (update-in [:coll] conj (->> (get-note-seq scene track) (drop-time drop) (offset-time (- drop))
                                                       (take-time take) (times-time take times) (offset-time t)))
                          (update-in [:t] + (* take times)))))
                  {:t 0 :coll []} specs)]
    (note-seq (:t m) (apply concat (:coll m)))))

(comment
  (with-m4l (take-time 16 (track-note-seq :drums [:seed-1 4 3] [:seed-1 4 0 12])))
  (with-m4l (take-time 16 (track-note-seq :drums [:seed-1 4])))

  (with-m4l (take-time 16 (->> (get-note-seq :seed-1 :drums) (drop-time 12) (offset-time -12) (take-time 4) (offset-time 0))))
  (with-m4l (take-time 16 (track-note-seq :drums [:seed-1 4] [:seed-1 4] [:seed-1 4] [:seed-1 4 0 12])))
  (with-m4l (take-time 16 (track-note-seq :drums [:intro 4] [:intro 4] [:intro 4] [:intro 4 12])))
  )

(def demo-matrix*
     [[:intro {:drums #(track-note-seq :drums [:intro 4] [:intro 4] [:intro 4] [:intro 4 12])
               :scene-length 16}]])

; (with-m4l (set-matrix demo-matrix*))

(def dms-matrix*
     [

      [:intro {:drums #(track-note-seq :drums [:intro 4 3] [:intro 4 1 12])               
               :scene-length 16}]
      [:intro-1 {:drums #(track-note-seq :drums [:intro 4 3] [:intro-1 4 1 12])
                 :scene-length 16}]
      [:intro-2 {:drums #(track-note-seq :drums [:intro-2 4 3] [:intro-2 4 1 12])
                 :bass #(track-note-seq :bass [:intro-1 16])
                 :scene-length 16}]
      [:intro-3 {:drums #(track-note-seq :drums [:intro-2 4 3] [:intro-2 4 1 12])
                 :bass #(track-note-seq :bass [:intro-1 16])
                 :scene-length 16}]])

                                        ; (with-m4l (set-matrix dms-matrix*))

(defn note-event [pitch velocity tick]
  (MidiEvent. (doto (ShortMessage.) (.setMessage ShortMessage/NOTE_ON pitch velocity)) tick))

(defn midifile [matrix tracks scenes]
  (let [ppq 480
        sequence (Sequence. Sequence/PPQ ppq)
        matrix (into {} matrix)        
        groups (->> (for [track tracks scene scenes] [track scene (-> scene matrix :scene-length)])
                    (group-by first))
        note-event (fn [p v t] (MidiEvent. (doto (ShortMessage.) (.setMessage ShortMessage/NOTE_ON p v))
                                          (* t ppq)))]
    (doseq [track tracks]
      (let [midi-track (.createTrack sequence)]
        (reduce (fn [{:keys [track-t] :as m} [track scene scene-length]]
                  (doseq [{:keys [p v t d]} (->> (get-notes scene track)
                                 (take-time scene-length)
                                 (offset-time track-t))]
                    (doto midi-track
                      (.add (note-event p v t))
                      (.add (note-event p 0 (+ t d)))))
                  (update-in m [:track-t] + scene-length)) {:track-t 0} (groups track))))
    (MidiSystem/write sequence, 1, (clojure.java.io/file "clj4l.mid"))))

(defn notate [matrix tracks scenes]
  (midifile matrix tracks scenes)
  (println (sh "midi2ly" "--duration-quant=16" "--start-quant=16" "clj4l.mid"))
  (println (sh "lilypond" "--pdf" "clj4l-midi.ly"))
  (println (sh "open" "clj4l-midi.pdf")))

; (with-m4l (notate dms-matrix* [:lead-synth :drums] [:intro :intro-1 :intro-2 :intro-3]))
; (with-m4l (notate dms-matrix* [:lead-synth] [:intro :intro-1]))

(comment
  (with-m4l
    (def notes* (get-notes :intro-3 :lead-synth)))
  (with-m4l
    (def tracks* *tracks*)
    (take-time 16 (track-note-seq :drums [:intro 4] [:intro 4] [:intro 4] [:intro 4 12]))
    #_(take-time 16 (track-note-seq :Bassline [:intro 4] [:intro 4] [:intro 4] [:intro 4 12])))

  (with-m4l (set-loop 0 0 4.0))
  (with-m4l (set-loop 0 0 8.0))
  (query-props "goto live_set tracks 0 clip_slots 0 clip" :length :loop_start :loop_end :looping :name)

  (get-notes 0 0)
  (set-notes 0 0 [{:p 60 :t 0.0 :d 4.0 :v 100}])
  (set-notes 0 0 [{:p 67 :t 2.0 :d 4.0 :v 100}])

  (query "goto live_set tracks 0 clip_slots 0 clip" "call select_all_notes" "call get_selected_notes")
 
  (control "goto live_set" "call start_playing")
  (control "goto live_set" "call stop_playing")
  )

