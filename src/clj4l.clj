(ns clj4l
  "In max4l, to receive osc, use udpreceive without cnmat compatiblity, deferlow, skip OpenSoundControl,
   go directly to OSC-route." 
  (:require
   [clojure.string :as str]
   [clojure.contrib.logging :as log]
   [osc :as osc]))

; (alter-var-root #'*out* (constantly *out*))

(def *tracks*)
(def *scenes*)

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
                       (osc/osc-handle result-server "/clj4l/result" (fn [msg] (swap! result conj (:args (log/spy msg)))))
                       {:query-client (osc/osc-client "127.0.0.1" 6543) :result-server result-server :result result }))

(defn query [& args]
  (let [client (:query-client *query-ctx*)]
    (osc/osc-send client "/clj4l/query/begin")
    (doseq [cmd (map #(str/split % #"\s+") args)]
      (apply osc/osc-send client (str "/clj4l/query/" (if (#{"path" "goto"} (first cmd)) "path" "object")) cmd))
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

(defn control [& args]
  (doseq [cmd (map #(str/split % #"\s+") args)]
    (apply osc/osc-send *control-client* (str "/clj4l/control/" (if (#{"path" "goto"} (first cmd)) "path" "object")) cmd)))

(defn track-index [key] (-> key *tracks* :index))
(defn scene-index [key] (-> key *scenes* :index))

(defmacro with-m4l [& body]
  `(binding [*tracks* (query-children "goto live_set" :tracks)
             *scenes* (query-children "goto live_set" :scenes)]
     (do ~@body)))

; pitch time duration velocity muted
(defn set-notes [track scene notes]
  (control (str "goto live_set tracks " (track-index track) " clip_slots " (scene-index scene) " clip")
           "call select_all_notes" "call replace_selected_notes" (str "call notes " (count notes)))
  (doseq [n notes] (control (apply str "call note " (interpose \space n))))
  (control "call done"))

(defn get-notes [track scene]
  (->> (query (str "goto live_set tracks " (track-index track) " clip_slots " (scene-index scene) " clip")
              "call select_all_notes" "call get_selected_notes")
       (filter #(= (first %) "get_selected_notes")) (drop 1) butlast (map #(->> % (drop 2) vec)) vec))

(defn set-loop
  ([track scene end] (set-loop track scene 0.0 end))
  ([track scene start end]
     (control (str "goto live_set tracks " (track-index track) " clip_slots " (scene-index scene) " clip")
              (str "set loop_start " start) (str "set loop_end " end) "set looping 1")))

(comment
  (with-m4l (set-loop :synth :1 4.0))
  (with-m4l (set-loop :synth :1 8.0))
  (query-props "goto live_set tracks 0 clip_slots 0 clip" :length :loop_start :loop_end :looping :name)

  (with-m4l (get-notes :synth :1))
  (with-m4l (set-notes :synth :1 [[60 0.0 0.5 100 0] [60 1.0 0.5 100 0]]))
  (with-m4l (set-notes :synth :1 [[67 0.0 0.5 100 0]]))

  (query "goto live_set tracks 0 clip_slots 0 clip" "call select_all_notes" "call get_selected_notes")
 
  (control "goto live_set" "call start_playing")
  (control "goto live_set" "call stop_playing")
  )

