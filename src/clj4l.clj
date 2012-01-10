(ns clj4l
  "In max4l, to receive osc, use udpreceive without cnmat compatiblity, deferlow, skip OpenSoundControl,
   go directly to OSC-route." 
  (:require
   [clojure.string :as str]
   [clojure.contrib.logging :as log]
   [osc :as osc]))

; (alter-var-root #'*out* (constantly *out*))

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
                       (let [v (next %2)] (if (> (count v) 1) v (first v))))
               {})))

; (query-props "goto live_set" :tracks :scenes :clip_trigger_quantization)

(defn- as-key [s]
  "Live uses <empty> for scenes without names."
  (if (and s (not= s "<empty>"))
    (-> s
        (clojure.string/replace #"^\d*\s+" "")
        (clojure.string/replace #"\s+" "-")
        keyword)))

; (map as-key ["abacab" "1 Track 1" "  1" nil "<empty>"])

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

; (query-children "goto live_set" :tracks)
; (query-children "goto live_set" :scenes)
; (query "path live_set tracks 0 clip_slots 100 clip" "call select_all_notes" "call get_selected_notes")
; (query "path live_set tracks 0 clip_slots 0 clip" "call select_all_notes" "call get_selected_notes")
; (query "path live_set" "get tracks")

(defn control [& args]
  (doseq [cmd (map #(str/split % #"\s+") args)]
    (apply osc/osc-send *control-client* (str "/clj4l/control/" (if (#{"path" "goto"} (first cmd)) "path" "object")) cmd)))

; pitch time duration velocity muted
(defn notes-to-clip [track clip-slot notes]
  (apply control (concat [(str "path live_set tracks " track " clip_slots " clip-slot "  clip")
                           "call select_all_notes" "call replace_selected_notes"
                           (str "call notes " (count notes))]
                          (map #(apply str "call note " (interpose " " %)) notes) ["call done"])))

(comment
  (control "goto live_set" "getinfo")
  (control "goto live_set tracks 0" "get name")
  (control "goto live_set" "get tracks")

  (notes-to-clip 0 0 [[60 0.0 0.5 100 0 ]])
  (notes-to-clip 0 0 [[67 0.0 0.5 100 0 ]]) 
 
  (control "goto live_set" "call continue_playing")
  (control "goto live_set" "call stop_all_clips")
  (control "path live_set" "set current_song_time 0")
  (control "path live_set" "set current_song_time 16")
  (control "path live_set" (str "set current_song_time " (* 12 4)))
  (control "path live_set" "set current_song_time 0" "call start_playing")
  (control "path live_set" "set current_song_time 16" "call continue_playing")
  (control "path live_set scenes 0" "call fire")
  (control "path live_set scenes 1" "call fire")
  )

