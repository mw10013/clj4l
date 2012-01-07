(ns clj4l
  "In max4l, to receive osc, use udpreceive without cnmat compatiblity, deferlow, skip OpenSoundControl,
   go directly to OSC-route." 
  (:require
   [clojure.string :as str]
   [clojure.contrib.logging :as log]
   [osc :as osc]))

; (alter-var-root #'*out* (constantly *out*))

(osc/osc-debug true)
(defonce *control-client* (osc/osc-client "127.0.0.1" 5432))

(comment (def *query-port* 3456)
         (defonce *query-ctx* (let [result (atom [])
                                    result-server (osc/osc-server *query-port*)]
                                (osc/osc-handle result-server "/clj4l/result/begin" (fn [msg] (reset! result {})))
                                (osc/osc-handle result-server "/clj4l/result"
                                                (fn [msg] (def msg* msg) (swap! result conj (-> msg :args first))))
                                {:result-server result-server :result result})))

(defn set-log-level!
  "http://www.paullegato.com/blog/setting-clojure-log-level/"
  [level]
  "Sets the root logger's level, and the level of all of its Handlers, to level."
  (let [logger (log/impl-get-log "")]
    (.setLevel logger level)
    (doseq [handler (.getHandlers logger)]
      (. handler setLevel level))))

(set-log-level! java.util.logging.Level/FINEST)

(defn control [client & args]
  (doseq [cmd (map #(str/split % #"\s+") args)]
    (apply osc/osc-send client (str "/clj4l/control/" (if (= (first cmd) "path") "path" "object")) cmd)))

; pitch time duration velocity muted
(defn notes-to-clip
  ([track clip-slot notes]
     (notes-to-clip *control-client* track clip-slot notes))
  ([client track clip-slot notes]
     (apply control client (concat [(str "path live_set tracks " track " clip_slots " clip-slot "  clip")
                                    "call select_all_notes" "call replace_selected_notes"
                                    (str "call notes " (count notes))]
                                   (map #(apply str "call note " (interpose " " %)) notes) ["call done"]))))

(comment
  (notes-to-clip 0 0 [[60 0.0 0.5 100 0 ]])
  (notes-to-clip 0 0 [[67 0.0 0.5 100 0 ]])
  
  (control *control-client* "path live_set" "call continue_playing")
  (control *control-client* "path live_set" "call stop_all_clips")
  (control *control-client* "path live_set" "set current_song_time 0")
  (control *control-client* "path live_set" "set current_song_time 16")
  (control *control-client* "path live_set" (str "set current_song_time " (* 12 4)))
  (control *control-client* "path live_set" "set current_song_time 0" "call start_playing")
  (control *control-client* "path live_set" "set current_song_time 16" "call continue_playing")
  (control *control-client* "path live_set scenes 0" "call fire")
  (control *control-client* "path live_set scenes 1" "call fire")
  )

