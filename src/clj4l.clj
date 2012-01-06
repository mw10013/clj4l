(ns clj4l
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
  (osc/in-osc-bundle client osc/OSC-TIMETAG-NOW
                 (doseq [cmd (map #(str/split % #"\s+") args)]
                   (apply osc/osc-send client (str "/clj4l/control/" (if (= (first cmd) "path") "path" "object")) cmd))))

(comment
  (control *control-client* "path live_set" "call continue_playing")
  (control *control-client* "path live_set" "call stop_playing")
  (control *control-client* "path live_set" "call stop_all_clips")
  (control *control-client* "path live_set" "set current_song_time 0")
  (control *control-client* "path live_set" "set current_song_time 16")
  (control *control-client* "path live_set" (str "set current_song_time " (* 12 4)))
  (control *control-client* "path live_set" "set current_song_time 0" "call start_playing")
  (control *control-client* "path live_set" "set current_song_time 16" "call continue_playing")
  (control *control-client* "path live_set scenes 0" "call fire")
  (control *control-client* "path live_set scenes 1" "call fire")
  )



