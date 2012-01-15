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
    (apply osc/osc-send *control-client* (log/spy msg))))

(defn query [& msgs]
  (let [client (:query-client *query-ctx*)]
    (osc/osc-send client "/clj4l/query/begin")
    (doseq [msg (as-osc-msgs "/clj4l/query/" msgs)]
      (apply osc/osc-send client (log/spy msg)))
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
    (control (scene-track-path scene track) "call select_all_notes" "call replace_selected_notes"
             (str "call notes " (count notes)))
    (doseq [n notes] (control (apply str "call note " (interpose \space n))))
    (control "call done")))

(defn get-notes [scene track]
  (->> (query (scene-track-path scene track) "call select_all_notes" "call get_selected_notes")
       (filter #(= (first %) "get_selected_notes")) (drop 1) (drop-last) (map #(->> % (drop 2) vec))
       #_(map (fn [[p t d v]] {:p p :t t :d d :v v})) vec))

(defn note-seq [length notes]
  (for [offset (iterate (partial + length) 0) [_ t :as n] notes] (update-in n [1] + offset)))

(defn take-beats [n notes] (take-while (fn [[ _ t]] (< t n)) notes))

(defn get-note-seq [scene track] (note-seq (:loop_end (get-loop scene track)) (get-notes scene track)))

(defn get-loop [scene track]
  (query-props (scene-track-path scene track) :loop_start :loop_end :looping :length))

(defn set-loop
  ([scene track end] (set-loop scene track 0.0 end))
  ([scene track start end]
     (control (scene-track-path scene track) (str "set loop_start " start) (str "set loop_end " end) "set looping 1")))

(defn set-matrix [matrix]
  (let [track-keys (map #(-> % val :key) *tracks*)]
    (println track-keys)
    (doseq [[scene {:keys [scene-length] :as m}] matrix]
      (doseq [[track f] (select-keys m track-keys)]
        (set-notes scene track (take-beats scene-length (f)))))))

(def matrix*
     {:intro {:scene-length 16 :synth #(get-note-seq :seed-1 :synth) :drums #(get-note-seq :seed-1 :drums)}
      :groove {:scene-length 4 :synth #(get-note-seq :seed-2 :synth) :drums #(get-note-seq :seed-2 :drums)}})

(with-m4l (set-matrix matrix*))

(comment
  (with-m4l (set-loop 0 0 4.0))
  (with-m4l (set-loop 0 0 8.0))
  (query-props "goto live_set tracks 0 clip_slots 0 clip" :length :loop_start :loop_end :looping :name)

  (with-m4l (get-notes :seed-1 :synth))
  (with-m4l (set-notes 0 0 [[60 0.0 0.5 100 0] [60 1.0 0.5 100 0]]))
  (with-m4l (set-notes 0 0 [[67 0.0 0.5 100 0]]))

  (query "goto live_set tracks 0 clip_slots 0 clip" "call select_all_notes" "call get_selected_notes")
 
  (control "goto live_set" "call start_playing")
  (control "goto live_set" "call stop_playing")
  )

