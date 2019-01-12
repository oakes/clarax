(ns pixel-midi-gogo.core
  (:require #?(:clj [clojure.edn :as edn]
               :cljs [cljs.tools.reader.edn :as edn])
            [clara.rules :as rules]
            [clara.rules.engine :as engine]
            [clara.rules.accumulators]
            [clojure.string :as str]
            #?(:cljs [goog.object :as gobj]))
  #?(:cljs (:import goog.net.XhrIo)))

(defonce *session (atom nil))
(defonce *readers (atom {'object (constantly nil)
                         'js (constantly nil)}))
#?(:clj (defonce *engine (atom nil)))

(defn init-readers [readers]
  (let [readers (reduce
                  (fn [readers [k v]]
                    (-> readers
                        (assoc (-> k pr-str (str/replace "-" "_") symbol) v)
                        (assoc (-> k pr-str (str/replace "_" "-") symbol) v)
                        (assoc (-> k pr-str
                                   (str/replace "_" "-")
                                   (str/replace "/" ".") symbol) v)
                        (assoc (-> k pr-str
                                   (str/replace "-" "_")
                                   (str/replace "/" ".") symbol) v)))
                  {}
                  readers)]
    (swap! *readers merge readers)))

(defn insert*
  ([fact]
   (rules/insert! fact))
  ([session fact]
   (if engine/*rule-context*
     (do (insert* fact) session)
     (-> session
         (rules/insert fact)
         rules/fire-rules))))

(defmulti insert (fn [& args]
                   (-> args last type)))

(defmethod insert
  :default
  [& args]
  (apply insert* args))

(defmulti action (fn [& args]
                   (first args)))

(defn delete
  ([fact]
   (rules/retract! fact))
  ([session fact]
   (if engine/*rule-context*
     (do (delete fact) session)
     (-> session
         (rules/retract fact)
         rules/fire-rules))))

(defn edit
  ([fact new-args]
   (rules/retract! fact)
   (rules/insert! (merge fact new-args)))
  ([session fact new-args]
   (if engine/*rule-context*
     (do (edit fact new-args) session)
     (-> session
         (rules/retract fact)
         (rules/insert (merge fact new-args))
         rules/fire-rules))))

#?(:cljs [(defn watch-files [files]
            (when-not js/COMPILED
              (.send XhrIo
                "/watch"
                (fn [])
                "POST"
                (pr-str files))))
          
          (doto js/window
            (gobj/set "onAction"
              (fn [action-name data]
                (action action-name (edn/read-string {:readers @*readers} data)))))])

(defn send-action [action-name data]
  #?(:cljs (if js/window.java
             (.onaction js/window.java action-name (pr-str data))
             (action action-name data))
     :clj (-> @*engine
              (.executeScript "window")
              (.call "onAction" (into-array [action-name (pr-str data)])))))

(defn get-time []
  #?(:cljs (.getTime (js/Date.))
     :clj (System/currentTimeMillis)))

