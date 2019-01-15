(ns pixel-midi-gogo.core
  (:require [clara.rules :as rules]
            [clara.rules.engine :as engine]
            [clara.rules.accumulators]
            #?(:cljs [pixel-midi-gogo.utils :as utils]))
  #?(:cljs (:import goog.net.XhrIo)))

(defonce *session (atom nil))

(defprotocol Insertable
  (insert! [this] [this session]))

(defprotocol Deletable
  (delete! [this] [this session]))

(defprotocol Updatable
  (update! [this new-args] [this new-args session]))

(defprotocol Upsertable
  (upsert! [this new-args query] [this new-args query session]))

(defprotocol ActionReceivable
  (receive-action! [this action-name]))

(defprotocol ActionSendable
  (send-action! [this action-name]))

(defn insert*
  ([fact]
   (rules/insert! fact))
  ([fact session]
   (if engine/*rule-context*
     (do (insert* fact) session)
     (-> session
         (rules/insert fact)
         rules/fire-rules))))

(defn delete*
  ([fact]
   (rules/retract! fact))
  ([fact session]
   (if engine/*rule-context*
     (do (delete* fact) session)
     (-> session
         (rules/retract fact)
         rules/fire-rules))))

(defn update*
  ([fact new-args]
   (rules/retract! fact)
   (insert! (merge fact new-args)))
  ([fact new-args session]
   (if engine/*rule-context*
     (do (update* fact new-args) session)
     (->> (rules/retract session fact)
          (insert! (merge fact new-args))))))

(defn upsert*
  ([fact new-args query]
   (or (some-> @*session
               (rules/query query) first :?ret
               (update! new-args))
       (insert! fact)))
  ([fact new-args query session]
   (or (some-> session
               (rules/query query) first :?ret
               (update! new-args session))
       (insert! fact session))))

(defmulti code->result identity)

(defn receive-action* [this action-name]
  (case action-name
    "insert" (swap! *session (partial insert! this))
    "update" (swap! *session (partial update! (:old this) (:new this)))
    #?@(:cljs ["eval" (pr-str (utils/obj->clj (code->result this) 0))])))

(extend-type #?(:clj Object :cljs default)
  Insertable
  (insert!
    ([this] (insert* this))
    ([this session] (insert* this session)))
  Deletable
  (delete!
    ([this] (delete* this))
    ([this session] (delete* this session)))
  Updatable
  (update!
    ([this new-args] (update* this new-args))
    ([this new-args session] (update* this new-args session)))
  Upsertable
  (upsert!
    ([this new-args query] (upsert* this new-args query))
    ([this new-args query session] (upsert* this new-args query session)))
  ActionReceivable
  (receive-action! [this action-name]
    (receive-action* this action-name))
  ActionSendable
  (send-action! [this action-name]
    (receive-action! this action-name)))

#?(:cljs (defn watch-files [files]
           (when-not js/COMPILED
             (.send XhrIo
               "/watch"
               (fn [])
               "POST"
               (pr-str files)))))

(defn get-time []
  #?(:cljs (.getTime (js/Date.))
     :clj (System/currentTimeMillis)))

