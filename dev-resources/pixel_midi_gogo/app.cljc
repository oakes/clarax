(ns pixel-midi-gogo.app
  (:require [pixel-midi-gogo.core :as pmg-core]
            [pixel-midi-gogo.view #?@(:cljs [:refer [View map->View]]
                                      :clj  [:refer [map->View]])]
            [pixel-midi-gogo.event #?@(:cljs [:refer [Event map->Event]]
                                       :clj [:refer [map->Event]])]
            [pixel-midi-gogo.canvas #?@(:cljs [:refer [Canvas map->Canvas]]
                                        :clj [:refer [map->Canvas]])]
            [clara.rules :as rules]
            [clojure.string :as str]
            #?@(:clj [[clojure.edn :as edn]
                      [pixel-midi-gogo.build :as build]]
                :cljs [[cljs.tools.reader.edn :as edn]
                       [goog.object :as gobj]
                       [pixel-midi-gogo.utils :as utils]]))
  #?(:clj (:import [pixel_midi_gogo.view View]
                   [pixel_midi_gogo.event Event]
                   [pixel_midi_gogo.canvas Canvas]))
  #?(:cljs (:require-macros [pixel-midi-gogo.build :as build])))

(defrecord TodoItem [text])

(defrecord NewTodo [text timestamp])

(defrecord EditTodo [text record timestamp])

(defrecord Mouse [x y])

(def current-ns *ns*)

(defn normalize-nses [readers]
  (reduce
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
    readers))

(def readers (normalize-nses
               {View map->View
                Event map->Event
                TodoItem map->TodoItem
                NewTodo map->NewTodo
                EditTodo map->EditTodo
                Mouse map->Mouse
                Canvas map->Canvas
                'object (constantly nil)
                'js (constantly nil)}))

#?(:cljs [(doto js/window
            (gobj/set "onAction"
              (fn [record action-name]
                (pmg-core/receive-action! (edn/read-string {:readers readers} record) action-name))))
          (when js/window.java
            (extend-type default
              pmg-core/ActionSendable
              (send-action! [this action-name]
                (.onaction js/window.java (pr-str this) action-name))))])

(defn init [init?]
  (#?(:clj build/init-clj :cljs build/init-cljs)
    init?
    #?(:cljs [pixel-midi-gogo.core
              pixel-midi-gogo.view
              pixel-midi-gogo.event
              pixel-midi-gogo.canvas]
       :clj current-ns)
    ["dev-resources/pixel_midi_gogo/app.edn"]))

#?(:cljs (do
           (when js/window.java
             (set! (.-onload js/window)
               (fn []
                 ; hack thanks to http://stackoverflow.com/a/28414332/1663009
                 (set! (.-status js/window) "MY-MAGIC-VALUE")
                 (set! (.-status js/window) "")
                 (.onload js/window.java))))
           (init (nil? js/window.java))))

