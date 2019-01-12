(ns pixel-midi-gogo.app
  (:require [pixel-midi-gogo.core :as pmg-core]
            [pixel-midi-gogo.view #?@(:cljs [:refer [View map->View]]
                                      :clj  [:refer [map->View]])]
            [pixel-midi-gogo.event #?@(:cljs [:refer [Event map->Event]]
                                       :clj [:refer [map->Event]])]
            [pixel-midi-gogo.canvas #?@(:cljs [:refer [Canvas map->Canvas]]
                                        :clj [:refer [map->Canvas]])]
            [clara.rules :as rules]
            #?(:clj [pixel-midi-gogo.build :as build]))
  #?(:clj (:import [pixel_midi_gogo.view View]
                   [pixel_midi_gogo.event Event]
                   [pixel_midi_gogo.canvas Canvas]))
  #?(:cljs (:require-macros [pixel-midi-gogo.build :as build])))

(defrecord TodoItem [text])

(defrecord NewTodo [text timestamp])

(defrecord EditTodo [text record timestamp])

(defrecord Mouse [x y])

(def current-ns *ns*)

(pmg-core/init-readers
  {View map->View
   Event map->Event
   TodoItem map->TodoItem
   NewTodo map->NewTodo
   EditTodo map->EditTodo
   Mouse map->Mouse
   Canvas map->Canvas})

(defn init []
  (#?(:clj build/init-clj :cljs build/init-cljs)
    #?(:cljs [pixel-midi-gogo.core
              pixel-midi-gogo.view
              pixel-midi-gogo.event
              pixel-midi-gogo.canvas]
       :clj current-ns)
    ["dev-resources/pixel_midi_gogo/app.edn"]))

#?(:cljs (if js/window.java
           (set! (.-onload js/window)
             (fn []
               ; hack thanks to http://stackoverflow.com/a/28414332/1663009
               (set! (.-status js/window) "MY-MAGIC-VALUE")
               (set! (.-status js/window) "")
               (.onload js/window.java)))
           (init)))

