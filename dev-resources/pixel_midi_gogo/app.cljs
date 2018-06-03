(ns pixel-midi-gogo.app
  (:require [pixel-midi-gogo.core]
            [pixel-midi-gogo.view :refer [View map->View]]
            [pixel-midi-gogo.event :refer [Event]]
            [pixel-midi-gogo.canvas :refer [Canvas map->Canvas]]
            [clara.rules :as rules])
  (:require-macros [pixel-midi-gogo.core :as pmg]))

(defrecord TodoItem [text])

(defrecord NewTodo [text timestamp])

(defrecord EditTodo [text record timestamp])

(defrecord Mouse [x y])

(pmg/init
  [pixel-midi-gogo.core
   pixel-midi-gogo.view
   pixel-midi-gogo.event
   pixel-midi-gogo.canvas]
  ["dev-resources/pixel_midi_gogo/app.edn"])

