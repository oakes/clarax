(ns pixel-midi-gogo.app
  (:require [pixel-midi-gogo.core :refer [Def map->Def]]
            [pixel-midi-gogo.view :refer [View map->View]]
            [pixel-midi-gogo.event :refer [Event]]
            [clara.rules :as rules]
            [clara.rules.accumulators :refer [all]])
  (:require-macros [pixel-midi-gogo.core :refer [init]]))

(defrecord Person [name email])

(defrecord ListItem [text])

(init
  [pixel-midi-gogo.core pixel-midi-gogo.view pixel-midi-gogo.event]
  ["app.edn"])

