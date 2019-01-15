(ns pixel-midi-gogo.canvas
  (:require [pixel-midi-gogo.core :as pmg-core]
            #?(:cljs [pixel-midi-gogo.canvas-client :as client])))

(defrecord Canvas [parent value timestamp])

(extend-type Canvas
  pmg-core/ActionReceivable
  (receive-action! [this action-name]
    (case action-name
      #?@(:cljs ["insert" (client/insert! this)])
      (pmg-core/receive-action* this action-name))))

