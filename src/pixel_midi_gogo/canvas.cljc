(ns pixel-midi-gogo.canvas
  #?(:cljs (:require [pixel-midi-gogo.canvas-client])))

(defrecord Canvas [parent value timestamp])

