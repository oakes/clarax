(require
  '[orchestra.spec.test :as st]
  '[expound.alpha :as expound]
  '[clojure.spec.alpha :as s]
  '[figwheel.main :as figwheel]
  '[nightlight.core :as nightlight]
  '[pixel-midi-gogo.server :as server])

(server/dev-start {:port 3000 :main-cljs-file "dev-resources/pixel_midi_gogo/app.cljc"})
(st/instrument)
(alter-var-root #'s/*explain-out* (constantly expound/printer))
(nightlight/start {:port 4000})
(figwheel/-main "--build" "dev")

