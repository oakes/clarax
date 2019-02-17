(require
  '[cljs.build.api :as api]
  '[orchestra.spec.test :as st]
  '[expound.alpha :as expound]
  '[clojure.spec.alpha :as s]
  '[nightlight.core :as nightlight]
  '[pixel-midi-gogo.server :as server])

(server/dev-start {:port 3000 :main-cljs-file "dev-resources/pixel_midi_gogo/app.cljc"})
(st/instrument)
(alter-var-root #'s/*explain-out* (constantly expound/printer))
(nightlight/start {:port 4000})

(println "Building main.js")
(api/build "src" {:main          'pixel-midi-gogo.dev
                  :optimizations :none
                  :output-to     "target/public/main.js"
                  :output-dir    "target/public/main.out"
                  :asset-path    "/main.out"})
(println "Launching window")
(-> "classes" java.io.File. .mkdir)
(compile 'pixel-midi-gogo.init)
(require '[pixel-midi-gogo.init :refer [dev-main]])
(dev-main)

