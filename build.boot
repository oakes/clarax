(set-env!
  :resource-paths #{"src"}
  :dependencies '[[adzerk/boot-cljs "2.1.4" :scope "test"]
                  [adzerk/boot-reload "0.5.2" :scope "test"]
                  [nightlight "RELEASE" :scope "test"]
                  [seancorfield/boot-tools-deps "0.1.4" :scope "test"]]
  :repositories (conj (get-env :repositories)
                  ["clojars" {:url "https://clojars.org/repo/"
                              :username (System/getenv "CLOJARS_USER")
                              :password (System/getenv "CLOJARS_PASS")}]))

(require
  '[clojure.edn :as edn]
  '[adzerk.boot-cljs :refer [cljs]]
  '[adzerk.boot-reload :refer [reload]]
  '[nightlight.boot :refer [nightlight]]
  '[boot-tools-deps.core :refer [deps]])

(task-options!
  pom {:project 'pixel-midi-gogo
       :version "1.0.0-SNAPSHOT"
       :url "https://github.com/oakes/PixiMidiGogo"
       :license {"Public Domain" "http://unlicense.org/UNLICENSE"}
       :dependencies (->> "deps.edn"
                          slurp
                          edn/read-string
                          :deps
                          (reduce
                            (fn [deps [artifact info]]
                              (if-let [version (:mvn/version info)]
                                (conj deps
                                  (transduce cat conj [artifact version]
                                    (select-keys info [:scope :exclusions])))
                                deps))
                            []))}
  push {:repo "clojars"})

(deftask run []
  (set-env! :resource-paths #{"src" "dev-resources"})
  (comp
    (deps :aliases [:cljs])
    (nightlight :port 4000 #_#_:url "http://localhost:3000")
    (watch)
    (reload :asset-path "pixel_midi_gogo")
    (cljs
      :optimizations :none
      :compiler-options {:asset-path "/main.out"})
    (with-pass-thru _
      (require '[pixel-midi-gogo.server :refer [dev-start]])
      ((resolve 'dev-start) {:port 3000 :main-cljs-file "dev-resources/pixel_midi_gogo/app.cljs"}))
    (target)))

(deftask local []
  (comp (pom) (jar) (install)))

(deftask deploy []
  (comp (pom) (jar) (push)))

