(defn read-deps-edn [aliases-to-include]
  (let [{:keys [paths deps aliases]} (-> "deps.edn" slurp clojure.edn/read-string)
        deps (->> (select-keys aliases aliases-to-include)
                  vals
                  (mapcat :extra-deps)
                  (into deps)
                  (reduce
                    (fn [deps [artifact info]]
                      (if-let [version (:mvn/version info)]
                        (conj deps
                          (transduce cat conj [artifact version]
                            (select-keys info [:scope :exclusions])))
                        deps))
                    []))]
    {:dependencies deps
     :source-paths (set paths)
     :resource-paths (set paths)}))

(let [{:keys [source-paths resource-paths dependencies]} (read-deps-edn [])]
  (set-env!
    :source-paths source-paths
    :resource-paths resource-paths
    :dependencies (into '[[adzerk/boot-cljs "2.1.5" :scope "test"]
                          [adzerk/boot-reload "0.6.0" :scope "test"]
                          [nightlight "RELEASE" :scope "test"]]
                        dependencies)
    :repositories (conj (get-env :repositories)
                    ["clojars" {:url "https://clojars.org/repo/"
                                :username (System/getenv "CLOJARS_USER")
                                :password (System/getenv "CLOJARS_PASS")}])))

(require
  '[adzerk.boot-cljs :refer [cljs]]
  '[adzerk.boot-reload :refer [reload]]
  '[nightlight.boot :refer [nightlight]])

(task-options!
  pom {:project 'pixel-midi-gogo
       :version "1.0.0-SNAPSHOT"
       :url "https://github.com/oakes/PixiMidiGogo"
       :license {"Public Domain" "http://unlicense.org/UNLICENSE"}}
  push {:repo "clojars"})

(deftask run []
  (set-env!
    :dependencies #(into (set %) (:dependencies (read-deps-edn [:cljs])))
    :resource-paths #(conj % "dev-resources"))
  (comp
    (nightlight :port 4000 #_#_:url "http://localhost:3000")
    (watch)
    (reload :asset-path "public")
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

