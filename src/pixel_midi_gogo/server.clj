(ns pixel-midi-gogo.server
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [ring.middleware.resource :refer [wrap-resource]]
            [ring.middleware.file :refer [wrap-file]]
            [ring.middleware.reload :refer [wrap-reload]]
            [ring.middleware.content-type :refer [wrap-content-type]]
            [ring.middleware.keyword-params :refer [wrap-keyword-params]]
            [ring.middleware.params :refer [wrap-params]]
            [ring.util.response :refer [redirect not-found]]
            [ring.util.request :refer [body-string]]
            [org.httpkit.server :refer [run-server]]
            [rum.core :as rum]
            [clojure.tools.cli :as cli]))

(defonce *web-server (atom nil))
(defonce *options (atom nil))
(defonce *files (atom {}))

(defn handler [{:keys [uri] :as request}]
  (or (case uri
        "/"
        {:status 200
         :headers {"Content-Type" "text/html"}
         :body (-> "public/index.html" io/resource slurp)}
        "/watch"
        (do
          (->> request :body .bytes slurp
               edn/read-string
               (reduce
                 (fn [m filename]
                   (assoc m filename (slurp filename)))
                 {})
               (reset! *files))
          {:status 200})
        nil)
      (not-found "Page not found")))

(defn print-server [server]
  (println
    (str "Started PixelMidiGogo on http://localhost:"
      (-> server meta :local-port)
      "/index.html"))
  server)

(defn start
  ([opts]
   (-> handler
       (wrap-resource "public")
       (start opts)))
  ([app {:keys [main-cljs-file] :as opts}]
   (when main-cljs-file
     (let [files (reduce
                   (fn [m filename]
                     (assoc m filename (slurp filename)))
                   {}
                   (keys @*files))]
       (when (not= @*files files)
         (-> main-cljs-file (io/file) (.setLastModified (System/currentTimeMillis)))
         (reset! *files files))))
   (when-not @*web-server
     (->> (merge {:port 0} opts)
          (reset! *options)
          (run-server (-> app #_wrap-content-type wrap-params wrap-keyword-params))
          (reset! *web-server)
          print-server))))

(defn dev-start [opts]
  (.mkdirs (io/file "target" "public"))
  (start (-> #'handler
             (wrap-reload)
             (wrap-file "target/public"))
    opts))

(def cli-options
  [["-p" "--port PORT" "Port number"
    :default 5000
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 0 % 0x10000) "Must be an integer between 0 and 65536"]]
   [nil "--host HOST" "The hostname that PixelMidiGogo listens on"
    :default "0.0.0.0"]
   ["-u" "--usage" "Show CLI usage options"]])

(defn -main [& args]
  (let [cli (cli/parse-opts args cli-options)]
    (cond
      ;; if there are CLI errors, print error messages and usage summary
      (:errors cli)
      (println (:errors cli) "\n" (:summary cli))
      ;; if user asked for CLI usage, print the usage summary
      (get-in cli [:options :usage])
      (println (:summary cli))
      ;; in other cases start PixelMidiGogo
      :otherwise
      (start (:options cli)))))

