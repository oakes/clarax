(ns pixel-midi-gogo.init
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [pixel-midi-gogo.core :as pmg-core]
            [pixel-midi-gogo.app :as app])
  (:import [javafx.application Application]
           [javafx.fxml FXMLLoader]
           [javafx.stage Stage]
           [javafx.scene Scene]
           [javafx.event EventHandler])
  (:gen-class :extends javafx.application.Application))

(definterface Bridge
  (onload [])
  (onaction [action-name data]))

(defn -start [^pixel_midi_gogo.init app ^Stage stage]
  (let [root (FXMLLoader/load (io/resource "main.fxml"))
        scene (Scene. root 1242 768)
        webview (.lookup scene "#webview")
        engine (.getEngine webview)
        bridge (reify Bridge
                 (onload [this]
                   (try
                     (app/init)
                     (catch Exception e (.printStackTrace e) (throw e))))
                 (onaction [this action-name data]
                   (try
                     (pmg-core/action action-name
                       (edn/read-string {:readers @pmg-core/*readers} data))
                     (catch Exception e (.printStackTrace e) (throw e)))))]
    (reset! pmg-core/*engine engine)
    (doto stage
      (.setTitle "PixelMidiGogo")
      (.setScene scene)
      (.show))
    (.setContextMenuEnabled webview false)
    (.setOnStatusChanged engine
      (reify EventHandler
        (handle [this event]
          (-> engine
              (.executeScript "window")
              (.setMember "java" bridge)))))
    (.load engine "http://localhost:3000/")))

(defn -main [& args]
  (when (= "Linux" (System/getProperty "os.name"))
    (System/setProperty "prism.lcdtext" "false")
    (System/setProperty "prism.text" "t2k"))
  (Application/launch pixel_midi_gogo.init (into-array String args)))

(defn dev-main []
  (-main))

