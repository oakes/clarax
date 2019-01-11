(ns pixel-midi-gogo.app
  (:require [clojure.java.io :as io])
  (:import [javafx.application Application]
           [javafx.fxml FXMLLoader]
           [javafx.stage Stage]
           [javafx.scene Scene]
           [javafx.event EventHandler])
  (:gen-class :extends javafx.application.Application))

(definterface Bridge
  (onload []))

(defn -start [^pixel_midi_gogo.app app ^Stage stage]
  (let [root (FXMLLoader/load (io/resource "main.fxml"))
        scene (Scene. root 1242 768)
        webview (.lookup scene "#webview")
        engine (.getEngine webview)
        bridge (reify Bridge
                 (onload [this]
                   (println "loaded")))]
    (doto stage
      (.setTitle "PixelMidiGogo")
      (.setScene scene)
      (.show))
    (.setContextMenuEnabled webview false)
    (.setOnStatusChanged engine
      (reify EventHandler
        (handle [this event])))
    (-> engine
        (.executeScript "window")
        (.setMember "java" bridge))
    (.load engine "http://localhost:3000/")))

(defn -main [& args]
  (when (= "Linux" (System/getProperty "os.name"))
    (System/setProperty "prism.lcdtext" "false")
    (System/setProperty "prism.text" "t2k"))
  (Application/launch pixel_midi_gogo.app (into-array String args)))

(defn dev-main []
  (-main))

