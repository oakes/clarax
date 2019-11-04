(ns play-cljc.state.macros-js
  (:require [play-cljc.state.build :as build]
            [cljs.env :as env]
            [clara.macros :as macros]
            [clara.rules :as rules]))

(def ^:private ^:const ns-sym 'play-cljc.state)

(defn- cljs-fn
  ([val]
   (swap! env/*compiler* assoc-in [:clara.macros/productions ns-sym] val))
  ([key val]
   (swap! env/*compiler* assoc-in [:clara.macros/productions ns-sym key] val)))

(defmacro defsession [var-name & forms]
  (binding [build/*cljs-fn* cljs-fn]
    (let [{:keys [init-forms]} (build/forms->rules var-name forms)
          session (macros/sources-and-options->session-assembly-form [(list 'quote ns-sym)])]
      `(let [var# (def ~var-name (atom nil))]
         (->> ~session ~@init-forms rules/fire-rules (reset! ~var-name))
         var#))))

