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

(defmacro init-state [*session & forms]
  (binding [build/*cljs-fn* cljs-fn]
    (let [{:keys [init-forms rules]} (build/forms->rules *session forms)
          session (macros/sources-and-options->session-assembly-form [(list 'quote ns-sym)])]
      `(->> ~session ~@init-forms rules/fire-rules (reset! ~*session)))))
