(ns clairvoyant.core)

;; ---------------------------------------------------------------------
;; Protocols

(defprotocol ITraceEnter
  (-trace-enter [this trace-data]))

(defprotocol ITraceError
  (-trace-error [this trace-data]))

(defprotocol ITraceExit
  (-trace-exit [this trace-data]))

;; ---------------------------------------------------------------------
;; API

(defn trace-enter
  "Call the -trace-enter method for a given tracer with trace-data.
  tracer must satisfy ITraceEnter."
  [tracer trace-data]
  (-trace-enter tracer trace-data))

(defn trace-error
  "Call the -trace-error method for a given tracer with trace-data.
  tracer must satisfy ITraceError."
  [tracer trace-data]
  (-trace-error tracer trace-data))

(defn trace-exit
  "Call the -trace-exit method for a given tracer with trace-data.
  tracer must satisfy ITraceExit."
  [tracer trace-data]
  (-trace-exit tracer trace-data))

;; ---------------------------------------------------------------------
;; Core tracers 

(def default-tracer
  (reify
    ITraceEnter
    (-trace-enter [_ {:keys [ns name args arglist protocol dispatch-val anonymous?]}]
      (.groupCollapsed js/console
        (if protocol
          (str protocol " " name " " arglist)
          (str ns "/" name
               " "
               (when dispatch-val (str (pr-str dispatch-val) " "))
               arglist
               (when anonymous? " (anonymous)"))))
      (let [arglist (remove '#{&} arglist)]
        (doseq [[sym val] (map vector arglist args)]
          (.log js/console (str sym) "=" val))))

    ITraceExit
    (-trace-exit [_ {:keys [exit]}]
      (.log js/console exit)
      (.groupEnd js/console))

    ITraceError
    (-trace-error [_ {:keys [form error]}]
      (.error js/console (.-stack error))
      (.groupEnd js/console))))
