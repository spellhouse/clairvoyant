(ns clairvoyant.core
  (:require [clojure.walk :refer [prewalk]]))

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

(def ^:private fn-re
  "Matches the function signature of the result of (str f) where f is a
  function. Captures the argument list and identifier (if possible)."
  #"function\s+([a-zA-Z0-9_$]+)?\(([a-zA-Z0-9_,\s]+)\)")

(defn- fn-signature
  "Return the function signature of f.

  Example:

    (fn-signature inc)
    ;;=> (fn inc [x])

    (fn-signature map)
    ;; => (fn [f c1 c2 c3 var_args])
  "
  [f]
  (let [[_ name sig] (re-find fn-re (str f))
        arglist (mapv symbol (.split (str sig) ","))]
    (if name
      (list 'fn (symbol name) arglist)
      (list 'fn arglist))))


(def default-tracer
  (let [pr-val (fn [x]
                 (cond 
                   (fn? x) 
                   (pr-str (fn-signature x))
                   (coll? x)
                   (pr-str (prewalk (fn [x]
                                            (if (fn? x)
                                              (fn-signature x)
                                              x)) 
                                          x))
                   :else (pr-str x)))
        log-binding (fn [form init]
                      (.groupCollapsed js/console "%c%s %c%s"
                                       "font-weight:bold;"
                                       (pr-str form)
                                       "font-weight:normal;"
                                       (pr-val init)))
        log-exit (fn [exit]
                   (.groupCollapsed js/console "=>" (pr-val exit))
                   (.log js/console exit)
                   (.groupEnd js/console))
        has-bindings? #{'fn*
                        `fn
                        'fn
                        'defn
                        `defn
                        'defmethod
                        `defmethod
                        'deftype
                        `deftype
                        'defrecord
                        `defrecord
                        'reify
                        `reify
                        'let
                        `let
                        'extend-type
                        `extend-type
                        'extend-protocol
                        `extend-protocol}
        fn-like? (disj has-bindings? 'let `let)]
    (reify
      ITraceEnter
      (-trace-enter
        [_ {:keys [anonymous? arglist args dispatch-val form init name ns op protocol]}]
        (cond
         (fn-like? op)
         (let [title (if protocol
                       (str protocol " " name " " arglist)
                       (str ns "/" name
                            (when dispatch-val
                              (str " " (pr-str dispatch-val)))
                            (str " " arglist)
                            (when anonymous? " (anonymous)")))
               arglist (remove '#{&} arglist)]
           (.groupCollapsed js/console title)
           (.groupCollapsed js/console "bindings"))
         
         (#{'let `let} op)
         (let [title (str op)]
           (.groupCollapsed js/console title)
           (.groupCollapsed js/console "bindings"))

         (#{'binding} op)
         (log-binding form init)))

      ITraceExit
      (-trace-exit [_ {:keys [op exit]}]
        (cond
         (#{'binding} op)
         (do (log-exit exit)
             (.groupEnd js/console))

         (has-bindings? op)
         (do (.groupEnd js/console)
             (log-exit exit)
             (.groupEnd js/console))))

      ITraceError
      (-trace-error [_ {:keys [op form error ex-data]}]
        (cond
         (#{'binding} op)
         (do
           (.error js/console (.-stack error))
           (when ex-data
             (.groupCollapsed js/console "ex-data")
             (.groupCollapsed js/console (pr-val ex-data))
             (.log js/console ex-data)
             (.groupEnd js/console)
             (.groupEnd js/console)))

         (has-bindings? op)
         (do (.groupEnd js/console)
             (do
               (.error js/console (.-stack error))
               (when ex-data
                 (.groupCollapsed js/console "ex-data")
                 (.groupCollapsed js/console (pr-val ex-data))
                 (.log js/console ex-data)
                 (.groupEnd js/console)
                 (.groupEnd js/console)))
             (.groupEnd js/console)))))))
