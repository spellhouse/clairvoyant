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
  (letfn [(log-binding [form init]
            (.groupCollapsed js/console "%c%s %c%s"
                             "font-weight:bold;"
                             (pr-str form)
                             "font-weight:normal;"
                             (pr-str init)))
          (has-bindings? [op]
            #{'fn*
              `fn
              'fn
              'defn
              `defn
              'defmethod
              `defmethod
              'reify
              `reify
              'let
              `let
              'extend-type
              `extend-type
              'extend-protocol
              `extend-protocol} op)]
    (reify
      ITraceEnter
      (-trace-enter
        [_ {:keys [anonymous?
                   arglist
                   args
                   dispatch-val
                   form
                   init
                   name
                   ns
                   op
                   protocol]}]
        (cond
         ;; fn-like
         (#{'fn*
            `fn
            'fn
            'defn
            `defn
            'defmethod
            `defmethod
            'reify
            `reify
            'extend-type
            `extend-type
            'extend-protocol
            `extend-protocol} op)
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
         (do (.info js/console exit)
             (.groupEnd js/console))

         (has-bindings? op)
         (do (.groupEnd js/console)
             (.info js/console exit)
             (.groupEnd js/console))))

      ITraceError
      (-trace-error [_ {:keys [op form error]}]
        (cond
         (#{'binding} op)
         (.error js/console (.-stack error))

         (has-bindings? op)
         (do (.groupEnd js/console)
             (.error js/console (.-stack error))
             (.groupEnd js/console)))))))
