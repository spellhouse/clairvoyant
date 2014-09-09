(ns clairvoyant.core
  (:require
   [clojure.walk :as walk]
   [clojure.pprint :as pprint]
   [cljs.analyzer :as analyzer]))

(def ^:dynamic *tracer*)
(def ^:dynamic *trace-depth*)

(defmacro ^:private with-trace-context
  [{:keys [tracer trace-depth trace-data]
    :or {trace-depth 0}}
   & body]
  `(binding [*tracer* ~tracer
             *trace-depth* ~trace-depth]
     ~@body))

(defmacro
  ^{:private true
    :doc "Define one or more methods with the same fn-tail."}
  defmethods [multifn dispatch-vals & fn-body]
  `(do
     ~@(for [dispatch-val dispatch-vals]
         `(defmethod ~multifn ~dispatch-val ~@fn-body))))


(defn ^:private debug-form
  "Throw an exception containing a pretty printed form. Only useful for 
  debugging macros in ClojureScript."
  [form]
  (throw (Exception. (with-out-str (pprint/pprint form)))))

;; Borrowed from schema
;; SEE: https://github.com/Prismatic/schema/blob/9a2f3ab3b12d215300e66fa84c9e1c7070d6654a/src/clj/schema/macros.clj#L13
(defn cljs-env?
  "Take the &env from a macro, and tell whether we are expanding into cljs."
  [env]
  (boolean (:ns env)))

(defn resolve-sym
  [sym env]
  (if (cljs-env? env)
   (if-let [resolved (:name (analyzer/resolve-var env sym))]
     resolved
     sym)
   (if-let [resolved (resolve sym)]
     (let [sym (.sym resolved)
           ns-name (.. resolved ns name)]
       (symbol (str ns-name) (str sym)))
     sym)))

(defn trace-body
  "Given a form and trace data, return the form for a trace life cycle."
  [form trace-data]
  `(let [trace-data# ~trace-data] ;; Cache the initial trace data.
     (when (satisfies? ITraceEnter ~*tracer*)
       (trace-enter ~*tracer* trace-data#))
     (let [;; Creating a nullary function adds an extra call but reduces
           ;; the amount of generated code. It also kills two birds with
           ;; one stone; the trace error and exit steps can occur in the
           ;; same location.
           f# (fn []
                (let [return# ~form]
                  (when (satisfies? ITraceExit ~*tracer*)
                    (trace-exit ~*tracer* (assoc trace-data# :exit return#)))
                  return#))]
       ;; Only setup a try/catch when the programmer expects trace error
       ;; information.
       (if (satisfies? ITraceError ~*tracer*)
         (try
           (f#)
           (catch js/Object e#
             (trace-error ~*tracer* (assoc trace-data# :error e#))
             (throw e#)))
         (f#)))))

(defmulti trace-form
  "Return the trace form for a single form."
  (fn [form env]
    (if (and (seq? form)
             (symbol? (first form)))
      (let [[op & rest] form]
        op)
      form))
  :default ::default)

(defmethod trace-form ::default
  [form env]
  (if (seq? form)
    (cons (first form)
          (doall (for [x (rest form)]
                   (trace-form x env))))
    form))

(defmacro trace-forms
  [{:keys [tracer trace-depth]} & forms]
  (if tracer
    (with-trace-context {:tracer tracer :trace-depth trace-depth}
      (let [traced-forms (doall (for [form forms] (trace-form form &env)))]
        `(do ~@traced-forms)))
    `(do ~@forms)))


;; ---------------------------------------------------------------------
;; Form tracing

;; let

(defn trace-bindings
  [bindings env & [quote-init?]]
  (let [quote-init? (not (false? quote-init?))]
    (doall (mapcat
            (fn [[binding form]]
              (let [trace-data `{:op '~'binding
                                 :form '~binding
                                 :init ~(if quote-init? `'~form form)}]
                `[~binding ~(trace-body (trace-form form env) trace-data)]))
            (partition 2 bindings)))))


(defmethods trace-form ['let* `let]
  [[op bindings & body :as form] env]
  (let [trace-data `{:op '~op
                     :form '~form}
        bindings (trace-bindings bindings env)
        body (doall (for [form body]
                      (trace-form form env)))
        form `(~op ~(vec bindings) ~@body)]
    (trace-body form trace-data)))


;; fn, fn*

(defn variadic? [arglist]
  (boolean (some '#{&} arglist)))

(defn normalize-arglist
  "Removes variation from an argument list."
  [arglist]
  (vec (remove '#{&} arglist)))

(defn munge-arglist
  "Given an argument list create a new one with generated symbols."
  [arglist]
  (vec (for [arg arglist]
         (if (= '& arg)
           arg
           (gensym "a_")))))

(defn condition-map?
  "Returns true if x is a condition map, false otherwise."
  [x]
  (and (map? x)
       (or (vector? (:pre x))
           (vector? (:post x)))))

(defn condition-map-and-body
  "Given a function body, return a vetor of the condition map and 
  the function body."
  [fn-body]
  (let [[x & body] fn-body]
    (if (and (seq body)
             (condition-map? x))
      [x body]
      [nil fn-body])))

(defn trace-fn-spec
  [arglist body trace-data env]
  (let [[condition-map body] (condition-map-and-body body)
        body (doall (for [form body]
                      (trace-form form env)))
        munged-arglist (munge-arglist arglist)
        args (normalize-arglist arglist)
        munged-args (normalize-arglist munged-arglist)
        trace-data (assoc trace-data :arglist `'~arglist)
        bindings (mapcat vector args munged-args)
        fn-form `(fn ~munged-arglist
                   (let [~@(trace-bindings bindings env false)]
                     ((fn []
                        ~condition-map
                        ~@body))))
        form (if (variadic? arglist)
               `(apply ~fn-form ~@munged-args)
               `(~fn-form ~@munged-args))]
    `(~munged-arglist ~(trace-body form trace-data))))

(defn trace-fn
  [form env]
  (let [[op & body] form
        [sym specs] (if (symbol? (first body))
                      [(first body) (rest body)]
                      [(gensym "fn_") body])
        specs (if (every? list? specs)
                specs
                (list specs))
        trace-data `{:op '~op
                     :form '~form
                     :ns '~(.-name *ns*)
                     :name '~sym
                     :anonymous? true}
        specs (doall (for [[arglist & body] specs]
                       (trace-fn-spec arglist body trace-data env)))]
    `(~op ~@specs)))

(defmethods trace-form [`fn 'fn* 'fn]
  [form env]
  (trace-fn form env))


;;;; defn


(defmethods trace-form ['defn `defn]
  [[op & body :as form] env]
  (let [[_ name] (macroexpand-1 form)
        [_ fn-body] (split-with (complement coll?) form)
        [_ & fn-specs] (macroexpand-1 `(fn ~@fn-body)) 
        trace-data `{:op '~op
                     :form '~form
                     :ns '~(.-name *ns*)
                     :name '~name
                     :anonymous? false}
        specs (doall (for [[arglist & body] fn-specs]
                       (trace-fn-spec arglist body trace-data env)))]
    `(def ~name (fn ~@specs))))


;;;; defmethod


(defmethod trace-form [`defmethod 'defmethod]
  [[op multifn dispatch-val & [arglist & body] :as form] env]
  (let [trace-data `{:op '~op
                     :form '~form
                     :ns '~(.-name *ns*)
                     :name '~multifn
                     :dispatch-val '~dispatch-val
                     :arglist '~arglist}]
    `(defmethod ~multifn ~dispatch-val
       ~@(trace-fn-spec arglist body trace-data env))))


;; ---------------------------------------------------------------------
;; Protocol specs


(defn trace-protocol-spec
  [spec-form trace-data env]
  (let [[name arglist & body] spec-form
        trace-data (assoc trace-data
                     :name `'~name
                     :form `'~spec-form
                     :arglist `'~arglist)]
    (cons name (trace-fn-spec arglist body trace-data env))))

(defn trace-reify-body
  [reify-body trace-data env]
  (let [impls (partition-all 2 (partition-by symbol? reify-body))]
    (doall (mapcat
            (fn [protos+specs]
              (let [protos (first protos+specs)
                    proto (last protos)
                    specs (second protos+specs)
                    trace-data (assoc trace-data
                                 :protocol `'~(resolve-sym proto env))
                    specs (doall (for [spec specs]
                                   (trace-protocol-spec spec trace-data env)))]
                `(~@protos ~@specs)))
            impls))))


;;;; reify


(defmethods trace-form [`reify 'reify]
  [[op & body :as form] env]
  (let [trace-data `{:op '~op
                     :form '~form
                     :ns '~(.-name *ns*)}]
    `(~op ~@(trace-reify-body body trace-data env))))


;;;; extend-type


(defmethods trace-form [`extend-type 'extend-type]
  [[op type & specs :as form] env]
  (let [trace-data `{:op '~op
                     :form '~form
                     :ns '~(.-name *ns*)}]
    `(~op ~type ~@(trace-reify-body specs trace-data env))))


;;;; extend-protocol


(defmethods trace-form [`extend-protocol 'extend-protocol]
  [[op proto & specs :as form] env]
  (let [trace-data `{:op '~op
                     :form '~form
                     :ns '~(.-name *ns*)}
        fake-specs (trace-reify-body
                    (for [x specs]
                      (if (symbol? x)
                        proto
                        x))
                    trace-data
                    env)
        real-specs (loop [fake-specs fake-specs
                          types (filter symbol? specs)
                          new-specs []]
                     (case [(boolean (seq fake-specs))
                            (boolean (seq types))]
                       [true true]
                       (let [x (first fake-specs)]
                         (if (symbol? x)
                           (recur (next fake-specs)
                                  (next types)
                                  (conj new-specs (first types)))
                           (recur (next fake-specs)
                                  types
                                  (conj new-specs (first fake-specs)))))

                       [true false]
                       (seq (into new-specs fake-specs))

                       :else
                       (seq new-specs)))]
    `(~op ~(resolve-sym proto env) ~@real-specs)))
