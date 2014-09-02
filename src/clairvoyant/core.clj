(ns clairvoyant.core
  (:require
   [clojure.walk :as walk]
   [cljs.analyzer :as analyzer]))

(def ^:dynamic *tracer*)
(def ^:dynamic *trace-depth* 0)

(defmacro ^:private with-trace-context
  [{:keys [tracer trace-depth trace-data]
    :or {trace-depth 0}}
   & body]
  `(binding [*tracer* ~tracer
             *trace-depth* ~trace-depth]
     ~@body))

;; ---------------------------------------------------------------------
;; Form tracing

(defn trace-body
  [form trace-data]
  `((when (satisfies? ITraceEnter ~*tracer*)
      (trace-enter ~*tracer* ~trace-data))
    (let [return# (if (satisfies? ITraceError ~*tracer*)
                    (try
                      ~form
                      (catch js/Object e#
                        (trace-error ~*tracer* (assoc ~trace-data :error e#))))
                    ~form)]
      (when (satisfies? ITraceExit ~*tracer*)
        (trace-exit ~*tracer* (assoc ~trace-data :exit return#)))
      return#)))

(defmulti trace-form
  (fn [[op & rest]] op)
  :default ::default)

(defmethod trace-form ::default
  [form] form)

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
  [arglist body trace-data]
  (let [[condition-map body] (condition-map-and-body body)
        munged-arglist (munge-arglist arglist)
        args (normalize-arglist arglist)
        munged-args (normalize-arglist munged-arglist)
        trace-data (-> trace-data
                       (assoc :arglist `'~arglist)
                       (assoc :args `~munged-args))
        form `((fn ~munged-arglist
                 (let ~(vec (interleave args munged-args))
                   ((fn []
                      ~condition-map
                      ~@body))))
               ~@munged-args)]
    `(~munged-arglist ~@(trace-body form trace-data))))

(defn trace-fn
  [form]
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
        specs (for [[arglist & body] specs]
                (trace-fn-spec arglist body trace-data))]
    `(~op ~@(doall specs))))

(defmethod trace-form 'fn*
  [form] (trace-fn form))

(defmethod trace-form 'fn
  [form] (trace-fn form))

(defmethod trace-form 'defn
  [form]
  (let [[_ name & [head & _ :as fn-forms]] (macroexpand-1 form)
        fn-body (if-not (vector? head) fn-forms (list fn-forms))
        trace-data `{:op 'defn
                     :form '~form
                     :ns '~(.-name *ns*)
                     :name '~name
                     :anonymous? false}
        specs (for [[arglist & body] fn-body]
                (trace-fn-spec arglist body trace-data))]
    `(def ~name
       (fn ~@(doall specs)))))

(defmethod trace-form 'defmethod
  [form]
  (let [[op multifn dispatch-val & [arglist & body]] form
        trace-data `{:op '~op
                     :form '~form
                     :ns '~(.-name *ns*)
                     :name '~multifn
                     :dispatch-val '~dispatch-val
                     :arglist '~arglist}]
    `(defmethod ~multifn ~dispatch-val
       ~@(trace-fn-spec arglist body trace-data))))

(defn trace-protocol-spec
  [spec-form trace-data]
  (let [[name arglist & body] spec-form
        trace-data (assoc trace-data
                     :name `'~name
                     :form `'~spec-form
                     :arglist `'~arglist)]
    (cons name (trace-fn-spec arglist body trace-data))))

(defmethod trace-form 'reify
  [form]
  (let [[op & body] form
        impls (partition-all 2 (partition-by symbol? body))
        trace-data `{:op '~op
                     :form '~form
                     :ns '~(.-name *ns*)}]
    `(reify
       ~@(mapcat
           (fn [proto+specs]
             (let [proto (ffirst proto+specs)
                   specs (second proto+specs)
                   trace-data (assoc trace-data :protocol `'~proto)
                   specs (for [spec specs]
                           (trace-protocol-spec spec trace-data))]
               `(~proto ~@(doall specs))))
           impls))))

;; ---------------------------------------------------------------------
;; Symbol expansion

(def expansion-form?
  '#{reify})

(defmulti expand-symbols
  (fn [[op & rest] env] op)
  :default ::default)

(defmethod expand-symbols 'reify
  [[op & rest] env]
  (cons op (map
            (fn [x]
              (if-let [resolved (and (symbol? x)
                                     (analyzer/resolve-var env x))]
                (:name resolved)
                x))
            rest)))

(defmethod expand-symbols ::default
  [form _] form)

(defn expand-form
  [form env]
  (walk/prewalk
   (fn [x]
     (if (and (list? x)
              (expansion-form? (first x)))
       (expand-symbols x env)
       x))
   form))

(defmacro trace-forms
  [{:keys [tracer trace-depth]} & forms]
  (if tracer
    (with-trace-context {:tracer tracer :trace-depth trace-depth}
      (let [expanded (expand-form forms &env)]
        `(do ~@(doall (map trace-form expanded)))))
    `(do ~@forms)))
