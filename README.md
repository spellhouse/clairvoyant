# Clairvoyant

Clairvoyant is a library that offers flexible tracing for ClojureScript.

## Usage

Add Clairvoyant to your project `:dependencies`.

![Clojars Project](http://clojars.org/spellhouse/clairvoyant/latest-version.svg)

This library is alpha quality.

## Example

```clj
(ns foo
  (:require [clairvoyant.core :as trace :include-macros true]))


(trace/trace-forms {:tracer trace/default-tracer}
 (defn add [a b]
   (+ a b)))

(add 1 2)
```

Check your JavaScript console and look for a grouped message titled
`foo/add [a b]`. Open it and you should see

```
▾ bindings
|  ▾ x 1
|  | 1
|  ▾ y 2
|  | 2
▸  => 3
```

For convenience you may omit the options map to `trace-forms`.

## Design

Clairvoyant is based on two concepts: tracers and source code
transformation. Although this is not a dramatic departure from similar
tools, Clairvoyant maintains an emphasis on extensibility which affords
the programmer finer control over the tracing process. This
extensibility is provided by Clojure's protocols and multimethods.


### Tracers

Tracers are defined around a small set of protocols which
correspond to key points in a trace's lifecycle: `ITraceEnter`,
`ITraceError`, and `ITraceExit`. Respectively, these align with when
a trace begins, when an error is encountered, and when a trace has
ended. The locations of these points may vary from form to form but in
general they're positioned to strategically.

At each point in the trace the tracer will receive data in the form of
a map containing at the least the following information.

 Key     | Description                      | Example
---------|----------------------------------|--------------
 `:op`   | The form operator                | `fn`
 `:form` | The form being traced            | `(fn [x] x)`
 `:ns`   | The form's originating namespace | `foo`

More information about trace data can be found in the
[Trace Data](#trace-data) section.

To picture how this looks consider this simplified example of the
tracing life cycle for the `defn` form.


```clj
(trace/trace-forms
  {:tracer default-tracer}
  (defn example [x]
    ;; TRACE ENTER
    ;; TRACE ERROR (START)
    {:pre [(even? x)]}
    (+ x x)
    ;; TRACE ERROR (END)
    ;; TRACE EXIT
    ))

(example 2)
(example 1)
```

In the first call to `example`, the tracer's `ITraceEnter` function is
called with the following trace data:

```clj
{:name 'example
 :ns 'foo
 :form '(defn example [x] {:pre [(even? x)]} (+ x x))
 :arglist '[x]
 :anonymous? false}
```

Notice this occurs _before_ the precondition is evaluated. This is
useful because it means the function's inputs are visible even when the
condition fails which can be useful for debugging. Under the hood the
underlying `fn` form is transformed to make this possible.

Before the function returns, the tracer's `ITraceExit` function is
called with the same trace data as before, however, it contains one
additional bit of information; the exit value.

```clj
{...
 :exit 4
 ...}
```

In the second call to `example` the tracer's `ITraceEnter` function
would still be called containing similar trace data from the first
example. But when the precondition is evaluated, it will raise an
`AssertionError`. This is when the tracer's `ITraceError` will be
called. As with the exit trace data this will contain the same data
information from the trace entry point but will contain a key for the
error.

```clj
{...
 :error AssertionError
 ...}
```

#### Custom tracers

To create a custom tracer implement at least one of the protocols for
the trace life cycle. The source for `clairvoyant.core/default-tracer`
can be used as a reference point. Remember, the behavior of a tracer
is not necessarily tied to logging to the console or printing to the
screen. Since the trace values are just data the barrier to creativity
is low.

An additional tracer that works well with [cljs-devtools]
(https://github.com/binaryage/cljs-devtools) is included as 
`clairvoyant.core/cljs-devtools-tracer`.


### Source code transformation

`clairvoyant.core/trace-forms` is the sole macro used for source code
trasnformation. It walks each form given to it delegating to the public
`trace-form` multimethod that is responsible for returning the transformed
form containing the tracer hook points. To ensure a high level of control
Clairvoyant avoids macroexpansion. This is important because it allows the
programmer to trace any form they wish rather than just the special forms.

#### Conditional tracing

If you want to remove tracing on production builds, 
`clairvoyant.core/trace-forms` will not add tracing when `js/goog.DEBUG`
is set to false this requires `:closure-defines {:goog.DEBUG false}`
option (under `:compiler` options in your `project.clj` file).
Therefore if you set this option in your prod builds you do not need to
remove `clairvoyant.core/trace-forms` from your source. 

WARNING: to set this option you do need to have `:optimizations :simple`
or `optimizations :advanced` or else the value will not be set because
the closure compiler does not do the substitution.

## Trace data

### `defn`, `fn`, `fn*`

 Key           | Description                              | Example
---------------|------------------------------------------|---------
 `:name`       | The function's name (if provided)        | `add`
 `:arglist`    | The function's signature                 | `[a b]`
 `:anonymous?` | Whether or not the function is anonymous | `false`

### `defmethod`

 Key             | Description                        | Example
-----------------|------------------------------------|-------------------
 `:name`         | The multimethod's name             | `+`
 `:arglist`      | The methods's signature            | `[a b]`
 `:dispatch-val` | The dispatch value                 | `[Number Number]`


### `reify`, `extend-type`, `extend-protocol`

 Key         | Description                          | Example
-------------|--------------------------------------|------------------------
 `:protocol` | The protocol's name (fully resolved) | `clojure.core/ILookup`
 `:name`     | The functions's name                 | `-lookup`
 `:arglist`  | The function's signature             | `[a b]`


## What about Clojure?

This library was born out of frustration with `println` debugging in
ClojureScript. For the moment it will remain a ClojureScript project but
that shouldn't be for long.

## License

Copyright © 2014 Christopher Joel Holdbrooks

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
