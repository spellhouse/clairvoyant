# Clairvoyant

Clairvoyant is a library that offers flexible tracing for ClojureScript.

## Usage

Add Clairvoyant to your project `:dependencies`.

```clj
[spellhouse/clairvoyant "0.1.0-SNAPSHOT"]
```

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
x = 1
y = 2
3
```

## License

Copyright © 2014 Christopher Joel Holdbrooks

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
