(defproject spellhouse/clairvoyant "0.1.0-SNAPSHOT"
  :description "ClojureScript tracing library"
  :url "http://github.com/spellhouse/clairvoyant"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies
  [[org.clojure/clojure "1.6.0" :scope "provided"]
   [org.clojure/clojurescript "0.0-2322" :scope "provided"]]

  :profiles
  {:dev
   {:dependencies
    [[com.cemerick/piggieback "0.1.3"]
     [weasel "0.4.0-SNAPSHOT"]]

    :plugins
    [[lein-cljsbuild "1.0.3"]]

    :source-paths
    ["src" "dev"]

    :repl-options
    {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}}}

  :cljsbuild
  {:builds [{:id "dev"
             :source-paths ["src" "dev"]
             :compiler {:output-to "resources/public/clairvoyant.js"
                        :output-dir "resources/public/out"
                        :optimizations :none
                        :source-map true}}]})
