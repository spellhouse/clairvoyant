(require '[clojure.java.shell])
(require '[clojure.string])

(def VERSION
  (do (-> (clojure.java.shell/sh "git" "describe" "--match" "v0.0")
          (:out)
          (.trim)
          (subs 1))))

(defproject spellhouse/clairvoyant VERSION
  :description "ClojureScript tracing library"
  :url "http://github.com/spellhouse/clairvoyant"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies
  [[org.clojure/clojure "1.6.0" :scope "provided"]
   [org.clojure/clojurescript "0.0-2322" :scope "provided"]]

  :clean-targets 
  ^{:protect false} ["dev-resources/public/out"
                     "resources/public/out"
                     "target"]

  :cljsbuild 
  {:builds 
   {:app {:source-paths ["src"]
          :compiler     
          {:output-to "dev-resources/public/clairvoyant.js"
           :output-dir "dev-resources/public/out/"
           :optimizations :none
           :pretty-print true}}}}

  :aliases
  {"auto-build" ~(clojure.string/split
                   "do cljsbuild clean, cljsbuild auto"
                   #"\s+")}

  :release-tasks 
  [["clean"]
   ["with-profiles" 
    "-dev,+release" 
    "cljsbuild" "once"]
   ["jar"]]

  :profiles
  {:dev
   {:dependencies
    [[com.cemerick/piggieback "0.1.3"]
     [weasel "0.4.0-SNAPSHOT"]]

    :plugins
    [[lein-cljsbuild "1.0.3"]]

    :source-paths
    ["dev"]

    :repl-options 
    {:nrepl-middleware 
     [cemerick.piggieback/wrap-cljs-repl]}

    :cljsbuild
    {:builds {:app {:source-paths ["dev"]}}}}

   :release 
   {:cljsbuild 
    {:jar true
     :builds 
     {:app {:compiler 
            {:output-to "resources/public/clairvoyant.js"
             :output-dir    "resources/public/out/"
             :optimizations :advanced
             :pretty-print  false}}}}}})
