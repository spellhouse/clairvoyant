(ns user
  (:require
   [weasel.repl :as ws-repl]
   [clairvoyant.core :as trace :include-macros true]))

(ws-repl/connect "ws://localhost:9091" :verbose true)
