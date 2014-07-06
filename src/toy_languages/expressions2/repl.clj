(ns toy-languages.expressions2.repl
  (:require [toy-languages.expressions2.core :as core])
  (:gen-class))

(defn -main [& args]
  (println "+------------------------------+")
  (println "|  Expression Language 2 REPL  |")
  (println "+------------------------------+")
  (loop []
    (print ">> ")
    (flush)
    (let [input (read-line)]
      (when (= input "exit")
        (println "Bye!")
        (System/exit 0))
      (try
        (->> input
             (core/exec)
             (println))
        (catch java.lang.Throwable t
          (println (str "Error: " (.getMessage t)))))
      (recur))))
