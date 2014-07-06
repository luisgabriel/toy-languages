(ns toy-languages.core
  (:require [toy-languages.expressions2.repl :as repl])
  (:require [toy-languages.expressions2.core :as core])
  (:gen-class))

(defn -main [& args]
  (let [kind (first args)]
    (if (= kind "repl")
      (repl/run)
      (let [source-code (nth args 1)]
        (try
          (->> source-code
               (core/compile)
               (println))
          (catch Throwable t
            (println (.getMessage t))))))))
