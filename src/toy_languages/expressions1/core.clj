(ns toy-languages.expressions1.core
  (:require [toy-languages.expressions1.parser :as parser])
  (:require [toy-languages.expressions1.typechecker :as typechecker])
  (:require [toy-languages.expressions1.interpreter :as interpreter]))

(defn exec [input]
  (let [result (parser/parse input)
        parsed (first result)
        ast-or-failure (nth result 1)]
    (if parsed
      (if (typechecker/check ast-or-failure)
        (interpreter/evaluate ast-or-failure)
        "Type error!")
      ast-or-failure)))
