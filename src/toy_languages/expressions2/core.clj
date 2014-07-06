(ns toy-languages.expressions2.core
  (:require [toy-languages.expressions2.parser :as parser])
  (:require [toy-languages.expressions2.typechecker :as typechecker])
  (:require [toy-languages.expressions2.interpreter :as interpreter]))

(defn exec [input]
  (let [result (parser/parse input)
        parsed (first result)
        ast-or-failure (nth result 1)]
    (if parsed
      (if (typechecker/check ast-or-failure)
        (interpreter/evaluate ast-or-failure)
        (throw (Throwable. "Type error.")))
      ast-or-failure)))
