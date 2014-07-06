(ns toy-languages.expressions2.core
  (:require [toy-languages.expressions2.parser :as parser])
  (:require [toy-languages.expressions2.typechecker :as typechecker])
  (:require [toy-languages.expressions2.interpreter :as interpreter])
  (:require [toy-languages.expressions2.compiler :as compiler])
  (:refer-clojure :exclude [compile eval]))

(defn- exec-core [kind input]
  (let [result (parser/parse input)
        parsed (first result)
        ast-or-failure (nth result 1)]
    (if parsed
      (if (typechecker/check ast-or-failure)
        (if (= kind :eval)
            (interpreter/evaluate ast-or-failure)
            (compiler/compile ast-or-failure))
        (throw (Throwable. "Type error.")))
      ast-or-failure)))

(defn compile [input]
  (exec-core :compile input))

(defn eval [input]
  (exec-core :eval input))
