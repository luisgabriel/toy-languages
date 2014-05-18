(ns toy-languages.expressions2.parser
  (:require [instaparse.core :as insta]))

(def grammar
  "program = exp

   <exp> = cexp
         | unary-exp
         | binary-exp
         | let-exp

   <cexp> = value / const-id / <'('> exp <')'>

   value = int | bool | string

   unary-exp = minus cexp
             | not cexp
             | length cexp

   binary-exp = exp plus cexp
              | exp minus cexp
              | exp and cexp
              | exp or cexp
              | exp eq cexp
              | exp concat cexp

   let-exp = <'let'> const-dec <'in'> exp

   <const-dec> = <'var'> const-id <'='> exp
               | const-dec <','> const-dec

   int = int-literal
   bool = bool-literal
   string = string-literal
   not = <'not'>
   length = <'length'>
   minus = <'-'>
   plus = <'+'>
   and = <'and'>
   or = <'or'>
   eq = <'=='>
   concat = <'++'>
   const-id = #'[A-Za-z][A-Za-z0-9]*'

   int-literal = #'\\d+'
   bool-literal = 'true' | 'false'
   <string-literal> = <'\"'> #'[A-Za-z0-9]*' <'\"'>")

(def parser (insta/parser grammar :auto-whitespace :standard))

(defn- transform-let-exp [& args]
  (let [exp (last args)
        decs (drop-last args)
        grouped-decs (partition 2 decs)]
    [:let-exp (vec (map vec grouped-decs)) exp]))

(def transform-options
  {:int-literal read-string
   :bool-literal read-string
   :not #(identity :not)
   :length #(identity :length)
   :binary-exp (fn [e1 op e2] [:binary-exp op e1 e2])
   :plus #(identity :add)
   :minus #(identity :minus)
   :and #(identity :and)
   :or #(identity :or)
   :eq #(identity :equals)
   :concat #(identity :concat)
   :let-exp transform-let-exp})

(defn parse [input]
  (let [result (->> (parser input) (insta/transform transform-options))]
    [(not (insta/failure? result)) result]))

(defn parses [input]
  (insta/parses parser input))
