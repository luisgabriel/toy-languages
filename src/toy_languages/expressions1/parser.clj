(ns toy-languages.expressions1.parser
  (:require [instaparse.core :as insta]))

(def parser
  (insta/parser
    "program = exp

     <exp> = cexp | unary-exp | binary-exp

     <cexp> = value | <'('> exp <')'>

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

     int-literal = #'\\d+'
     bool-literal = 'true' | 'false'
     <string-literal> = <'\"'> #'[A-Za-z0-9]+' <'\"'>"
    :auto-whitespace :standard))

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
   :concat #(identity :concat)})

(defn parse [input]
  (->> (parser input) (insta/transform transform-options)))

(defn parses [input]
  (insta/parses parser input))
