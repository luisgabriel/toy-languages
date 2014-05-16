(ns toy-languages.expressions1.parser
  (:require [instaparse.core :as insta]))

(def parser
  (insta/parser
    "program = fexp
     <fexp> = exp | <lparen> exp <rparen>

     <exp> = value | unary-exp | binary-exp

     value = int | bool | string

     unary-exp = minus fexp
               | not fexp
               | length fexp

     binary-exp = fexp plus fexp
                | fexp minus fexp
                | fexp and fexp
                | fexp or fexp
                | fexp eq fexp
                | fexp concat fexp

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
     <string-literal> = <'\"'> #'[A-Za-z0-9]+' <'\"'>
     <lparen> = <'('>
     <rparen> = <')'>
    "
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
