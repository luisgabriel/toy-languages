(ns toy-languages.expressions2.interpreter
  (:require [clojure.string :as string]))

(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

(declare eval-exp)

(defn- eval-value [value]
  (nth (first value) 1))

(defn- eval-id [table subtree]
  (let [id (first subtree)]
    (table (keyword id))))

(defn- update-table [table decs]
  (loop [ds decs t table]
    (if (empty? ds)
      t
      (let [d (first ds)
            const-id (first d)
            exp (nth d 1)
            id (nth const-id 1)
            exp-value (eval-exp t exp)
            id-key (keyword id)
            new-table (assoc t id-key exp-value)]
        (recur (subvec ds 1) new-table)))))

(defn- eval-let [table content]
  (let [decs (first content)
        exp (nth content 1)
        new-table (update-table table decs)]
    (eval-exp new-table exp)))

(defn- eval-unary [table exp]
  (let [op (first exp)
        subexp (nth exp 1)
        exp-value (eval-exp table subexp)]
    (case op
      :minus (- 0 exp-value)
      :not (not exp-value)
      :length (.length exp-value))))

(defn- eval-binary [table exp]
  (let [op (first exp)
        lexp (nth exp 1)
        rexp (nth exp 2)
        lvalue (eval-exp table lexp)
        rvalue (eval-exp table rexp)]
    (case op
      :add (+ lvalue rvalue)
      :minus (- lvalue rvalue)
      :and (and lvalue rvalue)
      :or (or lvalue rvalue)
      :equals (= lvalue rvalue)
      :concat (string/join "" (concat lvalue rvalue)))))

(defn- eval-exp [table exp]
  (let [kind (first exp)
        subtree (subvec exp 1)]
    (case kind
      :value (eval-value subtree)
      :const-id (eval-id table subtree)
      :let-exp (eval-let table subtree)
      :unary-exp (eval-unary table subtree)
      :binary-exp (eval-binary table subtree))))

(defn evaluate [ast]
  (let [node (first ast)
        exp (nth ast 1)]
    (case node
      :program (eval-exp {} exp))))
