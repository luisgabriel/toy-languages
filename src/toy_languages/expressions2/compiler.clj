(ns toy-languages.expressions2.compiler
  (:refer-clojure :exclude [compile]))

(declare compile-exp)

(defn- compile-value [node]
  (let [tuple (first node)
        kind (first tuple)
        value (nth tuple 1)]
    (if (= kind :string)
      (str "\"" value "\"")
      value)))

(defn- compile-id [subtree]
  (symbol (first subtree)))

(defn- to-vector-dec [decs]
  (loop [ds decs v []]
    (if (empty? ds)
      v
      (let [d (first ds)
            const-id (first d)
            exp (nth d 1)
            id (symbol (nth const-id 1))
            exp-code (compile-exp exp)
            new-vec (conj (conj v id) exp-code)]
        (recur (subvec ds 1) new-vec)))))

(defn- compile-let [content]
  (let [decs (first content)
        exp (nth content 1)
        let-decs (to-vector-dec decs)
        exp-code (compile-exp exp)]
    `(let ~let-decs ~exp-code)))

(defn- compile-unary [exp]
  (let [op (first exp)
        subexp (nth exp 1)
        exp-code (compile-exp subexp)]
    (case op
      :minus `(- 0 ~exp-code)
      :not `(not ~exp-code)
      :length `(.length ~exp-code))))

(defn- compile-binary [exp]
  (let [op (first exp)
        lexp (nth exp 1)
        rexp (nth exp 2)
        lcode (compile-exp lexp)
        rcode (compile-exp rexp)]
    (case op
      :add `(+ ~lcode ~rcode)
      :minus `(- ~lcode ~rcode)
      :and `(and ~lcode ~rcode)
      :or `(or ~lcode ~rcode)
      :equals `(= ~lcode ~rcode)
      :concat `(str ~lcode ~rcode))))

(defn- compile-exp [exp]
  (let [kind (first exp)
        subtree (subvec exp 1)]
    (case kind
      :value (compile-value subtree)
      :const-id (compile-id subtree)
      :let-exp (compile-let subtree)
      :unary-exp (compile-unary subtree)
      :binary-exp (compile-binary subtree))))

(defn compile [ast]
  (let [node (first ast)
        exp (nth ast 1)]
    (case node
      :program (compile-exp exp))))
