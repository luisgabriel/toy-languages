(ns toy-languages.expressions1.interpreter)

(declare eval-exp)

(defn- eval-value [value]
  (nth (first value) 1))

(defn- eval-unary [exp]
  (let [op (first exp)
        subexp (nth exp 1)]
    (case op
      :minus (- 0 (eval-exp subexp))
      :not (not (eval-exp subexp))
      :length (.length (eval-exp subexp)))))

(defn- eval-binary [exp]
  (let [op (first exp)
        lexp (nth exp 1)
        rexp (nth exp 2)
        lvalue (eval-exp lexp)
        rvalue (eval-exp rexp)]
    (case op
      :add (+ lvalue rvalue)
      :minus (- lvalue rvalue)
      :and (and lvalue rvalue)
      :or (or lvalue rvalue)
      :equals (.equals lvalue rvalue)
      :concat (concat lvalue rvalue))))

(defn- eval-exp [exp]
  (let [kind (first exp)
        subtree (subvec exp 1)]
    (case kind
      :value (eval-value subtree)
      :unary-exp (eval-unary subtree)
      :binary-exp (eval-binary subtree))))

(defn evaluate [ast]
  (let [node (first ast)
        exp (nth ast 1)]
    (case node
      :program (eval-exp exp))))
