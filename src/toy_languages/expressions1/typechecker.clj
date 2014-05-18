(ns toy-languages.expressions1.typechecker)

(declare check-exp)

(defn- type-value [value]
  (first (first value)))

(defn- type-unary [exp]
  (let [op (first exp)]
    (case op
      :minus :int
      :not :bool
      :length :int)))

(defn- type-binary [exp]
  (let [op (first exp)]
    (case op
      :add :int
      :minus :int
      :and :bool
      :or :bool
      :equals :bool
      :concat :string)))

(defn- type-exp [exp]
  (let [kind (first exp)
        subtree (subvec exp 1)]
    (case kind
      :value (type-value subtree)
      :unary-exp (type-unary subtree)
      :binary-exp (type-binary subtree))))

(defn- check-unary [ast]
  (let [node (first ast)
        exp (nth ast 1)]
    (and
      (check-exp exp)
      (case node
        :minus (= (type-exp exp) :int)
        :not (= (type-exp exp) :bool)
        :length (= (type-exp exp) :string)))))

(defn- check-binary [ast]
  (let [op (first ast)
        lexp (nth ast 1)
        rexp (nth ast 2)
        is-type #(= (type-exp %1) %2)]
    (and
      (check-exp lexp)
      (check-exp rexp)
      (case op
        :add (and (is-type lexp :int) (is-type rexp :int))
        :minus (and (is-type lexp :int) (is-type rexp :int))
        :and (and (is-type lexp :bool) (is-type rexp :bool))
        :or (and (is-type lexp :bool) (is-type rexp :bool))
        :equals (= (type-exp lexp) (type-exp rexp))
        :concat (and (is-type lexp :string) (is-type rexp :string))))))

(defn- check-exp [exp]
  (let [kind (first exp)
        subtree (subvec exp 1)]
    (case kind
      :value true
      :unary-exp (check-unary subtree)
      :binary-exp (check-binary subtree))))

(defn check [ast]
  (let [node (first ast)
        tree (nth ast 1)]
    (case node
      :program (check-exp tree)
      false)))
