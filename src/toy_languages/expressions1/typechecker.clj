(ns toy-languages.expressions1.typechecker)

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
      :and :int
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

(defn- check-exp-type [exp expected-type]
  (let [exp-type (type-exp exp)]
    (= exp-type expected-type)))

(defn- check-unary [ast]
  (let [node (first ast)
        exp (nth ast 1)]
    (and
      (check-exp exp)
      (case node
        :minus (check-exp-type exp :int)
        :not (check-exp-type exp :bool)
        :length (check-exp-type exp :string)))))

(defn- check-binary [ast]
  (let [op (first ast)
        lexp (nth ast 1)
        rexp (nth ast 2)]
    (and
      (check-exp lexp)
      (check-exp rexp)
      (case op
        :add (and (check-exp-type lexp :int) (check-exp-type rexp :int))
        :minus (and (check-exp-type lexp :int) (check-exp-type rexp :int))
        :and (and (check-exp-type lexp :bool) (check-exp-type rexp :bool))
        :or (and (check-exp-type lexp :bool) (check-exp-type rexp :bool))
        :equals (and (check-exp-type lexp :string) (check-exp-type rexp :string))
        :concat (and (check-exp-type lexp :string) (check-exp-type rexp :string))))))

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
