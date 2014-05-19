(ns toy-languages.expressions2.typechecker)

(declare check-exp)

(defn- type-value [value]
  (first (first value)))

(defn- type-id [table id]
  (let [id-key (keyword id)]
    (if (contains? table id-key)
      (table (keyword id))
      (throw (Throwable. (str "Unknown constant '" id "'."))))))

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

(defn- type-exp [table exp]
  (let [kind (first exp)
        subtree (subvec exp 1)]
    (case kind
      :value (type-value subtree)
      :const-id (type-id table (first subtree))
      :unary-exp (type-unary subtree)
      :binary-exp (type-binary subtree))))

(defn- update-table [table decs]
  (loop [ds decs t table]
    (if (empty? ds)
      t
      (let [d (first ds)
            const-id (first d)
            exp (nth d 1)
            id (nth const-id 1)
            exp-type (type-exp t exp)
            id-key (keyword id)
            new-table (assoc t id-key exp-type)]
        (if (contains? t id-key)
          (throw (Throwable. (str "Constant '" id "' already declared!")))
          (recur (subvec ds 1) new-table))))))

(defn- check-id [table id]
  (if (contains? table (keyword id))
    true
    (Throwable. (str "Unknown constant '" id "'."))))

(defn- check-let [table content]
  (let [decs (first content)
        exp (nth content 1)
        new-table (update-table table decs)]
    (check-exp new-table exp)))

(defn- check-unary [table ast]
  (let [node (first ast)
        exp (nth ast 1)
        is-type #(= (type-exp table exp) %)]
    (and
      (check-exp table exp)
      (case node
        :minus (is-type :int)
        :not (is-type :bool)
        :length (is-type :string)))))

(defn- check-binary [table ast]
  (let [op (first ast)
        lexp (nth ast 1)
        rexp (nth ast 2)
        is-type #(= (type-exp table %1) %2)]
    (and
      (check-exp table lexp)
      (check-exp table rexp)
      (case op
        :add (and (is-type lexp :int) (is-type rexp :int))
        :minus (and (is-type lexp :int) (is-type rexp :int))
        :and (and (is-type lexp :bool) (is-type rexp :bool))
        :or (and (is-type lexp :bool) (is-type rexp :bool))
        :equals (= (type-exp table lexp) (type-exp table rexp))
        :concat (and (is-type lexp :string) (is-type rexp :string))))))

(defn- check-exp [table exp]
  (let [kind (first exp)
        subtree (subvec exp 1)]
    (case kind
      :value true
      :const-id (check-id table (first subtree))
      :let-exp (check-let table subtree)
      :unary-exp (check-unary table subtree)
      :binary-exp (check-binary table subtree))))

(defn check [ast]
  (let [node (first ast)
        tree (nth ast 1)]
    (case node
      :program (check-exp {} tree)
      false)))
