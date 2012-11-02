(ns droplet.lattice.set
  (:require droplet.instant
            strucjure
            strucjure.parser
            strucjure.pattern
            strucjure.util))

(defprotocol Clause
  (run [this states bindings-seq] "Apply this clause, return [new-states new-bindings-seq]"))

(defrecord Project [sym pattern]
  strucjure.pattern.AST
  (with-scope [this scope]
    (strucjure.pattern/pass-scope
     (fn [pattern] `(->Project '~sym ~pattern)) pattern scope))
  Clause
  (run [this states bindings-seq]
    (let [new-bindings-seq (atom ())]
      (doseq [bindings bindings-seq
              elem (:inner (get states sym))]
        (if-let [[rest bindings] (strucjure.pattern/run pattern elem bindings {})]
          (when (nil? rest)
            (swap! new-bindings-seq conj bindings))))
      [states @new-bindings-seq])))

(defrecord Conj [sym fun]
  strucjure.pattern.AST
  (with-scope [this scope]
    [`(->Conj '~sym ~(strucjure.util/src-with-scope fun scope)) scope])
  Clause
  (run [this states bindings-seq]
    (let [new-state (atom #{})]
      (doseq [bindings bindings-seq]
        (swap! new-state conj (fun nil bindings)))
      [(update-in states [sym] droplet.lattice/join @new-state) bindings-seq])))

(defrecord Rule [clauses]
  droplet.instant.Rule
  (run [this states]
    (let [[new-states _] (reduce
                          (fn [[states bindings-seq] clause]
                            (run clause states bindings-seq))
                          [states (list {})]
                          clauses)]
      new-states)))

(strucjure/defview parse-clause
  (prefix (and symbol? ?sym) ?pattern)
  (->Project sym (strucjure/run strucjure.parser/parse-pattern-ast pattern))

  (prefix ['conj (and symbol? ?sym) ?fun])
  (->Conj sym fun))

(strucjure/defview parse-rule
  [& ((strucjure/zero-or-more-prefix parse-clause) ?clauses)]
  (let [[rule-code _] (strucjure.pattern/chain-scope (fn [clauses] `(->Rule ~clauses)) clauses #{})]
    rule-code))

(defmacro rule [& forms]
  (strucjure/run parse-rule forms))

(defmacro query [sym & forms]
  `(fn [states#]
     (let [query-states# (assoc states# '~sym (droplet.lattice/->Set #{}))
           result-states# (droplet.instant/run ~(strucjure/run parse-rule forms) query-states#)]
       (:inner (get result-states# '~sym)))))
