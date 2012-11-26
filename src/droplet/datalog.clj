(ns droplet.datalog
  (:require droplet
            strucjure
            strucjure.parser
            strucjure.pattern
            strucjure.util))

;; TODO
;;   pick a better name :)
;;   negation + stratification
;;   aggregates

(defprotocol Clause
  (source [this] "Return the name of the source state")
  (run [this states bindings-seq] "Apply this clause, return new bindings-seq]"))

(defrecord Project [name pattern]
  strucjure.pattern.AST
  (with-scope [this scope]
    (strucjure.pattern/pass-scope
     (fn [pattern] `(->Project '~name ~pattern)) pattern scope))
  Clause
  (source [this]
    name)
  (run [this states bindings-seq]
    (for [bindings bindings-seq
          elem (get states name)
          :let [result (strucjure.pattern/run pattern elem bindings {})]
          :when (not (nil? result))
          :let [[rest new-bindings] result]
          :when (nil? rest)]
      new-bindings)))

(defrecord Query [agg-fn out-fn clauses]
  strucjure.pattern.AST
  (with-scope [this scope]
    (let [[new-clauses new-scope] (strucjure.pattern/chain-scope identity clauses scope)] ; so ugly :'(
      [`(->Query ~agg-fn ~(strucjure.util/src-with-scope out-fn new-scope) ~new-clauses) new-scope]))
  clojure.lang.IFn
  (invoke [this states]
    (agg-fn
     (for [bindings (reduce #(run %2 states %1) [{}] clauses)]
       (out-fn nil bindings)))))

(strucjure/defview parse-clause
  (prefix (and keyword? ?name) ?pattern)
  (->Project name (strucjure/run strucjure.parser/parse-pattern-ast pattern)))

(strucjure/defnview parse-query [agg-fn]
  [?out-fn & ((strucjure/zero-or-more-prefix parse-clause) ?clauses)]
  (->Query agg-fn out-fn clauses))

(defmacro query [agg-fn & forms]
  (let [query-ast (strucjure/run (parse-query agg-fn) forms)
        [query _] (strucjure.pattern/with-scope query-ast #{})]
    query))

(defmacro rule [action agg-fn sink & forms]
  (let [query-ast (strucjure/run (parse-query agg-fn) forms)
        sources (set (map source (:clauses query-ast)))
        [query _] (strucjure.pattern/with-scope query-ast #{})]
    `(droplet/->Rule ~action ~sink ~sources ~query)))

(defmacro deduct [sink & forms]
  `(rule :deduct set ~sink ~@forms))

(defmacro induct [sink & forms]
  `(rule :induct set ~sink ~@forms))
