(ns droplet.flow
  (require [droplet.lattice :as lattice]
           [droplet.runtime :as runtime]
           strucjure
           strucjure.util))

;; --- UTIL ---

(defprotocol Scoped
  (scope [this] "Return the set of symbols bound by this object"))

(defprotocol AST
  (with-scope [this scope] "Given a surrounding scope, return code which evals to the object in question"))

;; --- SOURCES ---

(defprotocol Source
  (run-source [this states] "Return a set of bindings"))

(defrecord In? [name pattern]
  Scoped
  (scope [this]
    (strucjure/scope pattern))
  Source
  (run-source [this states]
    (set
     (for [elem (get states name)
           :let [result (strucjure.pattern/run pattern elem {} {})]
           :when (not (nil? result))
           :let [[rest bindings] result]]
       bindings))))

(defmacro in? [name pattern]
  `(->In? ~name (strucjure/pattern ~pattern)))

(defrecord Gte? [name value]
  Scoped
  (scope [this]
    #{})
  Source
  (run-source [this states]
    (let [state (get states name)]
      (if (lattice/lte? value state)
        #{{}}
        #{}))))

(def gte? ->Gte?)

(defrecord Join? [sources]
  Scoped
  (scope [this]
    (apply clojure.set/union (map scope sources)))
  Source
  (run-source [this states]
    (reduce clojure.set/join (map #(run-source % states) sources))))

(defn join? [& sources]
  (->Join? sources))

;; --- SINKS ---

(defprotocol Sink
  (run-sink [this states bindings] "Given bindings, run-sink the rule and return modified states."))

(defn- join-in [state elem]
  (lattice/join state (conj (lattice/bottom state) elem)))

(defrecord In! [name value-fn]
  AST
  (with-scope [this scope]
    `(->In! ~name ~(strucjure.util/src-with-scope value-fn scope)))
  Sink
  (run-sink [this states bindings]
    (update-in states [name] join-in (value-fn nil bindings))))

(defmacro in! [name value-fn]
  `(->In! ~name '~value-fn))

(defrecord Gte! [name value-fn]
  AST
  (with-scope [this scope]
    `(->Gte! ~name ~(strucjure.util/src-with-scope value-fn scope)))
  Sink
  (run-sink [this states bindings]
    (update-in states [name] lattice/join (value-fn nil bindings))))

(defmacro gte! [name value-fn]
  `(->Gte! ~name '~value-fn))

(defrecord Do! [sinks]
  AST
  (with-scope [this scope]
    `(->Do! ~(vec (map #(with-scope % scope) sinks))))
  Sink
  (run-sink [this states bindings]
    (reduce #(run-sink %2 %1 bindings) states sinks)))

(defn do! [& sinks]
  (->Do! sinks))

;; --- FLOWS ---

(defrecord Flow [source sink]
  runtime/Rule
  (run-rule [this old-states new-states]
    (let [bindings-set (run-source source old-states)]
      (reduce #(run-sink sink %1 %2) new-states bindings-set))))

(strucjure/defview +source+ droplet.flow.Source %)
(strucjure/defview +sink+ droplet.flow.Sink %)
(strucjure/defview +flow+
  [& ((strucjure/zero-or-more +source+) ?sources) & ((strucjure/zero-or-more +sink+) ?sinks)]
  (let [source (apply join? sources)
        sink (with-scope (apply do! sinks) (scope source))]
    (eval `(->Flow ~source ~sink)))) ;; smell :(

(defn flow [& elems]
  (strucjure/run +flow+ elems))
