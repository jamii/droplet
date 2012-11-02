(ns droplet.lattice
  (:require droplet.instant
            clojure.set))

(defprotocol JoinLattice
  (join [this elem] "The least upper bound of two lattice elements. Must be idempotent, associative, commutative."))

(defmacro deflattice [name args & op]
  `(defrecord ~name ~(conj args 'inner)
     ~'droplet.lattice.JoinLattice
     (~'join [~'_ ~'elem]
       (new ~name ~@args (~@op ~'inner ~'elem)))))

(deflattice Bool [] or)
(deflattice Max [] max)
(deflattice Min [] min)
(deflattice Set [] clojure.set/union)
(deflattice Map [value-lattice] merge-with (partial join value-lattice))

(defmacro query [args & body]
  (let [states (gensym "states")]
    `(fn [~states]
       (let [~@(apply concat
                      (for [arg args]
                        [arg `(:inner (get ~states '~arg))]))]
         ~@body))))

(defmacro defquery [name args & body]
  `(def ~name (query ~args ~@body)))

(defrecord Join [sink query-fn]
  droplet.instant.Rule
  (run [this states]
    (assoc states sink (join (get states sink) (query-fn states)))))
