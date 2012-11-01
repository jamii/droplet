(ns droplet.lattice
  (:require droplet.fixpoint
            clojure.set))

(defprotocol JoinLattice
  (join* [this elem-a elem-b] "The least upper bound of two lattice elements. Must be idempotent, associative, commutative."))

; we treat nil as a lattice identity - kinda ugly but lots of morphisms otherwise require dispatching on return types
(defn join [type elem-a elem-b]
  (cond
   (nil? elem-a) elem-b
   (nil? elem-b) elem-a
   :else (join* type elem-a elem-b)))

(defmacro deflattice [name args & op]
  `(defrecord ~name ~args
     ~'droplet.lattice.JoinLattice
     (~'join* [~'this ~'elem-a ~'elem-b]
       (~@op ~'elem-a ~'elem-b))))

(deflattice Bool [] or)
(deflattice Max [] max)
(deflattice Min [] min)
(deflattice Set [] clojure.set/union)
(deflattice Map [value-lattice] merge-with (partial join value-lattice))

(defmacro query [args & body]
  `(fn [{:keys [~@args]}] ~@body))

(defmacro defquery [name args & body]
  `(def ~name (query ~args ~@body)))

(defrecord Join [type sink query-fn]
  droplet.fixpoint.Rule
  (run [this env]
    (assoc env sink (join type (get env sink) (query-fn env)))))
