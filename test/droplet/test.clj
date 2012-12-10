(ns droplet.test
  (use clojure.test
       droplet)
  (require [droplet.datalog :as d]
           [droplet.lattice :as lattice :refer [->Max ->Min]]))

;; --- CORE --

;; deductive tests

(def path-states
  {:edge #{[1 2] [2 3] [3 3] [4 5] [5 4]}
   :path #{}})

(def path-rules
  [(d/deduct :path [a b]
             :edge [?a ?b])
   (d/deduct :path [a c]
             :edge [?a ?b]
             :path [?b ?c])])

(deftest path-test
  (is (= #{[1 2] [1 3] [2 3] [3 3] [4 4] [5 4] [4 5] [5 5]}
         (get (fixpoint path-states path-rules) :path))))

;; inductive tests

(def growth-states path-states)

(def deductive-growth-rules
  [(d/deduct :path [a b]
             :edge [?a ?b])
   (d/deduct :path [a c]
             :edge [?a ?b]
             :edge [?b ?c])])

(def inductive-growth-rules
  [(ephemeral :path)
   (d/induct :edge [a b]
             :path [?a ?b])])

(deftest growth-test
  (is (= #{[1 2] [1 3] [2 3] [3 3] [4 4] [5 4] [4 5] [5 5]}
         (get (quiscience growth-states [deductive-growth-rules] inductive-growth-rules) :edge))))

;; nosql tests

;; vector-clock is a {node-id Max} lattice
(defn vc [& elems]
  (into {}
        (for [[node-id clock] (partition 2 elems)]
          [node-id (->Max clock)])))

;; like lpair in Bloom^L
;; not truly a lattice - join is not unique
(defrecord Causal [vc val]
  lattice/BoundedSemiLattice
  (bottom [this]
    (->Causal {} nil))
  (lte? [this that]
    (lattice/lte? (:vc this) (:vc that)))
  (join [this that]
    (cond
     (lattice/lt? (:vc this) (:vc that)) that
     (lattice/lt? (:vc that) (:vc this)) this
     :else (->Causal (lattice/join (:vc this) (:vc that)) (lattice/join (:val this) (:val that))))))

(def nosql-states
  {:puts #{}
   :store {}}) ; {key (->Causal vc val)}

(def nosql-rules
  [(ephemeral :puts)
   (persistent :store)
   (d/deduct :store [key (->Causal vc val)]
             :puts {:key ?key :vc ?vc :val ?val})])

(defn new-nosql []
  (reactive nosql-states nosql-rules))

(defn put! [nosql key vc val]
  (insert! nosql :puts {:key key :vc vc :val val}))

(defn is! [nosql key vc val]
  (if (await-for 1000 nosql)
    (is (= (->Causal vc val) (get-in @nosql [:states :store key])))
    (throw (agent-error nosql))))

(deftest nosql-serial-test
  (doto (new-nosql)
    (put! :x (vc :alice 1) (->Max 10))
    (is! :x (vc :alice 1) (->Max 10))
    (put! :y (vc :alice 2) (->Max 20))
    (is! :y (vc :alice 2) (->Max 20))
    (put! :x (vc :alice 3) (->Max 5))
    (is! :x (vc :alice 3) (->Max 5))))

(deftest nosql-merge-test
  (doto (new-nosql)
    (put! :x (vc :alice 1) (->Max 10))
    (put! :x (vc :alice 1 :bob 1) (->Max 20))
    (put! :x (vc :alice 1 :charlie 1) (->Max 15))
    ;; no causal relation - max value wins
    (is! :x (vc :alice 1 :bob 1 :charlie 1) (->Max 20))))

(deftest nosql-causal-test
  (doto (new-nosql)
    (put! :x (vc :alice 1) (->Max 10))
    (put! :x (vc :alice 1 :bob 1) (->Max 20))
    (put! :x (vc :alice 1 :bob 1 :charlie 1) (->Max 15))
    ;; causally ordered - last write wins
    (is! :x (vc :alice 1 :bob 1 :charlie 1) (->Max 15))))
