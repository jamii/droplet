(ns droplet.test
  (use clojure.test
       droplet)
  (require [droplet.datalog :as d]))

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

;; like lpair in Bloom^L
;; not truly a lattice - join is not unique
;; vc is {node-id Max}
(defrecord Causal [vc val]
  SemiLattice
  (bottom [this]
    (->Causal {} nil))
  (lte? [this that]
    (lte? (:vc this) (:vc that)))
  (join [this that]
    (cond
     (lt? (:vc this) (:vc that)) that
     (lt? (:vc that) (:vc this)) this
     :else (->Causal (join (:vc this) (:vc that)) (join (:val this) (:val that))))))

(def nosql-states
  {:puts #{}
   :store {}}) ; {key (->Causal vc val)}

(def nosql-rules
  [(ephemeral :puts)
   (persistent :store)
   (d/rule :deduct #(into {} %)
           :store [key (->Causal vc val)]
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
    (put! :x {:alice (->Max 1)} (->Max 10))
    (is! :x {:alice (->Max 1)} (->Max 10))
    (put! :y {:alice (->Max 2)} (->Max 20))
    (is! :y {:alice (->Max 2)} (->Max 20))
    (put! :x {:alice (->Max 3)} (->Max 5))
    (is! :x {:alice (->Max 3)} (->Max 5))))

(deftest nosql-merge-test
  (doto (new-nosql)
    (put! :x {:alice (->Max 1)} (->Max 10))
    (put! :x {:alice (->Max 1) :bob (->Max 1)} (->Max 20))
    (put! :x {:alice (->Max 1) :charlie (->Max 1)} (->Max 15))
    ;; no causal relation - max value wins
    (is! :x {:alice (->Max 1) :bob (->Max 1) :charlie (->Max 1)} (->Max 20))))

(deftest nosql-causal-test
  (doto (new-nosql)
    (put! :x {:alice (->Max 1)} (->Max 10))
    (put! :x {:alice (->Max 1) :bob (->Max 1)} (->Max 20))
    (put! :x {:alice (->Max 1) :bob (->Max 1) :charlie (->Max 1)} (->Max 15))
    ;; causally ordered - last write wins
    (is! :x {:alice (->Max 1) :bob (->Max 1) :charlie (->Max 1)} (->Max 15))))
