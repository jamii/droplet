(ns droplet
  (use clojure.test)
  (require clojure.set
           strucjure
           strucjure.parser
           strucjure.pattern
           strucjure.util))

;; overview:
;;   states :: {name lattice-element}

;; correctness/lint:
;;   should have exactly one inductive sink per state

;; safety:

;; confluence/monotonicity:

;; TODO
;; put rules in states?

;; --- LATTICES ---

(defprotocol SemiLattice
  (bottom [this] "Return the least element of the lattice")
  (lte? [this that] "Less-than, the lattice relation")
  (join [this that] "Find the least upper bound of two lattice elements"))

(defn lt? [this that]
  (and (not= this that)
       (lte? this that)))

(defrecord Min [val])
(defrecord Max [val])

(extend-protocol SemiLattice

  java.lang.Boolean
  (bottom [this]
    false)
  (lte? [this that]
    (or (false? this)
        (true? that)))
  (join [this that]
    (or this that))

  Min
  (bottom [this]
    (->Min Double/POSITIVE_INFINITY))
  (lte? [this that]
    (>= (:val this) (:val that)))
  (join [this that]
    (->Min (min (:val this) (:val that))))

  Max
  (bottom [this]
    (->Max Double/NEGATIVE_INFINITY))
  (lte? [this that]
    (<= (:val this) (:val that)))
  (join [this that]
    (->Max (max (:val this) (:val that))))

  clojure.lang.APersistentSet
  (bottom [this]
    #{})
  (lte? [this that]
    (clojure.set/subset? this that))
  (join [this that]
    (clojure.set/union this that))

  clojure.lang.APersistentMap
  (bottom [this]
    {})
  (lte? [this that]
    (every? (fn [[key val]]
              (lte? val (get that key (bottom val))))
            this))
  (join [this that]
    (merge-with join this that)))

;; --- STATES ---

;; states is {name lattice-element}

;; --- RULES ---

(defrecord Rule [action sink sources fun]) ; action is either :deduct or :induct

;; --- DEDUCTIVE ---

(defn deduct [sink sources fun]
  (->Rule :deduct sink sources fun))

(defn deductions [states deductive-rule]
  (assert (= :deduct (:action deductive-rule)))
  (let [elements (map #(get states %) (:sources deductive-rule))
        new-element (apply (:fun deductive-rule) elements)]
    (update-in states [(:sink deductive-rule)] join new-element)))

(defn deductive-step [states deductive-rules]
  (reduce deductions states deductive-rules))

(defn fixpoint
  ([states deductive-rules]
     (let [new-states (deductive-step states deductive-rules)]
       (if (= states new-states)
         new-states
         (recur new-states deductive-rules))))
  ([states deductive-rules & deductive-rules-strata]
     (reduce fixpoint states (cons deductive-rules deductive-rules-strata))))

;; --- INDUCTIVE ---

(defn induct [sink sources fun]
  (->Rule :induct sink sources fun))

(defn inductions [states new-states inductive-rule]
  (assert (= :induct (:action inductive-rule)))
  (let [elements (map #(get states %) (:sources inductive-rule))
        new-element (apply (:fun inductive-rule) elements)]
    (assoc new-states (:sink inductive-rule) new-element)))

(defn inductive-step [states deductive-rules-strata inductive-rules]
  (let [fixed-states (apply fixpoint states deductive-rules-strata)]
    (reduce (partial inductions fixed-states) {} inductive-rules)))

(defn quiscience [states deductive-rules-strata inductive-rules]
  (let [new-states (inductive-step states deductive-rules-strata inductive-rules)]
    (if (= states new-states)
      new-states
      (recur new-states deductive-rules-strata inductive-rules))))

;; --- COMMON RULES ---

(defn persistent [name]
  (induct name [name] identity))

(defn ephemeral [name]
  (induct name [name] bottom))

;; --- TOP LEVEL ---

(defrecord Droplet [states deductive-rules-strata inductive-rules])

(defn reactive [states & rules]
  (let [rules (flatten rules)
        deductive-rules (filter #(= :deduct (:action %)) rules)
        inductive-rules (filter #(= :induct (:action %)) rules)]
    (agent (->Droplet states [deductive-rules] inductive-rules))))

(defn insert [droplet name value]
  (-> droplet
      (update-in [:states name] conj value)
      (update-in [:states] quiscience (:deductive-rules-strata droplet) (:inductive-rules droplet))))

(defn insert! [reactive name value]
  (send reactive insert name value))

;; --- TESTS ---

;; deductive tests

(def path-states
  {:edge #{[1 2] [2 3] [3 3] [4 5] [5 4]}
   :path #{}})

(def path-rules
  [(deduct :path [:edge] identity)
   (deduct :path [:path :edge]
           (fn [path edge]
             (set (for [[a b] path
                        [b' c] edge
                        :when (= b b')]
                    [a c]))))])

(deftest path-test
  (is (= #{[1 2] [1 3] [2 3] [3 3] [4 4] [5 4] [4 5] [5 5]}
         (get (fixpoint path-states path-rules) :path))))

;; inductive tests

(def growth-states path-states)

(def deductive-growth-rules
  [(deduct :path [:edge]
           (fn [edge]
             (set (for [[a b] edge
                        [b' c] edge
                        :when (= b b')]
                    [a c]))))])

(def inductive-growth-rules
  [(ephemeral :path)
   (induct :edge [:path :edge] join)])

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
   (deduct :store [:puts]
           (fn [puts]
             (into {}
                   (for [{:keys [key vc val]} puts]
                     [key (->Causal vc val)]))))])

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
