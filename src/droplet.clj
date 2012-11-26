(ns droplet
  (require clojure.set))

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
  (let [sources (select-keys states (:sources deductive-rule))
        sink ((:fun deductive-rule) sources)]
    (update-in states [(:sink deductive-rule)] join sink)))

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
  (let [sources (select-keys states (:sources inductive-rule))
        sink ((:fun inductive-rule) sources)]
    (assoc new-states (:sink inductive-rule) sink)))

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
  (induct name [name] (fn [states] (get states name))))

(defn ephemeral [name]
  (induct name [name] (fn [states] (bottom (get states name)))))

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
