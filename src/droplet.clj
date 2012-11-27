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

;; Checks for equality and if not equal returns lte?
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
    (cond
     (seq? that) (into this that)
     (set? that) (clojure.set/union this that)))

  clojure.lang.APersistentMap
  (bottom [this]
    {})
  (lte? [this that]
    (every? (fn [[key val]]
              (lte? val (get that key (bottom val))))
            this))
  (join [this that]
    (cond
     (seq? that) (merge-with join this (into {} that))
     (map? that) (merge-with join this that))))

;; --- STATES ---

;; states is {name lattice-element}

;; --- RULES ---

(defrecord Rule [action sink sources fun]) ; action is either :deduct or :induct

(defn productions [old-states new-states rule]
  (let [sources (select-keys old-states (:sources rule))
        sink ((:fun rule) sources)]
    (update-in new-states [(:sink rule)] join sink)))

;; --- DEDUCTIVE ---

;; Generate a deductive rule w/ the given sink sources and function
(defn deduct [sink sources fun]
  (->Rule :deduct sink sources fun))

;; TODO update
;; Given a deductive rule and a map of states,
;;  Get every state for each source in the rule (elements)
;;  Apply the deductive rule's function to each state
;;  Update the states map's sink state by joining the new states that were generated
(defn deductions [states rule]
  (assert (= :deduct (:action rule)))
  (productions states states rule))

;; Calls deductions with the current states list, and deduces through all the given rules
(defn deductive-step [states deductive-rules]
  (reduce deductions states deductive-rules))

;; Calls deductive-step with the states map and rules, until deductive-step no longer generates new states
;; If called with strata, run fixpoint on state and each item in (cons deductive-rules deductive-rules-strata)
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

;; TODO update
;; Given an inductive rule, a set of states, and a set of newly generated states
;;  Get every state for each source in the rule, and apply the inductive rule function to each state
;;  Insert each newly generated state in the new-states map w/ the key being the inductive-rule's :sink
(defn inductions [old-states new-states rule]
  (assert (= :induct (:action rule)))
  (productions old-states new-states rule))

;; Given a map of states, a list of stratified deductive rules, and a list of inductive rules
;;  Generate the fixpoint from executing all the deductive rules
;;  Call inductions for each inductive rule, given the set of fixed states from the deductive step.
;;  Collect and return the result of the inductive rules
(defn inductive-step [states deductive-rules-strata inductive-rules]
  (let [fixed-states (apply fixpoint states deductive-rules-strata)
        new-states (into {} (for [[name state] states] [name (bottom state)]))]
    (reduce (partial inductions fixed-states) new-states inductive-rules)))

;; Given a list of states, deductive rules, and inductive rules
;;  Recursively call inductive-step until no more new states are generated
(defn quiscience [states deductive-rules-strata inductive-rules]
  (let [new-states (inductive-step states deductive-rules-strata inductive-rules)]
    (if (= states new-states)
      new-states
      (recur new-states deductive-rules-strata inductive-rules))))

;; --- COMMON RULES ---

;; Deductive rule that just returns the object itself
;;  Will make a lattice persist across timesteps
(defn persistent [name]
  (induct name [name] (fn [states] (get states name))))

;; Inductive rule that returns the bottom of a lattice
;; Marks a table as ephemeral---that is, at each inductive step
;;  it'll start with the bottom value for that lattice
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
