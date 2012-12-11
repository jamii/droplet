(ns droplet.runtime
  (require [droplet.lattice :as lattice]))

;; --- STATES ---

;; states is {name lattice-element}

;; --- RULES ---

(defprotocol Rule
  (run-rule [this old-states new-states] "Run the rule on old-states and return modified new-states"))

(defrecord Raw [raw-fn]
  Rule
  (run-rule [this old-states new-states]
    (raw-fn old-states new-states)))

(def raw ->Raw)

(defn productions [old-states new-states rule]
  (let [sources (select-keys old-states (:sources rule))
        sink ((:fun rule) sources)]
    (update-in new-states [(:sink rule)] lattice/join sink)))

(defn deduct [rule]
  (vary-meta rule assoc ::action :deduct))

(defn induct [rule]
  (vary-meta rule assoc ::action :induct))

(defn deductive? [rule]
  (= :deduct (::action (meta rule))))

(defn inductive? [rule]
  (= :induct (::action (meta rule))))

;; --- DEDUCTIVE ---

(defn deductions [states rule]
  (assert (deductive? rule))
  (run-rule rule states states))

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

(defn inductions [old-states new-states rule]
  (assert (inductive? rule))
  (run-rule rule old-states new-states))

(defn inductive-step [states deductive-rules-strata inductive-rules]
  (let [fixed-states (apply fixpoint states deductive-rules-strata)]
    (reduce (partial inductions fixed-states) fixed-states inductive-rules)))

(defn quiscience [states deductive-rules-strata inductive-rules]
  (let [new-states (inductive-step states deductive-rules-strata inductive-rules)]
    (if (= states new-states)
      new-states
      (recur new-states deductive-rules-strata inductive-rules))))
