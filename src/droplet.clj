(ns droplet
  (require [droplet.runtime :as runtime]
           [droplet.lattice :as lattice]))

;; --- COMMON RULES ---

(defn ephemeral [name]
  (runtime/induct
   (runtime/raw
    (fn [old-states new-states]
      (update-in new-states [name] lattice/bottom)))))

;; --- TOP LEVEL ---

(defrecord Droplet [states deductive-rules-strata inductive-rules])

(defn reactive [states & rules]
  (let [rules (flatten rules)
        deductive-rules (filter runtime/deductive? rules)
        inductive-rules (filter runtime/inductive? rules)]
    (agent (->Droplet states [deductive-rules] inductive-rules))))

(defn insert [droplet name value]
  (-> droplet
      (update-in [:states name] conj value)
      (update-in [:states] runtime/quiscience (:deductive-rules-strata droplet) (:inductive-rules droplet))))

(defn insert! [reactive name value]
  (send reactive insert name value))
