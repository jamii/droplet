(ns droplet.lattice
  (require clojure.set))

(defprotocol BoundedSemiLattice
  (bottom [this] "Return the least element of the lattice")
  (lte? [this that] "Less-than, the lattice relation")
  (join [this that] "Find the least upper bound of two lattice elements"))

(defn lt? [this that]
  (and (not= this that)
       (lte? this that)))

(defrecord Min [val])
(defrecord Max [val])

(extend-protocol BoundedSemiLattice

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
