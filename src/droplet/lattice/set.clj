(ns droplet.lattice.set
  (require clojure.set)
  (use droplet
       droplet.lattice))

;; Implements an observed-remove set (OR-set).
;; Supports adding and removing items to the set.

;; In the case of a concurrent add/remove operation with no
;;  causal ordering, add has precedence.

(def clock (atom 0))
(defn clock-value
  []
  (swap! clock inc))

(defn assoc-set
  "Assoc'es an item to this multimap"
  [mmap k v]
  (merge-with clojure.set/union mmap {k v}))

(defn dissoc-tombs
  "Remove tombstone values for the given items, and if there are
  no more identifiers for an item, remove that from the set"
  [m tombs]
  (let [without (merge-with clojure.set/difference m tombs)]
    (select-keys without (for [[k v] without :when (seq v)] k))))

;; ORSet with tombstones
(deftype ORSet [m t]
  BoundedSemiLattice
  (bottom [self]
    (ORSet. {} {}))
  (lte? [self that]
    (and (clojure.set/subset? (merge-with clojure.set/union m t) (merge-with clojure.set/union (.m that) (.t that)))
         (clojure.set/subset? t (.t that))))
  (join [self that]
    (ORSet. (merge-with clojure.set/union (dissoc-tombs m (.t that)) (dissoc-tombs (.m that) t))
           (merge-with clojure.set/union t (.t that))))

  clojure.lang.IPersistentSet
  (contains [self elem] (contains? m))
  (disjoin [self elem]
    (let [tombs (get m elem)]
      (ORSet. (dissoc m elem) (assoc-set t elem tombs))))
  (get [self item] (get m item))


  clojure.lang.IPersistentCollection
  (seq [self] (seq (keys m)))
  (cons [self elem]  
    (ORSet. (assoc-set m elem #{(clock-value)}) t))
  (empty [self] (empty m))
  (equiv [self other] (= m (.m other)))
  (count [self] (count m)))

(defn or-set-tombs
  "Returns a ORSET CRDT that implements the standard
  set interfaces"
  []
  (ORSet. {} {}))
