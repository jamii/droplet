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

(defn site-id
  []
  "MACADDR") ;; TODO

(defn assoc-set
  "Assoc'es an item to this multimap"
  [mmap k v]
  (merge-with clojure.set/union mmap {k v}))

(defn clean-empty
  "Removes entries from a map that has
  values that are empty sets"
  [m]
  (select-keys m (for [[k v] m :when (seq v)] k)))

(defn dissoc-tombs
  "Remove tombstone values for the given items, and if there are
  no more identifiers for an item, remove that from the set"
  [m tombs]
  (let [without (merge-with clojure.set/difference m tombs)]
    (clean-empty without)))

(defn version-lte?
  "Determines, given two version vector of unique item ids,
  if the latter is entirely equal or greater than the former"
  [vv-one vv-two]
  (let [diff (seq (clojure.set/difference vv-two vv-one))
        biggest (apply max vv-one)]
    (if (seq diff)
      (every? #(> % biggest) diff)
      true)))

(defn merge-with-operation
  [m vv operation]
    (clean-empty (into {} (map (fn [[k v]]
                    [k (operation v vv)])
                  m))))

(defn remove-versions
  "Removes entries in the desired map of {val #{versions}}
  given a list of versions"
  ([m vv]
    (merge-with-operation m vv clojure.set/difference))
  ([m vv operation]
    (merge-with-operation m vv operation)))

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

;; ORSet without tombstones
;; Uses version vectors
(deftype ORSetVector [m vv]
  BoundedSemiLattice
  (bottom [self]
    (ORSetVector. {} #{}))
  (lte? [self that]
    (let [removed #(clojure.set/difference %2 (apply clojure.set/union (keys %1)))
          self-removed (removed m vv)
          that-removed (removed (.m that) (.vv that))]
      (and (version-lte? vv (.vv that))
           (clojure.set/subset? self-removed that-removed))))
  (join [self that]
    (let [that-added      (remove-versions (.m that) vv)
          only-in-this    (clean-empty (merge-with clojure.set/difference m (.m that)))
          removed-in-that (remove-versions only-in-this (.vv that) clojure.set/intersection)]
      (ORSetVector. (merge-with clojure.set/union that-added (merge-with clojure.set/difference m removed-in-that))
                    (clojure.set/union vv (.vv that)))))

  clojure.lang.IPersistentSet
  (contains [self elem] (contains? m))
  (disjoin [self elem]
    (ORSetVector. (dissoc m elem) vv))
  (get [self item] (get m item))


  clojure.lang.IPersistentCollection
  (seq [self] (seq (keys m)))
  (cons [self elem]
    (let [t (clock-value)]
      (ORSetVector. (assoc-set m elem #{t})
                    (conj vv t))))
  (empty [self] (empty m))
  (equiv [self other] (= m (.m other)))
  (count [self] (count m)))

(defn or-set-tombless
  "Returns an ORSet implemented without tombstones"
  []
  (ORSetVector. {} #{}))