(ns droplet.lattice.set
  (require [clojure.set :refer [union subset? difference intersection]]
           [droplet.lattice :refer [join BoundedSemiLattice]]))

;; Implements an observed-remove set (OR-set).
;; Supports adding and removing items to the set.

;; In the case of a concurrent add/remove operation with no
;;  causal ordering, add has precedence.

(def clock (atom 0))
(defn clock-value
  []
  (swap! clock inc))

(defn site-id []
  "MACADDR") ;; TODO

(defn assoc-set
  "Assoc'es an item to this multimap"
  [mmap k v]
  (update-in mmap [k] union v))
  ; (merge-with union mmap {k v}))

(defn clean-empty
  "Removes entries from a map that has
  values that are empty sets"
  [mmap]
  (into {} (filter #(seq (val %)) mmap)))

(defn dissoc-tombs
  "Remove tombstone values for the given items, and if there are
  no more identifiers for an item, remove that from the set"
  [mmap tombs]
  (clean-empty (merge-with difference mmap tombs)))

(defn version-lte?
  "Determines, given two version vector of unique item ids,
  if the latter is entirely equal or greater than the former"
  [vv-one vv-two]
  (let [diff (seq (difference vv-two vv-one))
        biggest (apply max vv-one)]
    (if (seq diff)
      (every? #(> % biggest) diff)
      true)))

(defn merge-with-operation
  [mmap vv operation]
  (clean-empty (into {} (for [[k v] mmap] [k (operation v vv)]))))

(defn remove-versions
  "Removes entries in the desired map of {val #{versions}}
  given a list of versions"
  [mmap vv]
  (merge-with-operation mmap vv difference))

;; ORSet with tombstones
;; mmap is a map of {key #{uniqueid}}
;; tombstones is a list of unique ids that were removed from the set
(deftype ORSet [mmap tombstones]
  BoundedSemiLattice
  (bottom [this]
    (ORSet. {} {}))
  (lte? [this that]
    (and (subset? (merge-with union mmap tombstones) (merge-with union (.mmap that) (.tombstones that)))
         (subset? tombstones (.tombstones that))))
  (join [this that]
    (ORSet. (merge-with union (dissoc-tombs mmap (.tombstones that)) (dissoc-tombs (.mmap that) tombstones))
           (merge-with union tombstones (.tombstones that))))

  clojure.lang.IPersistentSet
  (contains [this elem] (contains? mmap elem))
  (disjoin [this elem]
    (let [tombs (get mmap elem)]
      (ORSet. (dissoc mmap elem) (assoc-set tombstones elem tombs))))
  (get [this item] (get mmap item))


  clojure.lang.IPersistentCollection
  (seq [this] (keys mmap))
  (cons [this elem]  
    (ORSet. (assoc-set mmap elem #{(clock-value)}) tombstones))
  (empty [this] (ORSet. {} {}))
  (equiv [this other] (= mmap (.mmap other)))
  (count [this] (count mmap)))

(defn or-set-tombs
  "Returns a ORSET CRDT that implements the standard
  set interfaces"
  []
  (ORSet. {} {}))

(defn- removed-from-orset
  "Returns a list of items that are in the second arg
  but not in the list of keys of the first"
  [vv mmap]
  difference mmap (apply union (keys vv)))

;; ORSet without tombstones
;; mmap is a map of {key #{uniqueid}}
;; vv is a version vector of {uniqueid ->Max}
(deftype ORSetVector [mmap vv]
  BoundedSemiLattice
  (bottom [this]
    (ORSetVector. {} #{}))
  (lte? [this that]
    (let [this-removed (removed-from-orset mmap vv)
          that-removed (removed-from-orset (.mmap that) (.vv that))]
      (and (version-lte? vv (.vv that))
           (subset? this-removed that-removed))))
  (join [this that]
    (let [that-added      (remove-versions (.mmap that) vv)
          only-in-this    (clean-empty (merge-with difference mmap (.mmap that)))
          removed-in-that (merge-with-operation only-in-this (.vv that) intersection)]
      (ORSetVector. (merge-with union that-added (merge-with difference mmap removed-in-that))
                    (union vv (.vv that)))))

  clojure.lang.IPersistentSet
  (contains [this elem] (contains? mmap))
  (disjoin [this elem]
    (ORSetVector. (dissoc mmap elem) vv))
  (get [this item] (get mmap item))


  clojure.lang.IPersistentCollection
  (seq [this] (seq (keys mmap)))
  (cons [this elem]
    (let [t (clock-value)]
      (ORSetVector. (assoc-set mmap elem #{t})
                    (conj vv t))))
  (empty [this] (empty mmap))
  (equiv [this other] (= mmap (.mmap other)))
  (count [this] (count mmap)))

(defn or-set-tombless
  "Returns an ORSet implemented without tombstones"
  []
  (ORSetVector. {} #{}))