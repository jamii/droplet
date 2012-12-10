(ns droplet.lattice.ordered-set
  (require clojure.set
           [droplet.test :as kvs])
  (use droplet
       droplet.lattice
       clojure.pprint))

;; Provides an ordered set lattice inspired by Treedoc:
;;  http://hal.inria.fr/inria-00445975
;;  http://run.unl.pt/bitstream/10362/7802/1/Sousa_2012.pdf

;; An ordered set has the two following operations as well as the lattice lte? and join methods:
;;  insert(prev-element, elem)
;;  delete(element)
;;
;; Each item in the ordered set contains a path, which is a list of {:path [{:branch :disamb}] :val val} and the payload value
;; A disambiguator for a non-tail node is omitted if the path passes through a major node. It is included if the path
;;  goes through a minor node
;; A disambiguator is a (clock, siteid) pair where clock is a lamport clock, the clock value that a node had when inserting that value
;;
;; Note that we diverge from the algorithm described in the Souza2012 paper when it comes to the Version Vector. The paper described
;;  the version vector as containing {disamb(node), counter} pairs, but if we only keep track of disambiguators, in the compare function
;;  we cannot get a list of node paths that do not exist in the set *but* have have an entry in the version vector---we would need to
;;  keep a list of "removed nodes" by disambiguator. Instead we make the VV key be the path+disambiguator, so we can retrieve the path
;;  directly.
;;

(def clock (atom 0))

(defn- clock-value
  "Next value for this node's vector clock id
  TODO"
  []
  (swap! clock inc))

(defn siteid<?
  "Returns a < relation for two site ids, that are usually MAC addresses.

  TODO real mac addresses.. for now just string comparison"
  [macaddra macaddrb]
  (< (.compareTo macaddra macaddrb) 0))

(defn- disamb<?
  [{clock-l :clock siteid-l :siteid} {clock-r :clock siteid-r :siteid}]
  (if (not= clock-l clock-r)
    (< clock-l clock-r) ;; If there is a lamport clock ordering, use that
    (siteid<? siteid-l siteid-r))) ;; else order by MAC address

(defn- single?
   "Returns true if the collection contains one item; else false."
   [coll]
   (= 1 (count coll)))

;; Compare by path and break ties (mini-nodes are siblings in same major node) with disambiguator
(defn item<?
  [{pathl :path} {pathr :path}]
  (loop [pathl pathl
         pathr pathr]
    (let [{l :branch l-disamb :disamb} (first pathl)
          {r :branch r-disamb :disamb} (first pathr)
          l-is-mininode (and l-disamb (> (count pathl) 1))] ;; mininode if this is not the last node and we have a disambiguator
      (cond
        (nil? l) (= r 1)
        (nil? r) (= l 0)
        (= l r) (if (or (and l-is-mininode r-disamb) ;; Comparing mininode to end node or other mininode, order by disambiguator
                        (and (single? pathl) (single? pathr))) ;; Finishes with two mini-siblings, order by disambiguator
                   (disamb<? l-disamb r-disamb)
                   (recur (rest pathl) (rest pathr))) ;; Normal case
        :else    (< l r)))))

(defn- path-len
  "Returns the path length for this path, which is usually the number of pairs
   except for the root which has a pair but no branch"
  [path]
  (if (and (= 1 (count path)) (nil? (:branch (first path))))
    0
    (count path)))

(defn ancestor?
  "Returns if the first path is an ancestor to the second"
  [pathl pathr]
  (and (< (path-len pathl) (path-len pathr)) ;; If path A is longer or equal, no way it can be an ancestor
    (loop [pathl pathl                           ;; Otherwise, determine if it's an ancestor
           pathr pathr]
      (let [{l :branch l-disamb :disamb} (first pathl)
            {r :branch r-disamb :disamb} (first pathr)]
        (or (nil? l) (and (= l r) (recur (subvec pathl 1) (subvec pathr 1))))))))

(defn- mini-sibling?
  "Returns true if the two paths are mini-siblings of each other,
   that is, they have the same path but differ in disambiguators"
   [pathl pathr]
   (and (= (count pathl) (count pathr))
        (= (map :branch pathl)
           (map :branch pathr))))

(defn- pathnode
  "Returns a new pathnode with the given branch
   and automatically-filled in disambiguator

   TODO use real lamport clock and macaddr for this!"
   [branch]
   {:branch branch :disamb {:clock (clock-value) :siteid "MACADDRESS"}})

(defn- extend-path
  "Extends the given path with a new tail element in the path. Will remove any disambiguator
   in the last path node if it's a major node before extending

   TODO check for major node, removes unconditionally right now"
   [oldpath newnode]
   ;; Filter out root node with {branch :nil}
   (let [cleanpath (filter :branch oldpath)]
    (if (empty? cleanpath)
      [newnode]
      (conj (vec (butlast cleanpath))
            (select-keys (last cleanpath) [:branch])
            newnode))))
     ; (cond
     ;  (= 0 (count cleanpath)) [newnode]
     ;  :else (conj (conj (vec (butlast cleanpath)) (select-keys (last cleanpath) (list :branch))) newnode))))

(defn- disamb-for-path
  "Returns the disambiguator for the node described by the given path"
  [path]
  (:disamb (last path)))

(defn- root-node
  "Returns a new root node"
  []
  [(pathnode nil)])

(defn- new-id
  "Returns a new position id between the two given ids

  There must be no already-existing id between the two paths.
  They must be adjacent"
  ;; TODO remove disambiguator from any non-mini node, we we only
  ;;      need it at the end
  [{pathl :path} {pathr :path}]
  (cond
    (and (nil? pathl) (nil? pathr)) (root-node)
    (and (nil? pathl) (seq pathr)) (extend-path pathr (pathnode 0)) ;; If we're inserting at the left-most position
    (ancestor? pathl pathr) (extend-path pathr (pathnode 0)) ;; If we need to make a left child of path b
    (ancestor? pathr pathl) (extend-path pathl (pathnode 1))
    (mini-sibling? pathl pathr) (conj pathl (pathnode 1)) ;; Maintain disambiguator if making a child of a mininode
    :else (extend-path pathl (pathnode 1))))

(defn- find-node
  "Finds the given node w/ linear search by comparing the values"
  [{oset :oset} tofind]
  (first (drop-while #(not= (:val %) tofind) oset)))

(defn ordered-set
  []
  (sorted-set-by item<?))

(defn- in
   "If item exists in coll, return item; else nil."
   [coll item]
   (some #{item} coll))

(defn- seq-diff
  "Returns a new sequence of all elements in seqa not contained in seqb."
  [seqa seqb]
  (filter #(not (in seqb %)) seqa))

(defn- removed-set
  [oset vc]
  (let [existing-paths (map :path oset)]
    (set (seq-diff (map first vc) existing-paths))))

(defprotocol IInsertAfter
  (insert-after [this previous new-elem] "Insert a new element after the desired element. If nil is supplied,
                                          inserts at the beginning."))

;; Steps to an insert:
;;  1) Find next item after position
;;  2) Get path/id between pos and next
;;  3) Insert new item with id
;;  4) Update lamport clock
;;
;; NOTE: Extremely inefficient at the moment. Finding the required node is worst-case
;;        O(n) if the node is at the end. O(n) inserts are no good.
;;       Consider better ways to do this---might require changing the API (just inserting based
;;         on the data means we'll always have to linear search for the right node)
;;       ideas...?
;;
;; Same goes for remove


;; Provide a collection interface implementation for the ordered-set
;;  so it can be used as a regular collection

;; Not a true lattice. join is not unique :-/
(deftype OrderedSet [oset vc]
  BoundedSemiLattice
  (bottom [self]
    (OrderedSet. (ordered-set) (kvs/->Causal {} nil)))

  (lte? [self that]
    (let [this-removed (removed-set oset vc)
          that-removed (removed-set (.oset that) (.vc that))]
      (and (lte? vc (.vc that))
           (clojure.set/subset? this-removed that-removed))))

  (join [self that]
    (let [new-in-that (set (seq-diff (.oset that) vc))
          vc-keys (keys (.vc that))
          removed-in-this (set (filter
                                #(in vc-keys (:path %))
                                (clojure.set/difference oset (.oset that))))
           updated-set (into (ordered-set)
                             (clojure.set/difference
                              (clojure.set/union oset new-in-that)
                               removed-in-this))]
      (OrderedSet. updated-set (join vc (.vc that)))))

  IInsertAfter
  (insert-after [self prev-data data]
    (let [[before after & _] (drop-while #(not= (:val %) prev-data) oset)
        after (if before after (first oset))
        path (new-id before after)]
    (if (or (and (nil? before) (not (nil? prev-data)))
            (= (:val after) data))
      self ;; If we didn't find the previous term (but it was specified) ignore
      (OrderedSet. (conj oset {:path path :val data})
                   (join vc {path (->Max (:clock (disamb-for-path path)))})))))

  clojure.lang.IPersistentCollection
  (seq [self] (seq (map :val oset)))
  (cons [self o] (insert-after self nil o))
  (empty [self] (empty oset))
  (equiv [self o] false) ;; TODO
  (count [self] (count (seq self))) ;; O(n)

  clojure.lang.ISeq
  (first [self] (:val (first oset)))
  (next [self]  (map :val (next oset)))
  (more [self]  (map :val (rest oset)))

  clojure.lang.IPersistentSet
  (disjoin [self item] (OrderedSet. (if-let [found (first (filter #(= (:val %) item) oset))]
                                      (disj oset found)
                                      oset)
                                    vc))
  (contains [self item] (seq (find-node self item))) ;; NOTE operates in linear time---breaks implicit contract in clojure docs
  (get [self item] nil) ;; TODO

  Object
  (toString [self] (apply str (interpose " " (map :val oset)))))

(defmethod print-method OrderedSet [o w]
        (.write w (.toString o)))

(defn ordered-set-lattice
  "Returns a new ordered set lattice"
  []
  (OrderedSet. (ordered-set) (kvs/->Causal {} nil)))
