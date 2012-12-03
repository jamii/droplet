(ns droplet.datatypes.orderedset
  (require clojure.set
           [droplet.datalog :as d]
           [droplet.test :as kvs])
  (use droplet
       clojure.test))

;; Provides an ordered set lattice inspired by Treedoc:
;;  http://hal.inria.fr/inria-00445975
;;  http://run.unl.pt/bitstream/10362/7802/1/Sousa_2012.pdf

;; An ordered set has the two following operations as well as the lattice lte? and join methods:
;;  insert(pos, elem)
;;  delete(pos)
;;
;; where pos is a position identifier, which is equivalent to a path in the tree that backs the set.

;; Each item in the ordered set contains a path, which is a list of {:path [{:branch :disamb}] :val val} and the payload value
;; A disambiguator for a non-tail node is omitted if the path passes through a major node. It is included if the path
;;  goes through a minor node
;; A disambiguator is a (clock, siteid) pair where clock is a lamport clock, the clock value that a node had when inserting that value
;; (defrecord SetItem [path val])

;;
;; Note that we diverge from the algorithm described in the Souza2012 paper when it comes to the Version Vector. The paper described
;;  the version vector as containing {disamb(node), counter} pairs, but if we only keep track of disambiguators, in the compare function
;;  we cannot get a list of node paths that do not exist in the set *but* have have an entry in the version vector---we would need to
;;  keep a list of "removed nodes" by disambiguator. Instead we make the VV key be the path+disambiguator, so we can retrieve the path
;;  directly.
;;

;;
;; TODO optimize by only storing disambiguators for nodes that need it (last node or mini-nodes)
;;

(def clock (atom 0))

(defn clock-value
  "Next value for this node's vector clock id
  TODO"
  []
  (swap! clock inc)
  @clock)

(defn set-as-values
  [oset]
  (for [{val :val} (seq oset)]
    val))

(defn siteid<?
  "Returns a < relation for two site ids, that are usually MAC addresses.

  TODO real mac addresses.. for now just string comparison"
  [macaddra macaddrb]
  (< (.compareTo macaddra macaddrb) 0))

(defn disamb<?
  [disamb-a disamb-b]
  (if (not= (:clock disamb-a) (:clock disamb-b))
    (< (:clock disamb-a) (:clock disamb-b)) ;; If there is a lamport clock ordering, use that
    (siteid<? (:siteid disamb-a) (:siteid disamb-b)))) ;; else order by MAC address

;; Compare by path and break ties (mini-nodes are siblings in same major node) with disambiguator
(defn item<?
  [{patha :path} {pathb :path}]
  (loop [patha patha
         pathb pathb]
    (let [{l :branch l-disamb :disamb} (first patha)
          {r :branch r-disamb :disamb} (first pathb)]
      (cond
        (nil? l) (= r 1)
        (nil? r) (= l 0)
        (= l r)  (if (and (= 1 (count patha)) (= 1 (count pathb)))
                   (disamb<? l-disamb r-disamb) ;; Finishes with two mini-siblings, order by disambiguator
                   (recur (subvec patha 1) (subvec pathb 1))) ;; Normal case
        :else    (< l r)))))

(defn path-len
  "Returns the path length for this path, which is usually the number of pairs
   except for the root which has a pair but no branch"
  [path]
  (if (and (= 1 (count path)) (nil? (:branch (first path))))
    0
    (count path)))

(defn ancestor?
  "Returns if the first path is an ancestor to the second"
  [patha pathb]
  (if-not (>= (path-len patha) (path-len pathb)) ;; If path A is longer or equal, no way it can be an ancestor
    (loop [patha patha                           ;; Otherwise, determine if it's an ancestor
           pathb pathb]
      (let [{l :branch l-disamb :disamb} (first patha)
            {r :branch r-disamb :disamb} (first pathb)]
        (cond
          (nil? l) true
          (= l r)  (recur (subvec patha 1) (subvec pathb 1)) ;; Normal case, equal so continue down the tree
          :else    nil)))))

(defn mini-sibling?
  "Returns true if the two paths are mini-siblings of each other,
   that is, they have the same path but differ in disambiguators"
   [patha pathb]
   (= (for [{branch :branch} patha] branch)
      (for [{branch :branch} patha] branch)))

(defn pathnode
  "Returns a new pathnode with the given branch
   and automatically-filled in disambiguator

   TODO use real lamport clock and macaddr for this!"
   [branch]
   {:branch branch :disamb {:clock (clock-value) :siteid "MACADDRESS"}})

(defn disamb-for-path
  "Returns the disambiguator for the node described by the given path"
  [path]
  (:disamb (last path)))

(defn new-id
  "Returns a new position id between the two given ids

  There must be no already-existing id between the two paths.
  They must be adjacent"
  ;; TODO remove disambiguator from any non-mini node, we we only
  ;;      need it at the end
  [{patha :path} {pathb :path}]
  (cond
    (and (nil? patha) (nil? pathb)) [] ;; If it's an empty tree, create the root
    (and (nil? patha) (not (nil? pathb))) (conj pathb (pathnode 0)) ;; If we're inserting at the left-most position
    (ancestor? patha pathb) (conj pathb (pathnode 0)) ;; If we need to make a left child of path b
    (ancestor? pathb patha) (conj patha (pathnode 1))
    (mini-sibling? patha pathb) (conj patha (pathnode 1))
    :else (conj patha (pathnode 1))))

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
;;       Try a zipper?
;;
;; Same goes for remove
;;
;; TODO lamport clock
(defn oset-insert
  "Inserts a new item in the ordered set after the specified item. If nil is supplied,
  inserts at the beginning.
  Returns the new ordered set"
  [{oset :oset :as oset-lattice} prev-data data]
  (let [from-prev (seq (drop-while #(not= (:val %) prev-data) oset))
        before (first from-prev)
        after (if before (second from-prev) (first oset))
        path (new-id before after)]
    (if (and (nil? before) (not (nil? prev-data)))
      oset-lattice ;; If we didn't find the previous term (but it was specified) ignore
      (-> oset-lattice ;; Insert item and update item in vc
        (update-in [:oset] conj {:path path :val data})
        (update-in [:vc] #(join % {path (->Max (:clock (disamb-for-path path)))}))))))

(defn oset-remove
  "Removes the desired item from this set and returns it"
  [{oset :oset :as oset-lattice} item]
  (if-let [found (first (filter #(= (:val %) item) oset))]
    (assoc oset-lattice :oset (disj oset found))
    oset-lattice))

(defn ordered-set
  []
  (sorted-set-by item<?))

(defn removed-set
  [oset-lattice]
  (let [existing-paths (for [node (:oset oset-lattice)] (:path node))]
    (set (for [item (:vc oset-lattice) :when (not (some #{(first item)} existing-paths))] (first item)))))

(defrecord OrderedSet [oset vc]
  SemiLattice
  (bottom [this]
    (->OrderedSet (ordered-set) (kvs/->Causal {} nil)))
  (lte? [this that]
    (let [this-removed (removed-set this)
          that-removed (removed-set that)]
      (and (lte? (:vc this) (:vc that))
           (clojure.set/subset? this-removed that-removed))))
  (join [this that]
    (let [new-in-that (set (for [item (:oset that) :when (not (some #{(:path item)} (:vc this)))] item)) ;; New items added in that that were not removed in this
          removed-in-this (set (for [item (clojure.set/difference (:oset this) (:oset that)) :when (some #{(:path item)} (:vc that))] item)) ;; Items removed in this which that already knew about
          updated-set (clojure.set/union (:oset this (clojure.set/difference new-in-that removed-in-this)))]
      (->OrderedSet updated-set (join (:vc this) (:vc that))))))

;; Build a simple path (that is, no nodes in path are minor nodes)
;;  from a list of branches and a disambiguator
(defn build-simple-node
  [branches disamb val]
  (let [path (into []
               (for [x branches] {:branch x}))]
    {:path (conj (subvec path 0 (max 0 (- (count path) 1)))
                 (assoc (last path) :disamb disamb))
     :val val}))

(defn set-as-values
  [oset]
  (for [{val :val} (seq oset)]
    val))

(defn node
  ([branches]
    (build-simple-node branches {:clock 3 :siteid "MACADDR"} "data"))
  ([branches data]
    (build-simple-node branches {:clock 3 :siteid "MACADDR"} data))
  ([branches data disamb]
    (build-simple-node branches {:clock disamb :siteid "MACADDR"} data)))

(defn make-filled-oset
  []
  (-> (ordered-set)
    (conj (node '()       "c"))
    (conj (node '(0)      "b"))
    (conj (node '(0 0)    "a"))
    (conj (node '(1)      "e"))
    (conj (node '(1 0)    "d"))
    (conj (node '(1 1)    "f")))))

(defn make-filled-oset-lattice
  []
  (-> (->OrderedSet (ordered-set) (kvs/->Causal {} nil))
    (oset-insert nil "c")
    (oset-insert nil "a")
    (oset-insert "a" "b")
    (oset-insert "c" "d")
    (oset-insert "d" "e")
    (oset-insert "e" "f")))

(deftest item<?-test
;; Basic tests for non-ambiguous nodes
  (is (item<? (node '()) (node '(1))))
  (is (item<? (node '(0)) (node '())))
  (is (item<? (node '(0)) (node '(1))))
  (is (item<? (node '(0)) (node '(1 0 1))))
  (is (item<? (node '(0)) (node '(1 1 0 1))))
  (is (item<? (node '(0)) (node '(0 1))))
  (is (item<? (node '(0)) (node '(0 1 0 0))))
  (is (item<? (node '(0)) (node '(0 1 1 1))))
  (is (item<? (node '(1 0)) (node '(1 1))))
  (is (item<? (node '(1 0)) (node '(1 0 1))))
  (is (item<? (node '(1 0)) (node '(1 1 1 1))))
  (is (item<? (node '(1 1 0)) (node '(1 1 1))))
  (is (item<? (node '(1 1 0)) (node '(1 1 1 0))))
  (is (item<? (node '(0 0 1)) (node '(0 1))))
  (is (item<? (node '(0 0 1)) (node '(0 1 0))))
  (is (not (item<? (node '()) (node '(0)))))
  (is (not (item<? (node '(0 0 1)) (node '(0 0)))))
  (is (not (item<? (node '(0 0 1))(node '(0 0 0)))))
  (is (not (item<? (node '(01 1)) (node '(0 0)))))
  (is (not (item<? (node '(1)) (node '()))))
  (is (not (item<? (node '(1 1 0)) (node '(0 1 1)))))
  (is (not (item<? (node '(1 1 0)) (node '(1 0 1)))))
  (is (not (item<? (node '(0 0 1)) (node '(0 0))))))

(deftest disambiguator-test
  (is (item<? (node '(0 1) "a" 3) (node '(0 1) "a" 4)))
  (is (item<? (node '(0 1) "a" 3) (node '(0 1) "b" 7)))
  (is (item<? (node '(0 0) "a" 3) (node '(0 1) "a" 4)))
  (is (item<? (node '(1 1 0) "a" 0) (node '(1 1 0) "a" 1)))
  (is (not (item<? (node '(0 1) "a" 3) (node '(0 1) "a" 2))))
  (is (not (item<? (node '(1 0 1) "z" 6) (node '(1 0 1) "a" 2)))))

(deftest ancestor-test
  (is (ancestor? (:path (node '())) (:path (node '(0)))))
  (is (ancestor? (:path (node '())) (:path (node '(1)))))
  (is (ancestor? (:path (node '())) (:path (node '(0 1)))))
  (is (ancestor? (:path (node '())) (:path (node '(1 0)))))
  (is (ancestor? (:path (node '(0))) (:path (node '(0 1)))))
  (is (ancestor? (:path (node '(0))) (:path (node '(0 1 1)))))
  (is (ancestor? (:path (node '(1 0))) (:path (node '(1 0 0 1)))))
  (is (not (ancestor? (:path (node '(0))) (:path (node '(1))))))
  (is (not (ancestor? (:path (node '(1))) (:path (node '(0))))))
  (is (not (ancestor? (:path (node '(1))) (:path (node '())))))
  (is (not (ancestor? (:path (node '(1))) (:path (node '(0 0 1))))))
  (is (not (ancestor? (:path (node '(1 0 1))) (:path (node '(0 1 0)))))))

(defn is-str! 
  [oset s]
  (is (= (apply str (set-as-values oset)) s))
  oset)

(defn is-str-l! 
  [{oset :oset :as oset-lattice} s]
  (is (= (apply str (set-as-values oset)) s))
  oset-lattice)

(defn is-contents!
  [oset val]
  (is (= (set-as-values oset) val))
  oset)

(deftest orderedset-test
  (-> (make-filled-oset)
    (conj (node '(0 0 0)  "q"))
    (is-contents! '("q" "a" "b" "c" "d" "e" "f"))
    (is-str! "qabcdef")))

(deftest operation-test
  (-> (make-filled-oset-lattice)
    (oset-insert "a" "m")
    (is-str-l! "ambcdef")
    (oset-insert "d" "q")
    (is-str-l! "ambcdqef")
    (oset-insert "q" "z")
    (is-str-l! "ambcdqzef")
    (oset-insert "f" "l")
    (is-str-l! "ambcdqzefl")
    (oset-insert "q" "uuuu")
    (is-str-l! "ambcdquuuuzefl")
    (oset-insert "notfound" "notinserted")
    (is-str-l! "ambcdquuuuzefl")
    (oset-remove "m")
    (is-str-l! "abcdquuuuzefl")
    (oset-remove "uuuu")
    (is-str-l! "abcdqzefl")
    (oset-remove "f")
    (is-str-l! "abcdqzel")
    (oset-remove "s")
    (is-str-l! "abcdqzel")))

(deftest lattice-test
  (-> (make-filled-oset-lattice)))

