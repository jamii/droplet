(ns droplet.datatypes.orderedset
  (require clojure.set
       [droplet.datalog :as d])
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
; (defrecord SetItem [path val])

(defrecord OrderedSet [oset clock]
  SemiLattice
  (bottom [this]
  )
  (lte? [this that]
  )
  (join [this that]
  ))

(defn set-as-values
  [oset]
  (for [{val :val} (seq oset)]
    val))

(defn disamb<?
  [disamb-a disamb-b]
  (if (not= (:clock disamb-a) (:clock disamb-b))
    (< (:clock disamb-a) (:clock disamb-b)) ;; If there is a lamport clock ordering, use that
    (< (:siteid disamb-a) (:siteid disamb-b)))) ;; else order by MAC address

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

(defn ordered-set
  []
  (sorted-set-by item<?))

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
    (build-simple-node branches {:clock 3 :disamb "MACADDR"} "data"))
  ([branches data]
    (build-simple-node branches {:clock 3 :disamb "MACADDR"} data))
  ([branches data disamb]
    (build-simple-node branches {:clock disamb :disamb "MACADDR"} data)))

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

(deftest orderedset-test
  (let [is! (fn [os val]
              (is (= os val))
              os)
        is-str! (fn [oseq s]
                  (is (= (apply str oseq) s))
                  oseq)]
  (-> (ordered-set)
    (conj (node '()       "c"))
    (conj (node '(0)      "b"))
    (conj (node '(0 0)    "a"))
    (conj (node '(1)      "e"))
    (conj (node '(1 0)    "d"))
    (conj (node '(1 1)    "f"))
    (conj (node '(0 0 0)  "q"))
    (set-as-values)
    (is! '("q" "a" "b" "c" "d" "e" "f"))
    (is-str! "qabcdef"))))
