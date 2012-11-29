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
; (defrecord SetItem [path val])

(defrecord OrderedSet [oset vc]
  SemiLattice
  (bottom [this]
  )
  (lte? [this that]
  )
  (join [this that]
  ))

;; Compare by path and break ties with disambiguator
(defn item-comparator
  [{patha :path} {pathb :path}]
  (loop [patha patha
         pathb pathb]
    (let [{l :branch l-disamb :disamb} (first patha)
          {r :branch r-disamb :disamb} (first pathb)]
      (cond
        (nil? l) (= r 1)
        (nil? r) (= l 0)
        (= l r)  (if (and (= 1 (count patha)) (= 1 (count pathb)))
                  (< l-disamb r-disamb) ;; Finishes with two mini-siblings, order by disambiguator
                  (recur (subvec patha 1) (subvec pathb 1))) ;; Normal case
        :else    (< l r)))))

;; Builds a simple path (that is, no nodes in path are minor nodes)
;;  Takes a list of branches and a disambiguator
(defn build-simple-path
  [branches disamb val]
  (let [path (into []
               (for [x branches] {:branch x}))
        with-disamb (conj (subvec path 0 (max 0 (- (count path) 1))) (assoc (last path) :disamb disamb))]
    {:path with-disamb :val val}))

(deftest comparator-test
  ;; Basic tests for non-ambiguous paths
  (let [path (fn [branches]
              (build-simple-path branches "dis" "data"))]
    (is (item-comparator (path '(0)) (path '())))
    (is (item-comparator (path '(0)) (path '(1))))
    (is (item-comparator (path '(0)) (path '(1 0 1))))
    (is (item-comparator (path '(0)) (path '(1 1 0 1))))
    (is (item-comparator (path '(0)) (path '(0 1))))
    (is (item-comparator (path '(0)) (path '(0 1 0 0))))
    (is (item-comparator (path '(0)) (path '(0 1 1 1))))
    (is (item-comparator (path '(1 0)) (path '(1 1))))
    (is (item-comparator (path '(1 0)) (path '(1 0 1))))
    (is (item-comparator (path '(1 0)) (path '(1 1 1 1))))
    (is (item-comparator (path '(1 1 0)) (path '(1 1 1))))
    (is (item-comparator (path '(1 1 0)) (path '(1 1 1 0))))
    (is (item-comparator (path '(0 0 1)) (path '(0 1))))
    (is (item-comparator (path '(0 0 1)) (path '(0 1 0))))
    (is (not (item-comparator (path '(0 0 1)) (path '(0 0)))))
    (is (not (item-comparator (path '(0 0 1))(path '(0 0 0)))))
    (is (not (item-comparator (path '(01 1)) (path '(0 0)))))
    (is (not (item-comparator (path '(1)) (path '()))))
    (is (not (item-comparator (path '(1 1 0)) (path '(0 1 1)))))
    (is (not (item-comparator (path '(1 1 0)) (path '(1 0 1)))))
    (is (not (item-comparator (path '(0 0 1)) (path '(0 0)))))))
