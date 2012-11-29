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
                   (< l-disamb r-disamb) ;; Finishes with two mini-siblings, order by disambiguator
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

(deftest item<?-test
  ;; Basic tests for non-ambiguous paths
  (let [path (fn [branches]
              (build-simple-node branches "dis" "data"))]
    (is (item<? (path '()) (path '(1))))
    (is (item<? (path '(0)) (path '())))
    (is (item<? (path '(0)) (path '(1))))
    (is (item<? (path '(0)) (path '(1 0 1))))
    (is (item<? (path '(0)) (path '(1 1 0 1))))
    (is (item<? (path '(0)) (path '(0 1))))
    (is (item<? (path '(0)) (path '(0 1 0 0))))
    (is (item<? (path '(0)) (path '(0 1 1 1))))
    (is (item<? (path '(1 0)) (path '(1 1))))
    (is (item<? (path '(1 0)) (path '(1 0 1))))
    (is (item<? (path '(1 0)) (path '(1 1 1 1))))
    (is (item<? (path '(1 1 0)) (path '(1 1 1))))
    (is (item<? (path '(1 1 0)) (path '(1 1 1 0))))
    (is (item<? (path '(0 0 1)) (path '(0 1))))
    (is (item<? (path '(0 0 1)) (path '(0 1 0))))
    (is (not (item<? (path '()) (path '(0)))))
    (is (not (item<? (path '(0 0 1)) (path '(0 0)))))
    (is (not (item<? (path '(0 0 1))(path '(0 0 0)))))
    (is (not (item<? (path '(01 1)) (path '(0 0)))))
    (is (not (item<? (path '(1)) (path '()))))
    (is (not (item<? (path '(1 1 0)) (path '(0 1 1)))))
    (is (not (item<? (path '(1 1 0)) (path '(1 0 1)))))
    (is (not (item<? (path '(0 0 1)) (path '(0 0)))))))

(deftest orderedset-test
  (let [node (fn [branches data]
          (build-simple-node branches "dis" data))
        is! (fn [os val]
              (is (= os val))
              os)
        is-str! (fn [oseq s]
                  (is (= (apply str oseq) s))
                  oseq)]
  (-> (ordered-set)
    (conj (node '()     "c"))
    (conj (node '(0)    "b"))
    (conj (node '(0 0)  "a"))
    (conj (node '(1)    "e"))
    (conj (node '(1 0)  "d"))
    (conj (node '(1 1)  "f"))
    (conj (node '(0 0 0)  "q"))
    (set-as-values)
    (is! '("q" "a" "b" "c" "d" "e" "f"))
    (is-str! "qabcdef"))))
