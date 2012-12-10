(ns droplet.test.lattice.ordered-set
  (require clojure.set
           [droplet.test :as kvs])
  (use droplet
       droplet.lattice
       droplet.lattice.ordered-set
       clojure.test))

;; Test helpers

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

(defn strval
  [{oset :oset}]
  (apply str (set-as-values oset)))

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
    (conj (node '(1 1)    "f"))))

(defn make-filled-oset-lattice
  []
  (-> (->OrderedSet (ordered-set) (kvs/->Causal {} nil))
    (oset-insert nil "c")
    (oset-insert nil "a")
    (oset-insert "a" "b")
    (oset-insert "c" "d")
    (oset-insert "d" "e")
    (oset-insert "e" "f")))

;; Internal tests of ordering and tree functions

(deftest orderedset-test
  (-> (make-filled-oset)
    (conj (node '(0 0 0)  "q"))
    (is-contents! '("q" "a" "b" "c" "d" "e" "f"))
    (is-str! "qabcdef")))

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

;; Tests on lattice operations themselves
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
  (let [a (make-filled-oset-lattice)
        b (oset-insert a "f" "z") ;; Adds z after f
        c (oset-insert a "c" "l") ;; Adds l after c
        d (oset-remove a "c")]    ;; Removes
    (is-str-l! (join a b) "abcdefz")
    (is-str-l! (join a c) "abcldef")
    (is-str-l! (join b c) "abcldefz")
    (is-str-l! (join a (join b c)) "abcldefz")
    (is-str-l! (join a d) "abdef") ;; NOTE is this what we want? not commutative when joining two lattices w/ a removed element---yes
    (is-str-l! (join d a) "abcdef")
    (is-str-l! (join b d) "abdefz")
    (is-str-l! (join c d) "abldef")
    (is-str-l! (join b (join c d)) "abldefz")
    (is (lte? a b))
    (is (lte? a c))
    (is (lte? a d))
    (is (not (lte? b a)))
    (is (not (lte? c a)))
    (is (not (lte? d a)))))

(deftest mininode-test
  (let [orig (make-filled-oset-lattice)
        peer1 (oset-insert orig "c" "j")
        peer2 (oset-insert orig "c" "k")
        merged (join peer1 peer2)]
        ;; j and k should share mini-nodes, but j < k since lamport clock ordering
        ;;   is used to order them
      (is-str-l! merged "abcjkdef")
      (is-str-l! (oset-insert merged "c" "l") "abcljkdef")
      (is-str-l! (oset-insert merged "k" "l") "abcjkldef")
      ;; Inserting l between j and k will create a child of the j mininode and it should be ordered between
      (is-str-l! (oset-insert merged "j" "l") "abcjlkdef")))

(deftest nested-mininode-test
  (let [orig (-> (->OrderedSet (ordered-set) (kvs/->Causal {} nil))
                (oset-insert nil "c")
                (oset-insert nil "a")
                (oset-insert "c" "f"))
        one (-> orig
                (oset-insert "c" "e")
                (oset-insert "c" "d"))
        two (-> orig
                (oset-insert "c" "z")
                (oset-insert "c" "q"))
        three (oset-insert one "d" "r")]
    (is-str-l! orig "acf")
    (is-str-l! one "acdef")
    (is-str-l! two "acqzf")
    (is-str-l! three "acdref")
    (is-str-l! (join one two) "acdqezf")
    (is-str-l! (join three two) "acdqrezf")))

;; Ensure that joins can happen in any order and the result will converge to the LUB regardless
(deftest reorder-test
  (let [orig (make-filled-oset-lattice)
        one  (oset-remove orig "d")
        two  (oset-insert one "b" "z")
        three  (oset-insert two "a" "q")]
    (is (= (strval three) (strval (join (join (join orig one) two) three))))
    (is (= (strval three) (strval (join (join (join orig three) two) one))))
    (is (= (strval three) (strval (join (join (join orig two) three) one))))
    (is (= (strval three) (strval (join (join (join orig one) three) two))))))
