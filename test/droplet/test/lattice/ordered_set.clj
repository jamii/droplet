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
  [oset s]
  (is (= (apply str oset) s))
  oset)

(defn is-contents!
  [oset val]
  (is (= (set-as-values oset) val))
  oset)

(defn strval
  [oset]
  (apply str oset))

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
  (-> (ordered-set-lattice)
    (insert-after nil "c")
    (insert-after nil "a")
    (insert-after "a" "b")
    (insert-after "c" "d")
    (insert-after "d" "e")
    (insert-after "e" "f")))

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
    (insert-after "a" "m")
    (is-str-l! "ambcdef")
    (insert-after "d" "q")
    (is-str-l! "ambcdqef")
    (insert-after "q" "z")
    (is-str-l! "ambcdqzef")
    (insert-after "f" "l")
    (is-str-l! "ambcdqzefl")
    (insert-after "f" "l")
    (is-str-l! "ambcdqzefl")
    (insert-after "q" "uuuu")
    (is-str-l! "ambcdquuuuzefl")
    (insert-after "notfound" "notinserted")
    (is-str-l! "ambcdquuuuzefl")
    (disj "m")
    (is-str-l! "abcdquuuuzefl")
    (disj "uuuu")
    (is-str-l! "abcdqzefl")
    (disj "f")
    (is-str-l! "abcdqzel")
    (disj "s")
    (is-str-l! "abcdqzel")))

(deftest lattice-test
  (let [a (make-filled-oset-lattice)
        b (insert-after a "f" "z") ;; Adds z after f
        c (insert-after a "c" "l") ;; Adds l after c
        d (disj a "c")]    ;; Removes
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
        peer1 (insert-after orig "c" "j")
        peer2 (insert-after orig "c" "k")
        merged (join peer1 peer2)]
        ;; j and k should share mini-nodes, but j < k since lamport clock ordering
        ;;   is used to order them
      (is-str-l! merged "abcjkdef")
      (is-str-l! (insert-after merged "c" "l") "abcljkdef")
      (is-str-l! (insert-after merged "k" "l") "abcjkldef")
      ;; Inserting l between j and k will create a child of the j mininode and it should be ordered between
      (is-str-l! (insert-after merged "j" "l") "abcjlkdef")))

(deftest nested-mininode-test
  (let [orig (-> (->OrderedSet (ordered-set) (kvs/->Causal {} nil))
                (insert-after nil "c")
                (insert-after nil "a")
                (insert-after "c" "f"))
        one (-> orig
                (insert-after "c" "e")
                (insert-after "c" "d"))
        two (-> orig
                (insert-after "c" "z")
                (insert-after "c" "q"))
        three (insert-after one "d" "r")]
    (is-str-l! orig "acf")
    (is-str-l! one "acdef")
    (is-str-l! two "acqzf")
    (is-str-l! three "acdref")
    (is-str-l! (join one two) "acdqezf")
    (is-str-l! (join three two) "acdqrezf")))

;; Ensure that joins can happen in any order and the result will converge to the LUB regardless
(deftest reorder-test
  (let [orig (make-filled-oset-lattice)
        one  (disj orig "d")
        two  (insert-after one "b" "z")
        three  (insert-after two "a" "q")]
    (is (= (strval three) (strval (join (join (join orig one) two) three))))
    (is (= (strval three) (strval (join (join (join orig three) two) one))))
    (is (= (strval three) (strval (join (join (join orig two) three) one))))
    (is (= (strval three) (strval (join (join (join orig one) three) two))))))
