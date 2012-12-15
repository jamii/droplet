(ns droplet.test.lattice.set
  (require clojure.set)
  (use droplet
     droplet.lattice
     droplet.lattice.set
     clojure.test))

;; Test follows Fig. 14 in full CRDT paper
(defn test-orset-api
  [orig-set]
  (let [a  orig-set
        b  a
        c  a
        b (conj b :a)
        a (conj a :a)
        c (join c b)
        c (join c a)
        a (disj a :a)
        c (join c a)
        a (join a b)]
    (is (seq a) '(:a))
    (is (seq b) '(:a))
    (is (seq c) '(:a))
    (is (.mmap a) {:a #{1}})
    (is (.mmap b) {:a #{1}})
    (is (.mmap c) {:a #{1}})
    (list a b c)))

(deftest test-orset
  (let [[a b c] (test-orset-api (or-set-tombs))]
    (is (.tombstones a) {:a #{2}})
    (is (.tombstones b) {})
    (is (.tombstones c) {:a #{2}})
    (let [b (join b a)]
      (is (.tombstones b) {:a #{2}}))))

(deftest test-orset-vv
  (test-orset-api (or-set-tombless)))