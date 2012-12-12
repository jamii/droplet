(ns droplet.test.lattice.set
  (require clojure.set)
  (use droplet
     droplet.lattice
     droplet.lattice.set
     clojure.test))

;; Test follows Fig. 14 in full CRDT paper
(deftest test-orset
  (let [a (or-set-tombs)
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
    (is (.m a) {:a #{1}})
    (is (.m b) {:a #{1}})
    (is (.m c) {:a #{1}})
    (is (.t a) {:a #{2}})
    (is (.t b) {})
    (is (.t c) {:a #{2}})
    (let [b (join b a)]
      (is (.t b) {:a #{2}}))))