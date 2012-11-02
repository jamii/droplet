(ns droplet.examples
  (:use clojure.test)
  (:require droplet.lattice
            droplet.lattice.set
            droplet.instant))

(defn connected-to [x]
  (-> {'edge (droplet.lattice/->Set #{[1 2] [2 3] [3 3] [4 5] [5 4]})
       'path (droplet.lattice/->Set #{})}
      (droplet.instant/fixpoint [(droplet.lattice.set/rule edge [?a ?b]
                                                           (conj path [a b])
                                                           path [?b ?c]
                                                           (conj path [a c]))])
      ((droplet.lattice.set/query result
                                  path [?a x]
                                  (conj result a)))))

(deftest connected-to-test
  (is (= #{} (connected-to 1)))
  (is (= #{1} (connected-to 2)))
  (is (= #{1 2 3} (connected-to 3)))
  (is (= #{4 5} (connected-to 4)))
  (is (= #{4 5} (connected-to 5))))
