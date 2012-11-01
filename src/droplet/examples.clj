(ns droplet.examples
  (:use droplet.lattice
        droplet.fixpoint
        clojure.test))

(defn connected-to [x]
  (-> {:edge #{[1 2] [2 3] [3 3] [4 5] [5 4]}}
      (fixpoint [(->Join (->Set) :path (query [edge] edge))
                 (->Join (->Set) :path (query [edge path]
                                              (set (for [[a b] edge
                                                         [b' c] path
                                                         :when (= b b')]
                                                     [a c]))))])
      ((query [path]
              (set (for [[a b] path
                         :when (= x b)]
                     a))))))

(deftest connected-to-test
  (is (= #{} (connected-to 1)))
  (is (= #{1} (connected-to 2)))
  (is (= #{1 2 3} (connected-to 3)))
  (is (= #{4 5} (connected-to 4)))
  (is (= #{4 5} (connected-to 5))))
