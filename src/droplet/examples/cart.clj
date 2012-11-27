;; Example implementation of distributed shopping cart
(ns droplet.examples.cart
  (require clojure.set
           [droplet.datalog :as d])
  (use clojure.test
       droplet))

(def ACTION_OP 0)
(def CHECKOUT_OP 1)

;; A lattice that's not a true lattice---values
;; are simply copied over if there is a key conflict
;; TODO should use e.g. vector clocks for opid
(defrecord Cart [actions]
  SemiLattice
  (bottom [this]
    (->Cart {}))
  (lte? [this that]
    (lte? (:actions this) (:actions that)))
  (join [this that]
    (->Cart (merge-with (fn [one two] two) (:actions this) (:actions that)))))

(def cart-states
  {:sessions {}
   :add #{}
   :checkout #{}})

(defn cart-summary
  [sessions userid from-op to-op]
  (if-not (and ;; Make sure the desired to-op is a CHECKOUT action, and that we have all ids in the range
            (= CHECKOUT_OP (get-in sessions [userid :actions to-op :op]))
            (every? #(some (fn [x] (= %1 x)) (keys (get-in sessions [userid :actions]))) (range from-op to-op)))
    (throw (Exception. "Tried to call checkout but missing ids in range, or missing CHECKOUT_OP at id")))
  (apply merge-with + {}
    (for [opid (range from-op to-op)]
      {(get-in sessions [userid :actions opid :item]) (get-in sessions [userid :actions opid :cnt])})))

(def cart-rules
  [(persistent :sessions)
   (ephemeral :add)
   (ephemeral :checkout)
   (d/deduct :sessions [userid (->Cart {opid {:op ACTION_OP :item item :cnt cnt}})]
             :add {:userid ?userid :opid ?opid :item ?item :cnt ?cnt})
   (d/deduct :sessions [userid (->Cart {opid {:op CHECKOUT_OP :from-op from-op}})]
             :checkout {:userid ?userid :opid ?opid :from-op ?from-op})])

(defn add-to-cart! [carts userid opid item cnt]
  (insert! carts :add {:userid userid :opid opid :item item :cnt cnt}))

;; Returns a map of {item-id num-items}
(defn checkout! [carts userid opid from-op]
  (insert! carts :checkout {:userid userid :opid opid :from-op from-op})
  (if (await-for 1000 carts)
    (cart-summary (get-in @carts [:states :sessions]) userid from-op opid)))

(defn make-carts []
  (reactive cart-states cart-rules))

(deftest basic-cart-test
  (let [c (make-carts)]
    (add-to-cart! c 999 1 "beer" 5)
    (add-to-cart! c 999 2 "gin" 3)
    (add-to-cart! c 999 3 "absinthe" 9)
    (add-to-cart! c 999 4 "beer" 1)
    (add-to-cart! c 111 5 "cookies" 17)
    (add-to-cart! c 111 6 "cookies" -3)
    (add-to-cart! c 111 7 "cookies" -1)
    (let [result (checkout! c 999 5 1)]
      (if-not (= (get result "beer") 6)
        (throw (agent-error c)))
      (if-not (= (get result "gin") 3)
        (throw (agent-error c)))
      (if-not (= (get result "absinthe") 9)
        (throw (agent-error c))))
    (let [result (checkout! c 111 8 5)]
      (if-not (= (get result "cookies") 13)
        (throw (agent-error c))))))
;
;
; (:sessions (:states @c))