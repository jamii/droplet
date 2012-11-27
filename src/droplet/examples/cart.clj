;; Example implementation of distributed shopping cart
(ns droplet.examples.cart
  (require clojure.set
           [droplet.datalog :as d]
           [droplet.test :as kvs])
  (use clojure.test
       droplet))

(def ACTION_OP 0)
(def CHECKOUT_OP 1)

;; Cart is :actions: {opid (->Causal vc {args})}
;; If two actions have the same opid, the vc is used to
;;  break the tie. If there is no causal ordering, last one wins.
(defrecord Cart [actions]
  SemiLattice
  (bottom [this]
    (->Cart {}))
  (lte? [this that]
    (lte? (:actions this) (:actions that)))
  (join [this that]
    (->Cart (merge-with (fn [one two]
                          (cond
                             (lt? (:vc one) (:vc two)) two
                             (lt? (:vc two) (:vc one)) one
                             :else (kvs/->Causal (join (:vc one) (:vc two)) (:val two))))
              (:actions this) (:actions that)))))

(def cart-states
  {:sessions {}
   :add #{}
   :checkout #{}})

(defn cart-summary
  [sessions userid from-op to-op]
  (if-not (and ;; Make sure the desired to-op is a CHECKOUT action, and that we have all ids in the range
            (= CHECKOUT_OP (get-in sessions [userid :actions to-op :val :op]))
            (every? #(some (fn [x] (= %1 x)) (keys (get-in sessions [userid :actions]))) (range from-op to-op)))
    (throw (Exception. "Tried to call checkout but missing ids in range, or missing CHECKOUT_OP at id")))
  (apply merge-with + {}
    (for [opid (range from-op to-op)]
      {(get-in sessions [userid :actions opid :val :item]) (get-in sessions [userid :actions opid :val :cnt])})))

(def cart-rules
  [(persistent :sessions)
   (ephemeral :add)
   (ephemeral :checkout)
   (d/deduct :sessions [userid (->Cart {opid (kvs/->Causal vc {:op ACTION_OP :item item :cnt cnt})})]
             :add {:vc ?vc :userid ?userid :opid ?opid :item ?item :cnt ?cnt})
   (d/deduct :sessions [userid (->Cart {opid (kvs/->Causal vc {:op CHECKOUT_OP :from-op from-op})})]
             :checkout {:vc ?vc :userid ?userid :opid ?opid :from-op ?from-op})])

(defn add-to-cart! [carts vc userid opid item cnt]
  (insert! carts :add {:vc vc :userid userid :opid opid :item item :cnt cnt}))

;; Returns a map of {item-id num-items}
(defn checkout! [carts vc userid opid from-op]
  (insert! carts :checkout {:vc vc :userid userid :opid opid :from-op from-op})
  (if (await-for 1000 carts)
    (cart-summary (get-in @carts [:states :sessions]) userid from-op opid)))

(defn make-carts []
  (reactive cart-states cart-rules))

(deftest basic-cart-test
  (let [c (make-carts)]
    (add-to-cart! c (kvs/vc :alice 1) 999 1 "beer" 5)
    (add-to-cart! c (kvs/vc :bob 4)   999 2 "gin" 3)
    (add-to-cart! c (kvs/vc :bob 4 :chad 3)   999 2 "gin" 114) ;; causal order to make this version of 'op 2' win
    (add-to-cart! c (kvs/vc :alice 1 :bob 4 :chad 3)   999 3 "absinthe" 9) ;; the rest don't conflict on keys
    (add-to-cart! c (kvs/vc :alice 1 :bob 4 :chad 3)   999 4 "beer" 1)
    (add-to-cart! c (kvs/vc :alice 1 :bob 4 :chad 3)   111 5 "cookies" 17)
    (add-to-cart! c (kvs/vc :alice 1 :bob 4 :chad 3)   111 6 "cookies" -3)
    (add-to-cart! c (kvs/vc :alice 1 :bob 4 :chad 3)   111 7 "cookies" -1)
    (let [result (checkout! c (kvs/vc :alice 1 :bob 4 :chad 3) 999 5 1)]
      (if-not (= (get result "beer") 6)
        (throw (agent-error c)))
      (if-not (= (get result "gin") 114)
        (throw (agent-error c)))
      (if-not (= (get result "absinthe") 9)
        (throw (agent-error c))))
    (let [result (checkout! c (kvs/vc :alice 1 :bob 4 :chad 3) 111 8 5)]
      (if-not (= (get result "cookies") 13)
        (throw (agent-error c))))))
;
;
; (:sessions (:states @c))