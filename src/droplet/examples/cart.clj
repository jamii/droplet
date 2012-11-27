;; Example implementation of distributed shopping cart
(ns droplet.examples.cart
  (require clojure.set
           [droplet.datalog :as d])
  (use clojure.test
       droplet))

(def ACTION_OP 0)
(def CHECKOUT_OP 1)

;; Cart lattice has an actions map {op-id #{[actions]}} and an output checkout map {item-id count}
;; First item in [vals] is either ACTION_OP or CHECKOUT_OP, rest are args to the operation
;; The lattice has a monotone is-complete function, returns when a checkout has happened
(defrecord CartLattice [actions]
  SemiLattice
  (bottom [this]
    (->CartLattice {}))
  (lte? [this that]
    ;; what does lte? mean in this case?
    (lte? (:actions this) (:actions that)))
  (join [this that]
    (->CartLattice (join (:actions this) (:actions that)))))


(def cart-states
  {:sessions {}
   :add #{}
   :checkout #{}})

;; Monotonic (once a checkout action has been sent, this always returns the same value. until it has, returns nil)
(defn cart-summary
  [sessions userid from-op to-op]
  (apply merge-with + {}
      (let [actions (:actions (get sessions userid))
            action-ids (sort (keys actions))]
        (if (and (= CHECKOUT_OP (nth (first (get actions to-op)) 0))
                 (every? #(some (fn [x] (= %1 x)) action-ids) (range from-op to-op))) ;; All ids in the range are in the list of actions
          (for [id (range from-op to-op)]
              {(nth (first (get actions id)) 1) (nth (first (get actions id)) 2)})
          (throw (Exception. "Tried to call checkout but missing ids in range, or missing CHECKOUT_OP at id")))
        )))

(def cart-rules
  [(persistent :sessions)
   (ephemeral :add)
   (ephemeral :checkout)
   (d/deduct :sessions [userid (->CartLattice {opid #{[ACTION_OP item cnt]}})]
             :add {:userid ?userid :opid ?opid :item ?item :cnt ?cnt})
   (d/deduct :sessions [userid (->CartLattice {opid #{[CHECKOUT_OP from-op]}})]
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
    (let [result (checkout! c 999 5 1)]
      (if-not (= (get result "beer") 6)
        (throw (agent-error c)))
      (if-not (= (get result "gin") 3)
        (throw (agent-error c)))
      (if-not (= (get result "absinthe") 9)
        (throw (agent-error c))))))

; (:sessions (:states @c))