(ns droplet.instant)

(defprotocol Rule
  (run [this states] "Apply this rule now"))

(defn fixpoint [states rules]
  (let [new-states (reduce #(run %2 %1) states rules)]
    (if (= states new-states)
      new-states
      (recur new-states rules))))
