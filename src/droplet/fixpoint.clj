(ns droplet.fixpoint)

(defprotocol Rule
  (run [this env] "Update the environment"))

(defn fixpoint [env rules]
  (let [new-env (reduce #(run %2 %1) env rules)]
    (if (= env new-env)
      new-env
      (recur new-env rules))))
