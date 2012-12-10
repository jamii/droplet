(ns droplet.examples.paxos
  (use clojure.test
       [slingshot.slingshot :only [try+ throw+]]
       droplet))

;; !!! imaginary use-case, doesn't yet compile !!!

;; nodes

(defrecord Proposer [id])
(defrecord Acceptor [id weight])
(defrecord Learner [id])

;; messages

(defrecord Prepare [round])
(defrecord Break [round])
(defrecord Promise [round prev-accept])
(defrecord Accept [round value])
(defrecord Accepted [round value])

;; useful lattices

;; (similar to Causal, perhaps needs to be a builtin)
(extend-protocol SemiLattice
  Promise
  (bottom [this]
    (Promise. -1 nil))
  (lte? [this that]
    (<= (:round this) (:round that)))
  (join [this that]
    (cond
     (< (:round this) (:round that)) this
     (< (:round that) (:round this)) that
     (= this that) this
     :else (throw+ [:conflicting-promises! this that]))))

(defrecord IVar [set? value]
  SemiLattice
  (bottom [this]
    (IVar. false nil))
  (lte? [this that]
    (or (false? set?) (= this that)))
  (join [this that]
    (cond
     (not set?) that
     (= this that) this
     :else (throw+ [:double-assignment this that]))))

(defn ivar
  ([] (IVar. false nil))
  ([value] (IVar. true value)))

;; common

(defn common-states [acceptors learners]
  {:recv #{}
   :send #{}
   :acceptor acceptors
   :learner learners})

(defrule common-rules
  (persistent :recv :send :acceptor :learner))

;; proposers

(def proposer-states
  {:preparing (->Max -1)})

(defn prepare! [round]
  (deduct (is! :preparing (->Max round))
          (in? :acceptor ?acceptor)
          (in! :send [(:id acceptor) (Prepare. round)])))

(def stay-prepared!
  [(deduct (is? :preparing -1)
           (prepare! 0))
   (deduct (is? :preparing (and pos? ?preparing))
           (in? :recv [_ (Break. ?broken)])
           :when (> broken preparing)
           (prepare! (inc broken)))])

(defmacro quorum? [?id]
  `(deduct (in? :acceptor (Acceptor. ~?id ?round-weight))
           (in? :acceptor (Acceptor. (not ~?id) ?other-weight))
           :collect [?round-weights ?round-weight]
           :collect [?other-weights ?other-weight]
           :when (> (sum round-weights) (sum other-weights))))

(defn accept! [round value]
  (deduct (in? :acceptor ?acceptor)
          (in! :send [(:id acceptor) (Prepare. round)])))

(defn accept-promises! [proposer-value]
  (deduct (is? :preparing (Max. ?round))
          (in? :recv [?id (Promise. ?round ?prev-accept)])
          (quorom? ?id)
          :collect [prev-accepts prev-accept]
          :let [value (or (:value (max-by :round prev-accepts)) proposer-value)]
          (accept! round value)))

(defn proposer-rules [proposer-value]
  [(persistent :preparing)
   stay-prepared!
   (accept-promises! proposer-value)])

;; acceptors

(def acceptor-states
  {:promised (Promised. -1 nil)})

(def make-promises!
  (deduct (in? :recv [?id (Prepare. ?prepare-round)])
          (is? :promised (Promise. ?promise-round ?prev-accept))
          (if (> prepare-round promise-round)
            (do :let [promise (Promise. prepare-round prev-accept)]
                (is! :promised promise)
                (in! :send [id promise]))
            (in! :send [id (Break. prepare-round)]))))

(def keep-promises!
  (deduct (in? :recv [?id (Accept. ?round ?value)])
          (is? :promised (Promise. ?round _))
          :let [accepted (Accepted. round value)]
          (is! :promised (Promise. round accepted))
          (in! :send [id accepted])
          (in? :learner ?learner)
          (in! :send [(:id learner) accepted])))

(def acceptor-rules
  [(persistent :promised)
   make-promises!
   keep-promises!])

;; learners

(def learner-states
  {:learned (ivar)})

(def learn!
  (deduct (in? :recv [?id (Accepted. ?round ?value)])
          (quorom? ?id)
          (is! :learned (ivar value))))

(def learner-rules
  [(persistent :learned)
   learn!])
