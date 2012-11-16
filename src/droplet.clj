(ns droplet
  (use clojure.test)
  (require clojure.set
           strucjure
           strucjure.parser
           strucjure.pattern
           strucjure.util))

;; overview:
;;   states :: {symbol lattice-element}

;; correctness/lint:
;;   should have exactly one inductive sink per state

;; safety:

;; confluence/monotonicity:

;; TODO
;; switch to record/protocol with no inner state
;; put rules in states?
;; make bottom/1 part of lattice interface?

;; --- LATTICES ---

(defprotocol SemiLattice
  (lte? [this elem-a elemb] "Less-than, the lattice relation")
  (join [this elem-a elem-b] "Find the least upper bound of two lattice elements"))

(defmacro defjoin [name args join-form lt-form]
  (let [elem-a (gensym "elem-a")
        elem-b (gensym "elem-b")]
    `(defrecord ~name ~args
       ~'SemiLattice
       (~'lte? [~'_ ~elem-a ~elem-b]
         ~(if (nil? lt-form)
            `(= ~elem-b (~join-form ~@args ~elem-a ~elem-b))
            `(~lt-form ~@args ~elem-a ~elem-b)))
       (~'join [~'_ ~elem-a ~elem-b]
         (~join-form ~@args ~elem-a ~elem-b)))))

(defn lt? [lattice elem-a elem-b]
  (and (not= elem-a elem-b)
       (lte? lattice elem-a elem-b)))

(defjoin Or [] or nil)
(defjoin And [] and nil)
(defjoin Max [] max <=)
(defjoin Min [] min >=)
(defjoin Set [] clojure.set/union clojure.set/subset?)

(defn join-map [val-lattice map-a map-b]
  (merge-with (partial join val-lattice) map-a map-b))

(defn lte-map? [val-lattice map-a map-b]
  (every? (fn [[key val-a]]
            (and (contains? map-b key)
                 (lte? val-lattice val-a (get map-b key))))
          map-a))

(defjoin Map [val-lattice] join-map lte-map?)

(defn join-pair [fst-lattice snd-lattice [fst-a snd-a :as elem-a] [fst-b snd-b :as elem-b]]
  (cond
   (lt? fst-lattice fst-a fst-b) elem-b
   (lt? fst-lattice fst-b fst-a) elem-a
   :else [(join fst-lattice fst-a fst-b)
          (join snd-lattice snd-a snd-b)]))

(defn lte-pair?  [fst-lattice snd-lattice [fst-a snd-a :as elem-a] [fst-b snd-b :as elem-b]]
  (or (lte? fst-lattice fst-a fst-b)
      (and (= fst-a fst-b)
           (lte? snd-lattice snd-a snd-b))))

(defjoin Pair [fst-lattice snd-lattice] join-pair lte-pair?)

;; --- STATES ---

;; states is {symbol {:lattice lattice :element element}}

;; --- RULES ---

(defrecord Rule [action sink sources fun]) ; action is either :deduct or :induct

;; --- DEDUCTIVE ---

(defn deduct [sink sources fun]
  (->Rule :deduct sink sources fun))

(defn deductions [states deductive-rule]
  (assert (= :deduct (:action deductive-rule)))
  (let [elements (map #(:element (get states %)) (:sources deductive-rule))
        new-element (apply (:fun deductive-rule) elements)]
    (let [sink (:sink deductive-rule)]
      (let [{:keys [lattice element]} (get states sink)]
        (assoc-in states [sink :element] (join lattice element new-element))))))

(defn deductive-step [states deductive-rules]
  (reduce deductions states deductive-rules))

(defn fixpoint
  ([states deductive-rules]
     (let [new-states (deductive-step states deductive-rules)]
       (if (= states new-states)
         new-states
         (recur new-states deductive-rules))))
  ([states deductive-rules & deductive-rules-strata]
     (reduce fixpoint states (cons deductive-rules deductive-rules-strata))))

;; --- INDUCTIVE ---

(defn induct [sink sources fun]
  (->Rule :induct sink sources fun))

(defn inductions [states new-states inductive-rule]
  (assert (= :induct (:action inductive-rule)))
  (let [elements (map #(:element (get states %)) (:sources inductive-rule))
        new-element (apply (:fun inductive-rule) elements)]
    (let [sink (:sink inductive-rule)]
      (let [{:keys [lattice element]} (get states sink)]
        (assoc new-states sink {:lattice lattice
                                :element (join lattice element new-element)})))))

(defn inductive-step [states deductive-rules-strata inductive-rules]
  (let [fixed-states (apply fixpoint states deductive-rules-strata)]
    (reduce (partial inductions fixed-states) {} inductive-rules)))

(defn quiscience [states deductive-rules-strata inductive-rules]
  (let [new-states (inductive-step states deductive-rules-strata inductive-rules)]
    (if (= states new-states)
      new-states
      (recur new-states deductive-rules-strata inductive-rules))))

;; --- COMMON RULES ---

(defn persistent [sym]
  (induct sym [sym] identity))

(defn ephemeral [sym empty]
  (induct sym [sym] (constantly empty)))

;; --- TOP LEVEL ---

(defrecord Droplet [states deductive-rules-strata inductive-rules])

(defn reactive [states & rules]
  (let [rules (flatten rules)
        deductive-rules (filter #(= :deduct (:action %)) rules)
        inductive-rules (filter #(= :induct (:action %)) rules)]
    (agent (->Droplet states [deductive-rules] inductive-rules))))

(defn insert [droplet symbol value]
  (-> droplet
      (update-in [:states symbol :element] conj value)
      (update-in [:states] quiscience (:deductive-rules-strata droplet) (:inductive-rules droplet))))

(defn insert! [reactive symbol value]
  (send reactive insert symbol value))

;; --- TESTS ---

;; deductive tests

(def path-states
  {'edge {:lattice (->Set) :element #{[1 2] [2 3] [3 3] [4 5] [5 4]}}
   'path {:lattice (->Set) :element #{}}})

(def path-rules
  [(deduct 'path ['edge] identity)
   (deduct 'path ['path 'edge]
           (fn [path edge]
             (set (for [[a b] path
                        [b' c] edge
                        :when (= b b')]
                    [a c]))))])

(deftest path-test
  (is (= #{[1 2] [1 3] [2 3] [3 3] [4 4] [5 4] [4 5] [5 5]}
         (get-in (fixpoint path-states path-rules) ['path :element]))))

;; inductive tests

(def growth-states path-states)

(def deductive-growth-rules
  [(deduct 'path ['edge]
           (fn [edge]
             (set (for [[a b] edge
                        [b' c] edge
                        :when (= b b')]
                    [a c]))))])

(def inductive-growth-rules
  [(ephemeral 'path #{})
   (induct 'edge ['path] identity)])

(deftest growth-test
  (is (= #{[1 2] [1 3] [2 3] [3 3] [4 4] [5 4] [4 5] [5 5]}
         (get-in (quiscience growth-states [deductive-growth-rules] inductive-growth-rules) ['edge :element]))))

;; nosql tests

(def vc-lattice
  (->Map (->Max)))

(def nosql-lattice
  (->Map (->Pair vc-lattice (->Max))))

(def nosql-states
  {'puts {:lattice (->Set) :element #{}}
   'store {:lattice nosql-lattice :element {}}})

(def nosql-rules
  [(ephemeral 'puts #{})
   (persistent 'store)
   (deduct 'store ['puts]
           (fn [puts]
             (into {}
                   (for [{:keys [key vc val]} puts]
                     [key [vc val]]))))])

(defn new-nosql []
  (reactive nosql-states nosql-rules))

(defn put! [nosql key vc val]
  (insert! nosql 'puts {:key key :vc vc :val val}))

(defn is! [nosql key vc val]
  (if (await-for 1000 nosql)
    (is (= [vc val] (get-in @nosql [:states 'store :element key])))
    (throw (agent-error nosql))))

(deftest nosql-serial-test
  (doto (new-nosql)
    (put! :x {:alice 1} 10)
    (is! :x {:alice 1} 10)
    (put! :y {:alice 2} 20)
    (is! :y {:alice 2} 20)
    (put! :x {:alice 3} 5)
    (is! :x {:alice 3} 5)))

(deftest nosql-merge-test
  (doto (new-nosql)
    (put! :x {:alice 1} 10)
    (put! :x {:alice 1 :bob 1} 20)
    (put! :x {:alice 1 :charlie 1} 15)
    ;; no causal relation - max value wins
    (is! :x {:alice 1 :bob 1} 20)))

(deftest nosql-causal-test
  (doto (new-nosql)
    (put! :x {:alice 1} 10)
    (put! :x {:alice 1 :bob 1} 20)
    (put! :x {:alice 1 :bob 1 :charlie 1} 15)
    ;; causally ordered - last write wins
    (is! :x {:alice 1 :bob 1 :charlie 1} 15)))
