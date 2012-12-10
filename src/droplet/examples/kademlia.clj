(ns droplet.examples.kademlia
  (use clojure.test
       [slingshot.slingshot :only [try+ throw+]]
       droplet))

;; !!! imaginary use-case, doesn't yet compile !!!

;; assumes {} has bottom as default value
;; need to distinguish data from views
;; views can be recreated when data decreases
;; gc would be easier with e-a-v-t, just remove entity
;; don't like how the rate limiting relies on internals

(defrecord Peer [ip port])

;; messages

(defrecord Seen [peer])
(defrecord Seen? [end])

;; state

(defstate $heard peer time)
(defstate $saw peer time)

(defstate $time time)

(defstate $dialed bucket time)

(defin $recv)
(defout $send)

;; rules

(defquery peers
  (or (in? $seen [?peer _])
      (in? $heard [?peer _])))

(defnquery neighbours [n end]
  (with (in? peers ?peer)
        #(top-by n (partial closeness end) %)))

(defquery buckets
  (with (in? peers ?peer)
        #(group-by bucket %)))

(defrule tick
  (join (in? $recv Tick)
        (is? $time ?time)
        (next! is! $time (inc time))))

;; pings are rate-limited to 1 / peer / tick

(defrule ping
  (join (in? $ping ?peer)
        (in! $send [peer (Seen?. (random-end))])))

(defrule forget-pings
  (join (in? $recv Tick)
        (in? $ping ?peer)
        (next! not-in! $ping peer)))

(defrule hear
  (join (in? $recv [_ (Seen. ?peer)])
        (is? $time ?time)
        (in! $heard [peer (interval time)])))

(defrule look
  (join (in? $recv [_ (Seen. ?peer)])
        (not (in? $seen [?peer _]))
        (in! $ping peer)))

(defrule see
  (join (in? $recv [?peer _])
        (is? $time ?time)
        (in! $seen [peer (interval time)])))

(defrule follow
  (join (is? $time ?time-now)
        (in? $seen [?peer ?time-seen])
        (when (< time-seen time-now))
        (in! $ping peer)))

(defrule gossip
  (join (in? $recv [?peer (Seen?. ?end)])
        (in? (neighbours end) ?neighbour)
        (in! $send [?peer (Seen. neighbour)])))

;; gc peers so we don't run out of space
(defrule forget-peers
  (join (in? $recv GC)
        (is? $time ?time-now)
        (in? buckets ?bucket)
        :let [old-peers (drop min-peers-per-bucket (sort-by liveness bucket))]
        (next! on! $seen disj old-peers)
        (next! on! $heard disj old-peers)))
