(ns advent.queue)

(defrecord Dequeue [front rear])

(defn dq-empty?
  [{:keys [front rear front-n rear-n]}]
  (and (empty? front) (empty? rear)))

(defn to-dequeue
  [front rear front-n rear-n]
  (cond

    (empty? rear)
    (let [front-n* (quot front-n 2)
          rear-n* (- front-n front-n*)
          [front* rear*] (split-at front-n* front)]
      (Dequeue. front* (reverse rear*) front-n* rear-n*))

    (empty? front)
    (let [front-n* (quot rear 2)
          rear-n* (- rear-n front-n*)
          [front* rear*] (split-at rear-n* rear)]
      (Dequeue. (reverse front*) rear* front-n* rear-n*))))

(defn dq-append
  [{:keys [front rear front-n rear-n]} x]
  (Dequeue. front (conj rear x) front-n (inc rear-n)))

(dq-)






(defn head
  []
  )
