(ns advent.day07
  (:require [clojure.set :refer [difference union]]))

(defn map-vals
  [f m]
  (into {} (map (fn [[k v]] [k (f v)]) m)))

(defn parse-edge
  [line]
  (let [p #"^Step ([A-Z]) must be finished before step ([A-Z]) can begin.$"
        [_ src dest] (re-matches p line)]
    [(keyword src) (keyword dest)]))

(defn read-graph
  [path]
  (with-open [reader (clojure.java.io/reader path)]
    (let [edges (->> (line-seq reader)
                     (map parse-edge))]
      (reduce (fn [g [src dest]]
                (update g src (fnil conj []) dest))
              {}
              edges))))

(defn min-kw
  [x y]
  (if (<= (compare x y) 0) x y))

(defn topological-order
  "Kahn's algorithm."
  [graph]
  (let [sources (difference
                  (into #{} (keys graph))
                  (into #{} (apply concat (vals graph))))]
    (loop [acc [] seeds sources remaining graph]
      (if (seq seeds)
        (let [next (reduce min-kw seeds)
              new-remaining (dissoc remaining next)
              new-seeds (difference
                          (into #{} (get remaining next))
                          (into #{} (apply concat (vals new-remaining))))]
          (recur
            (conj acc next)
            (union (disj seeds next) new-seeds)
            new-remaining))
        acc))))

(defn duration
  [task]
  (- (int (get (name task) 0)) 4))

(defn min-val
  [m]
  (reduce (fn [[k v] [k* v*]]
            (cond
              (< v v*) [k v]
              (> v v*) [k* v*]
              :else [(min-kw k k*) v]))
          m))

(defn fastest-schedule
  [graph max-workers]
  (let [sources (difference
                  (into #{} (keys graph))
                  (into #{} (apply concat (vals graph))))]
    (loop [acc []
           elapsed 0
           seeds sources
           remaining graph
           in-progress {}]
      (cond
        (not (or (seq seeds) (seq in-progress))) [acc elapsed]

        (and (seq seeds) (< (count in-progress) max-workers))
        (let [next (reduce min-kw seeds)]
          (recur
            acc
            elapsed
            (disj seeds next)
            remaining
            (assoc in-progress next (duration next))))

        :otherwise
        (let [[finished dt] (min-val in-progress)
              new-remaining (dissoc remaining finished)
              new-seeds (difference
                          (into #{} (get remaining finished))
                          (into #{} (apply concat (vals new-remaining))))]
          (recur
            (conj acc finished)
            (+ elapsed dt)
            (union seeds new-seeds)
            new-remaining
            (map-vals #(- % dt) (dissoc in-progress finished))))))))

(def graph (read-graph "res/input-7.txt"))

(def answer-7b (last (fastest-schedule graph 5)))

(take 10 (->> (iterate inc 1)
              (map (partial fastest-schedule graph))
              (map last)))

(def answer-7a
  (->> (read-graph "res/input-7.txt")
       (topological-order)
       (map name)
       (apply str)))


