(ns advent.part1
  (:require [clj-time.core :as t]
            [clj-time.format :as f]
            [clojure.string :as str]
            [clojure.set :refer [difference union]]))

(defn map-vals
  [f m]
  (into {} (map (fn [[k v]] [k (f v)]) m)))

(defn update-interval
  [coll [start end] f]
  (loop [i start coll* coll]
    (if (< i end)
      (recur (inc i) (update coll* i f))
      coll*)))

(defn parse-entry
  [line]
  (let [[_ time event] (re-matches #"^\[([^\]]*)\] (.+)$" line)]
    [(f/parse (f/formatter "yyyy-MM-dd HH:mm") time)
     (condp re-matches event
       #"falls asleep" :sleep
       #"wakes up" :wake-up
       #"Guard \#(\d+) begins shift" :>> (comp #(Integer/parseInt %) last))]))

(defn read-entries
  [path]
  (with-open [reader (clojure.java.io/reader path)]
    (->> (line-seq reader)
         (map parse-entry)
         (sort-by first)
         (doall))))

(defn group-by-guard
  [entries]
  (->> (partition-by (comp integer? last) entries)
       (partition 2)
       (map (partial apply concat))
       (group-by (comp last first))
       (map-vals (partial apply concat))
       (map-vals #(remove (comp integer? last) %))))

(defn count-minutes
  [alternating]
  (let [minutes (into [] (repeat 60 0))]
    (->> (map (comp t/minute first) alternating)
         (partition 2)
         (reduce (fn [acc interval] (update-interval acc interval inc)) minutes))))

(defn summarize-guard
  [[guard alternating]]
  (let [minutes (count-minutes alternating)]
    {:guard guard
     :minutes minutes
     :sum (apply + minutes)}))

(defn most-sleeping-guard
  [guard-summaries]
  (->> guard-summaries
       (sort-by :sum)
       (last)))

(defn most-frequent-minute
  [guard-summaries]
  (->> guard-summaries
       (sort-by (fn [s] (apply max (:minutes s))))
       (last)))

(defn answer-4a
  [path]
  (let [guard (->> (read-entries path)
                   (group-by-guard)
                   (map summarize-guard)
                   (most-sleeping-guard))
        mx (apply max (:minutes guard))
        [[minute]] (keep-indexed (fn [i x] (when (= x mx) [i x])) (:minutes guard))]
    (* (:guard guard) minute)))

(defn answer-4b
  [path]
  (let [guard (->> (read-entries path)
                   (group-by-guard)
                   (map summarize-guard)
                   (most-frequent-minute))
        mx (apply max (:minutes guard))
        [[minute]] (keep-indexed (fn [i x] (when (= x mx) [i x])) (:minutes guard))]
    (* (:guard guard) minute)))


(answer-4a "res/input-4.txt")
(answer-4b "res/input-4.txt")


(def polymer (str/trim-newline (slurp "res/input-5.txt")))

(defn opposites?
  [u v]
  (and v (= 32 (Math/abs (- (int u) (int v))))))

(defn step
  [units]
  (loop [acc [] [u & us] units]
    (if-not u acc
      (if (opposites? u (first us))
        (recur acc (rest us))
        (recur (conj acc u) us)))))

(defn react
  [units]
  (first (->> (iterate step units)
              (partition 2 1)
              (keep-indexed (fn [i [p q]]
                              (when (= p q)
                                {:index i
                                 :polymer p
                                 :length (count p)
                                 }))))))

(defn get-pairs
  [units]
  (->> (distinct units)
       (group-by #(mod (int %) 32))
       (vals)
       (map (partial into #{}))))

(defn shortest-polymer
  [units]
  (let [pairs (get-pairs units)]
    (->> (map #(remove % units) pairs)
         (map react)
         (sort-by :length))))

;; answer 5a
(react polymer)
;; answer 5b
(shortest-polymer polymer)

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

(def answer-7b (last (fastest-schedule graph 5)))

(take 10 (->> (iterate inc 1)
              (map (partial fastest-schedule graph))
              (map last)))


(def answer-7a
  (->> (read-graph "res/input-7.txt")
       (topological-order)
       (map name)
       (apply str)))

(def graph (read-graph "res/input-7.txt"))
