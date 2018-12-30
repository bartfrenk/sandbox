(ns advent.day04
  (:require [clj-time.core :as t]
            [clj-time.format :as f]))

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

