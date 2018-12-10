(ns advent.part2
  (:require [clojure.string :as str])
  (:import [java.util Deque LinkedList]))

(defn read-tokens
  [path]

  (->> (slurp path)
       (str/trim-newline)
       (#(str/split % #" "))
       (map #(Integer/parseInt %))))

(defn parse-node
  [[n m & more]]
  (loop [stream more
         children []]
    (if (< (count children) n)
      (let [[child stream*] (parse-node stream)]
        (recur stream* (conj children child)))
      [{:children children :meta (take m stream)} (drop m stream)])))

(defn reduce-tree
  ([f node] (reduce-tree f identity node))
  ([f g {:keys [children meta]}]
   (if (seq children)
     (f (map (partial reduce-tree f g) children) meta)
     (g meta))))

(def answer-8a (->> (read-tokens "res/input-8.txt")
                    (parse-node)
                    (first)
                    (reduce-tree (fn [xss meta]
                                   (apply concat meta xss)))
                    (apply +)))

(def answer-8b (->> (read-tokens "res/input-8.txt")
                    (parse-node)
                    (first)
                    (reduce-tree (fn [xss meta]
                                   (let [v (into [] xss)]
                                     (->>  (map dec meta)
                                           (map (partial get v))
                                           (apply concat)))))
                    (apply +)))

(identity [answer-8a answer-8b])

(defn deque
  [xs]
  (LinkedList. xs))

(defn shift-left
  [k deque]
  (loop [k* k]
    (if (pos? k*)
      (do
        (.add deque (.removeFirst deque))
        (recur (dec k*)))
      deque)))

(defn shift-right
  [k deque]
  (loop [k* k]
    (if (pos? k*)
      (do
        (.push deque (.removeLast deque))
        (recur (dec k*)))
      deque)))

(defn insert-marble
  [marbles next]
  (if (zero? (mod next 23))
    (let [x (.removeLast (shift-right 7 marbles))]
      (do
        (shift-left 1 marbles)
        (+ x next)))
    (do
      (.add (shift-left 1 marbles) next)
      0)))

(defn play-game
  ([players rounds]
   (let [marbles (deque [0])]
     (loop [i 1 scores {}]
       (if (<= i rounds)
         (let [points (insert-marble marbles i)
               scores* (update scores (mod i players) (fnil + 0) points)]
           (recur (inc i) scores*))
         scores)))))

(def answer-9b (apply max (vals (play-game 430 7158800))))
(def answer-9a (apply max (vals (play-game 430 71588))))

(time (apply max (vals (play-game 430 7158800))))


;; (identity answer-9a)

;; (apply max (vals (play-game 30 5807)))
