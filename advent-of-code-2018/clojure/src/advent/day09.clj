(ns advent.day09
  (:import [java.util Deque LinkedList]))

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

