(ns advent.part2
  (:require [clojure.string :as str]
            [quil.core :as q]
            [quil.middleware :as m]
            )
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


(defn parse-star
  [line]
  (let [vector #"< *(-?\d+),  *(-?\d+)>"
        pattern (re-pattern (str "position=" vector " velocity=" vector))]
    (->> (re-matches pattern line)
         (rest)
         (map str/trim)
         (map #(Integer/parseInt %))
         (partition 2))))

(defn read-stars
  [path]
  (with-open [reader (clojure.java.io/reader path)]
    (->> (line-seq reader)
         (map parse-star)
         (doall))))

(def all-stars (read-stars "res/input-10.txt"))

(defn step-star
  [[[x y] [u v]]]
  [[(+ x u) (+ y v)] [u v]])

(defn step-all-stars
  [all-stars]
  (doall (map step-star all-stars)))

(defn find-local-minimum
  [f step init]
  (loop [previous (f init)
         state init
         i 0]
    (let [state* (step state)
          current (f state*)]
      (if (< previous current) [i state] (recur current state* (inc i))))))

(defn star-height
  [stars]
  (let [hs (map (fn [[[r c] _]] r) stars)]
    (- (apply max hs) (apply min hs))))

(defn draw-fixed-stars
  [stars]
  (fn []
    (q/background 0)
    (q/fill 255 255 0 50)
    (q/with-translation [-200 0]
      (doseq [[[r c] _] stars]
        (let [x (* 4 r)
              y (* 4 c)]
          (q/ellipse x y 10 10))))))

;; answer-10a
(q/defsketch fixed-constellation
  :features [:no-bind-output]
  :size [1000 1000]
  :middleware [m/pause-on-error]
  :draw (->> all-stars
             (find-local-minimum star-height step-all-stars)
             (last)
             (draw-fixed-stars)))

;; answer-10b
(def answer-10b
  (first (find-local-minimum star-height step-all-stars all-stars)))

(defn draw-moving-stars
  [init-state]
  (let [state (atom init-state)]
    (fn []
      (q/background 0)
      (q/fill 255 255 50)
      (q/with-translation [-200 0]
        (doseq [[[r c] _] @state]
          (q/ellipse (* 3 r) (* 3 c) 5 5)))
      (swap! state step-all-stars))))

(defn setup-moving-stars []
  (q/frame-rate 1))

(def init-state (last (find-local-minimum star-height step-all-stars all-stars)))

(q/defsketch moving-constellation
  :features [:no-bind-output]
  :size [1000 1000]
  :setup setup-moving-stars
  :middleware [m/pause-on-error]
  :draw (draw-moving-stars init-state))
