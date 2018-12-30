(ns advent.part2
  (:require [clojure.string :as str]
            [quil.core :as q]
            [quil.middleware :as m]))

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
