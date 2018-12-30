(ns advent.day05
  (:require [clj-time.format :as f]
            [clojure.string :as str]
            [clojure.set :refer [difference union]]))

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

