(ns clj-pearls.core-bench
  (:require [clj-pearls.core :as sut]
            [criterium.core :refer [quick-bench with-progress-reporting]]))


(defn min-free-bench
  "Runs benchmark of size n of `sut/min-free`."
  [n]
  (let [x (rand-int n)
        xs (shuffle (remove #(= x %) (range 0 n)))]
    (with-progress-reporting (quick-bench (sut/min-free xs) :verbose))))


(defn bench-all
  "Runs all benchmarks."
  []
  (for [n [1e5 1e6 1e7]]
    (min-free-bench (int n))))
