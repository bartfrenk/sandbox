(ns try.anglican
  (:require [anglican.emit :refer [defquery]]
            [anglican.runtime :refer [beta flip sample*]]))


(defn -main
  [& args]
  (println "Hello, World!"))

(defquery example
  (let [bet (sample (beta 5 3))]
    (observe (flip bet) true)
    (predict (> bet 0.7))))



(sample (beta 5 3))
