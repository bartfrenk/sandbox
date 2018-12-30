(ns advent.day08
  (:require [clojure.string :as str]))


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


