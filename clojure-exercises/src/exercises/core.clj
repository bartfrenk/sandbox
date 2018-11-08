(ns exercises.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(def trampoline*
  (fn [f & args]
    (loop [f* (apply f args)]
      (if (fn? f*) (recur (f*)) f*))))


(letfn [(triple [x] #(sub-two (* 3 x)))
          (sub-two [x] #(stop?(- x 2)))
          (stop? [x] (if (> x 50) x #(triple x)))]
    (trampoline* triple 2))

(letfn [(my-even? [x] (if (zero? x) true #(my-odd? (dec x))))
        (my-odd? [x] (if (zero? x) false #(my-even? (dec x))))]
  (map (partial trampoline* my-even?) (range 6)))

(def balanced
  (fn [n]
    (letfn [(digits [n]
              (if (zero? n) '()
                  (lazy-seq (cons (mod n 10) (digits (quot n 10))))) )]
      (loop [debt 0
             [x & rest] (vec (digits n))]
        (if (seq? rest)
          (recur (+ debt (- x (last rest))) (pop (vec rest)))
          (= debt 0))))))

(balanced 121)

(def power-set*
  (fn power-set
    ([coll] (power-set coll #{#{}}))
    ([coll acc]
     (if (empty? coll) acc
       (let [x (first coll)]
         (recur
           (disj coll x)
           (->> acc
                (map #(conj % x))
                (concat acc)
                (into #{}))))))))

(def equiv
  (fn [f coll]
    (->> coll
         (reduce (fn [m x] (let [k (f x)]
                             (assoc m k (conj (get m k) x)))) {})
         (vals)
         (map #(into #{} %))
         (into #{}))))


(conj ) (conj nil 1)

(equiv #(* % %) #{-2 -1 0 1 2})


(concat #{1 2} #{3 4})

(power-set* #{1 2})

(empty? x)

(def x #{1 2})

(disj x 1)

(conj x 3)

(concat)
