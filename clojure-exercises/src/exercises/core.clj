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



;; Problem 105
(def collect
  (fn [xs]
    (letfn [(update* [m k f & more]
              (assoc m k (apply f (get m k) more)))
            (collect* [[m active] x]
              (if (keyword? x)
                [(assoc m x []) x]
                [(update* m active conj x) active]))]
      (first (reduce collect* [{} nil] xs)))))

(def collect-2
  #(->> (partition-by keyword? %)
        (partition 2)
        (reduce (fn [agg [k v]]
                  (-> (zipmap (reverse k) (cons v (repeat ())))
                      (into agg))) {})))

(def collect-3
  #(->> (partition-by keyword? %)
        (partition 2)
        (mapcat (fn [[kws xs]]
               (->> (butlast kws)
                    (map (fn [kw] [kw []]))
                    ((fn [ys]
                       (conj ys [(last kws) (vec xs)]))))))
        (into {})))

(def t (flatten (mapcat identity {:a (range 100000) :b (range 100000)})))
(time (doall (collect t))) ;; around 135 msecs
(time (doall (collect-2 t))) ;; around 60 msecs
(time (doall (collect-3 t))) ;; around 70 msecs
(collect-3  [:a 1 2 3 :b :c 4])

;; Problem 137
(def digits
  (fn [n base]
    (loop [acc [] m n]
      (cond
        (zero? m) (if (empty? acc) [0] (reverse acc))
        :else (recur (conj acc (mod m base)) (quot m base))))))

(digits 1234501 10)
(digits 0 11)
(digits 9 2)
(time (digits Integer/MAX_VALUE 42))

;; Problem 144
(def oscilrate
  (fn oscilrate* [val & fns]
    (letfn [(it
              ([val] (list val))
              ([val f & fns]
               (lazy-seq (cons val (apply oscilrate* (f val) fns)))))]
      (apply it val (cycle fns)))))

(take 10 (oscilrate 3.14 int double))
(take 5 (oscilrate 3 #(- % 3) #(+ 5 %)))

(take 10 (cycle [1 2 3]))


;; Problem 110
(def pronunciation
  (fn [zs]
    (letfn [(pronounce [xs]
              (->> (partition-by identity xs)
                   (mapcat (fn [xss] [(count xss) (first xss)]))))]
      (iterate pronounce (pronounce zs)))))


(def uncurry
  (fn [g]
    (letfn [(step [f xs]
              (if (fn? f)
                (recur (f (first xs)) (rest xs))
                f))]
      (fn [& args]
        (step g args)))))


(uncurry (fn [a]
           (fn [b]
             (fn [c]
               (fn [d]
                 (+ a b c d)))))
         [1 2 3 4])


(take 3 (pronunciation [1]))

(partition-by identity [1 1 2])
