(ns clj-pearls.core
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]))

(defn separate
  "Single pass implementation of the juxtaposition of filter and remove, i.e., returns
  [(filter pred xs) (remove pred xs)]."
  ([pred xs] (separate pred xs [[] []]))
  ([pred [x & xs] [us vs]]
   (cond
     (nil? x) [us vs]
     (pred x) (recur pred xs [(conj us x) vs])
     :else (recur pred xs [us (conj vs x)]))))


(defn min-free
  "Returns the smallest number at least `a` that is not in `xs`. Assumes `xs` does
  not contain any duplicates. From Section 1 of the Pearls of Functional
  Algorithm Design."
  ([xs] (min-free 0 xs))
  ([a xs] (min-free a (count xs) xs))
  ([a n xs]
   (let [b (+ a 1 (quot n 2))
         [us vs] (separate (partial > b) xs)
         m (count us)]
     (cond
       (zero? n) a
       (= m (- b a)) (recur b (- n m) vs)
       :else (recur a m us)))))


(defn join
  "Merges two surpasser count tables."
  ([txs tys] (join (count tys) txs tys))
  ([n txs tys]
   (cond
     (zero? n) txs
     (empty? txs) tys
     :else
     (let [[[x c] & txs*] txs
           [[y d] & tys*] tys]
       (if (< x y)
         (cons [x (+ c n)] (join n txs* tys))
         (cons [y d] (join (dec n) txs tys*)))))))

(defn table
  "Returns list of pairs of elements and surpasser counts for those elements,
  ordered by the elements. Such an ordered list is called a surpasser count
  table."
  [xs]
  {:pre [(not (empty? xs))]}
  (if (= (count xs) 1)
    (list [(first xs) 0])
    (let [m (count xs)
          n (quot m 2)
          [ys zs] (split-at n xs)]
      (join (- m n) (table ys) (table zs)))))

(defn maximum-surpasser-count
  "Returns the maximum surpasser count of the numerical list `xs`. The surpasser
  count of an element of a list are the number of greater elements to the right
  of it."
  [xs]
  {:pre [(not (empty? xs))]}
  (apply max (map second (table xs))))


(defn invert
  "Finds all pairs of natural numbers that are mapped to `z` by `g`, under the
  assumption that `g` is strictly increasing in both arguments."
  ([f z] (invert [0 z] f z))
  ([[u v] f z]
   (let [z* (f [u v])]
     (cond
       (or (> u z) (neg? v)) ()
       (< z* z) (invert [(inc u) v] f z)
       (= z* z) (cons [u v] (invert [(inc u) (dec v)] f z))
       (> z* z) (invert [u (dec v)] f z)))))

(defn bsearch
  "Finds a natural number m in the interval [a, b) such that z is in the
  interval [g m, g (m + 1)), in time logarithmic in the length of the interval."
  [g [a b] z]
  (let [m (quot (+ a b) 2)]
    (cond
      (= (inc a) b) a
      (<= (g m) z) (recur g [m b] z)
      :else (recur g [a m] z))))

(defn invert-tail-recursive
  "Tail-recursive variant of invert."
  ([f z] (invert-tail-recursive () [0 z] f z))
  ([acc [u v] f z]
   (let [z* (f [u v])]
     (cond
       (or (> u z) (neg? v)) acc
       (< z* z) (recur acc [(inc u) v] f z)
       (= z* z) (recur (cons [u v] acc) [(inc u) (dec v)] f z)
       (> z* z) (recur acc [u (dec v)] f z)))))

(defn invert-bounded
  "Variant of `invert` that bounds the search space."
  ([f z]
   (let [m (bsearch (fn [y] (if (neg? y) (f [0 0]) (f [0 y]))) [-1 (inc z)] z)
         n (bsearch (fn [x] (if (neg? x) (f [0 0]) (f [x 0]))) [-1 (inc z)] z)]
     (loop [acc ()
            [u v] [0 m]]
       (let [z* (f [u v])]
         (cond
           (or (> u n) (neg? v)) acc
           (< z* z) (recur acc [(inc u) v])
           (= z* z) (recur (cons [u v] acc) [(inc u) (dec v)])
           (> z* z) (recur acc [u (dec v)])))))))


(defn merge-sorted*
  "Lazy recursive implemention of merging two sorted sequences."
  [[x & xs :as xss] [y & ys :as yss]]
  (cond
    (nil? x) yss
    (nil? y) xss
    (<= x y) (lazy-seq (cons x (merge-sorted* xs yss)))
    :else (lazy-seq (cons y (merge-sorted* xss ys)))))

(defn merge-sorted**
  "Non-lazy recursive implemention of merging two sorted sequences. Prone to
  overflowing the stack."
  [[x & xs :as xss] [y & ys :as yss]]
  (cond
    (nil? x) yss
    (nil? y) xss
    (<= x y) (cons x (merge-sorted** xs yss))
    :else (cons y (merge-sorted** xss ys))))

(defn merge-sorted
  "Tail recursive implemention of merging two sorted sequences."
  ([us vs] (merge-sorted us vs []))
  ([[x & xs :as us] [y & ys :as vs] acc]
   (cond
     (nil? x) (concat acc vs)
     (nil? y) (concat acc us)
     (<= x y) (recur xs vs (conj acc x))
     :else (recur us ys (conj acc y)))))


(defn smallest
  "Finds the k-th smallest element in the sorted union of sequences `zs` and
  `ws`. Assumes `zs` and `ws` are sorted in ascending order."
  [k zs ws]
  (cond
    (empty? zs) (nth ws k)
    (empty? ws) (nth zs k)
    :else
    (let [p (quot (count zs) 2)
          q (quot (count ws) 2)
          [xs [a & ys]] (split-at p zs)
          [us [b & vs]] (split-at q ws)]
      (case [(< a b) (<= k (+ p q))]
        [true true] (recur k zs us)
        [true false] (recur (- k p 1) ys ws)
        [false true] (recur k xs ws)
        [false false] (recur (- k q 1) zs vs)))))

(defn smallest*
  "Finds the k-th smallest element in the sorted union of sequences `zs` and
  `ws`. Assumes `zs` and `ws` are sorted in ascending order. Faster than
  `smallest` for sequences that allow for constant time indexing. Fixes indexing
  mistakes in Bird's version from the book."
  [k xa ya]
  (letfn [(search
            [k lx rx ly ry]
            (cond
              (== lx rx) (nth ya (+ k ly))
              (== ly ry) (nth xa (+ k lx))
              :else
              (let [mx (quot (+ lx rx) 2)
                    my (quot (+ ly ry) 2)]
                (case [(< (nth xa mx) (nth ya my)) (<= k (+ (- mx lx) (- my ly)))]
                  [true true] (recur k lx rx ly my)
                  [true false] (recur (- k (- mx lx) 1) (inc mx) rx ly ry)
                  [false true] (recur k lx mx ly ry)
                  [false false] (recur (- k (- my ly) 1) lx rx (inc my) ry)))))]
    (let [m (count xa)
          n (count ya)]
      (search k 0 m 0 n))))


(s/def ::digit
  (s/and int? #(and (<= 0 %) (< % 10))))

(s/def ::factor
  (s/coll-of ::digit :kind vector?))

(s/def ::term
  (s/coll-of ::factor :kind vector?))

(s/def ::expr
  (s/coll-of ::term :kind vector?))

(defn- val-factor
  [factor]
  (reduce (fn [acc d] (+ (* 10 acc) d)) 0 factor))

(defn- val-term
  [term]
  (apply * (map val-factor term)))

(defn val-expr
  "Evaluates expression `expr`. An expression is represented as a triply nested
  sequence, e.g. something of the form (((1) (2)) ((3 4))), in which the
  innermost list represents a single number, the next level up represents a
  product of multiple numbers, and the highest level represents a sum of
  products. Thus, the previous example stands for the expression 1*2+34"
  [expr]
  (apply + (map val-term expr)))

(defn- show-factor
  [factor]
  (apply str factor))

(defn- show-term
  [term]
  (str/join "*" (map show-factor term)))

(defn show-expr
  "Returns a string representation of expression `expr`."
  [expr]
  (str/join "+" (map show-term expr)))

(defn century?
  [n]
  (= n 100))

(defn splits
  "Computes a list of all splits of `xs`"
  [xs]
  (letfn [(split-at* [n zs]
            (if (zero? n)
              [zs]
              (split-at n zs)))]
    (let [n (count xs)]
      (map #(split-at % xs) (range 1 (inc n))))))

(defn partitions
  "Returns ordered partitions of a list. The number of such partitions is
  exponential in the length of the list. This makes this function infeasibly
  slow for long lists. A lazy implementation would be much better."
  [xs]
  (condp = (count xs)
    0 [[]]
    1 [[xs]]
    (mapcat (fn [[p q]] (map #(cons p %) (partitions q))) (splits xs))))


(defn expressions
  "Returns the expressions that can be built from a sequence of digits by
  interleaving juxtaposition, times and sum operations in the
  sequence. Implementation is non-lazy, and hence evaluation takes exponential
  time in the number of digits."
  [digits]
  (mapcat partitions (partitions digits)))

(defn good-exprs
  "Returns the string representation of all expressions in digits `digits` that
  satisfy `good-pred`. Takes exponential time in the number of digits, even to
  generate just a single good expression (i.e. one satisfying `good-pred`)."
  [digits good-pred]
  (->> (expressions digits)
       (map (fn [expr] [(val-expr expr) expr]))
       (filter (fn [[v _]] (good-pred v)))
       (map second)
       (map show-expr)))


(def digits [1 2 3 4 5 6 7 8 9])

(count (expressions digits))

(clojure.pprint/pprint (good-exprs [1 2 3 4 5 6 7 8 9] #(= 100 %)))


