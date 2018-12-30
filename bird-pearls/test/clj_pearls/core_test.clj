(ns clj-pearls.core-test
  (:require [clojure.test :refer :all]
            [clj-pearls.core :as sut]
            [clojure.test.check :refer [quick-check]]
            [clojure.test.check.properties :refer [for-all]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.clojure-test :refer [defspec]]))


(defspec min-free-returns-puncture-in-shuffled-punctured-sequence
  100
  (for-all [n gen/pos-int]
      (for-all [v (gen/choose 0 (dec n))]
          (= v (sut/min-free (shuffle (remove #(= v %) (range 0 n))))))))


(defspec separate-is-equivalent-to-juxt-filter-remove
  100
  (for-all [xs (gen/vector gen/int)
            t gen/int]
      (let [pred (partial < t)]
        (= [(filter pred xs) (remove pred xs)] (sut/separate pred xs)))))


(deftest maximum-surpasser-count-test
  (testing "GENERATING example from the book"
    (is (= 6 (sut/maximum-surpasser-count (map int "GENERATING"))))))


(defn bounded-nat
  [n]
  (gen/fmap #(mod % n) gen/pos-int))


(defn are-inverses-prop
  [invert-fn test-fn]
  (for-all [z (bounded-nat 1000)]
      (let [coll (invert-fn test-fn z)]
        (every? true?
                (for [p coll]
                  (= z (test-fn p)))))))

(defn f3
  [[x y]] (+ (* x x) (* y y) x y))

(defn m
  [[u v]] (* (inc u) (inc v)))

(defspec invert-bounded-returns-inverses-f3-spec
    100
    (are-inverses-prop sut/invert-bounded f3))

(defspec invert-bounded-returns-inverses-m-spec
    100
    (are-inverses-prop sut/invert-bounded m))

(defspec invert-tail-recursive-returns-inverses-f3-spec
    100
    (are-inverses-prop sut/invert-tail-recursive f3))

(defspec invert-tail-recursive-returns-inverses-m-spec
    100
    (are-inverses-prop sut/invert-tail-recursive m))
