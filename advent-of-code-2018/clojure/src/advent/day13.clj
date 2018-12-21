(ns advent.day13
  (:require [clojure.string :as str]))

(defrecord Tracks [chars width])

(defn next-direction
  [current-direction]
  (current-direction {:left :straight :straight :right :right :left}))

(def cart-symbols #{\<, \>, \^, \v})

(defn to-matrix-coords
  [{width :width} line-coords]
  [(quot line-coords width) (mod line-coords width)])

(defn to-line-coords
  [{width :width} [r c]]
  (+ (* width r) c))

(defn read-input [path]
  (let [lines (-> (slurp path)
                  (str/split #"\n"))
        [width] (distinct (map count lines))
        contents (apply str lines)
        carts (->> (keep-indexed #(when (cart-symbols %2) [%1 %2]) contents)
                   (map (fn [[line-coords sym]]
                          [(to-matrix-coords {:width width} line-coords) [:left sym]])))]
    {:tracks (Tracks. (into [] (map #(if (cart-symbols %) \* %) contents)) width)
     :carts carts}))



(defn track-at-matrix-coords
  [{chars :chars :as tracks} matrix-coords]
  (get chars (to-line-coords tracks matrix-coords)))

(defn moves-by-cart-sym
  [cart-sym]
  (case cart-sym
    \> [0 1]
    \< [0 -1]
    \^ [-1 0]
    \v [1 0]))

(defn update-cart-symbol
  [covers sym]
  (cond
    (and (= \\ covers) (= \^ sym)) \<
    (and (= \\ covers) (= \> sym)) \v
    (and (= \\ covers) (= \v sym)) \>
    (and (= \\ covers) (= \< sym)) \^
    (and (= \/ covers) (= \^ sym)) \>
    (and (= \/ covers) (= \> sym)) \^
    (and (= \/ covers) (= \v sym)) \<
    (and (= \/ covers) (= \< sym)) \v
    (nil? covers) (throw (Exception. "Should not happen"))
    :else sym))

(defn move-on-crossroads
  [cart-sym next-dir]
  (let [new-sym (case [cart-sym next-dir]
                  [\^ :left] \<
                  [\^ :right] \>
                  [\> :left] \^
                  [\> :right] \v
                  [\v :left] \>
                  [\v :right] \<
                  [\< :left] \v
                  [\< :right] \^
                  cart-sym)]
    [(moves-by-cart-sym new-sym) new-sym]))

(defn move-cart
  [track carts [[r c] [next-dir sym]]]
  (let [covers (track-at-matrix-coords track [r c])]
    (cond
      (not= \+ covers)
      (let [new-matrix-coords (map + [r c] (moves-by-cart-sym sym))
            new-covers (track-at-matrix-coords track new-matrix-coords)
            new-sym (update-cart-symbol new-covers sym)]
        [new-matrix-coords [next-dir new-sym]])

      (= \+ covers)
      (let [[move new-sym] (move-on-crossroads sym next-dir)
            new-matrix-coords (map + [r c] move)
            new-covers (track-at-matrix-coords track new-matrix-coords)]
        [new-matrix-coords [(next-direction next-dir) new-sym]]))))

(defn crash?
  [[pos _] & more]
  (->> (apply concat more)
       (filter (fn [[pos* _]] (= pos pos*)))
       (seq)))

(defn tick
  [track carts]
  (loop [[current & remaining] carts
         moved []]
    (if current
      (let [current* (move-cart track carts current)]
        (if-let [crashed (crash? current* moved remaining)]
          {:crash current*}
          (recur remaining (conj moved current*))))
      moved)))

(defn print-tracks
  [{:keys [chars width]}]
  (doseq [line (partition width chars)]
    (println line)))

(defn print-state
  [{:keys [chars width] :as tracks} carts]
  (let [carts-map (->> carts
                       (map (fn [[matrix-coords [_ sym]]]
                               [(to-line-coords tracks matrix-coords) sym]))
                       (into {}))]
    (print-tracks {:chars (into [] (map-indexed #(get carts-map %1 %2) chars))
                   :width width})))

(defn first-crash
  [track carts]
  (loop [carts* carts
         i 0]
    (if (zero? (mod i 10000)) (println carts*))
    (let [next-state (try (tick track carts*) (catch Exception _ (println carts*)))]
      (cond
        (:crash next-state)
        next-state
        (not (seq? next-state)) carts*
        :else (recur next-state (inc i))))))

(let [input (read-input "res/input-13.txt")]
  (println (first-crash (:tracks input) (:carts input))))

(print-state (:tracks input) (:carts input))

(def state (atom (read-input "res/input-13-test.txt")))

(print-state (:tracks @state) (:carts @state))
(next-state)

(defn next-state []
  ;; TODO: order before next tick
  (swap! state (fn [{:keys [tracks carts]}]
                 {:tracks tracks
                  :carts (tick tracks carts)})))

(def input (read-input ))

(print-tracks (:tracks input))
(:carts input)
(track-at-matrix-coords (:tracks input) [113 130])

(to-line-coords (:tracks input) [0 2])





(doseq [line (-> (read-input)
                 (str/split #"\n"))]
  (println (count line))
  )


(distinct [1 2 3 1])
