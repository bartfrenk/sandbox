(ns advent.day11)

(defn power
  [serial-number [x y]]
  (let [rack-id (+ x 10)]
    (-> (* rack-id y)
        (+ serial-number)
        (* rack-id)
        (quot 100)
        (mod 10)
        (- 5))))

(defn grid
  ([serial-number] (grid serial-number 300))
  ([serial-number size]
   {:grid
    (->> (for [i (range 1 (inc size)) j (range 1 (inc size))] [i j])
         (map (juxt identity (partial power serial-number)))
         (into {}))
    :size size}))

(defn max-power
  [{:keys [grid size]} [x y]]
  (when (= x y) (println x))
  (let [q (dec (max x y))]
    (loop [sz 2
           inner (get grid [x y])
           best {:power (get grid [x y]) :size 1}]
      (if (<= (+ q sz) size)
        (let [hori (->> (for [i (range x (+ x sz))] [i (dec (+ y sz))])
                              (map (partial get grid))
                              (apply +))
              vert (->> (for [j (range y (+ y sz))] [(dec (+ x sz)) j])
                              (map (partial get grid))
                              (apply +))
              power (+ inner hori vert (get grid [(dec (+ x sz)) (dec (+ y sz))]))
              best* (if (> power (:power best)) {:power power :size sz} best)]
          (recur (inc sz) power best*))
        best))))

(def grid-18 (grid 18))
(max-power grid-18 [90 269])

(def grid-9445 (->> (for [x (range 1 301) y (range 1 301)] [x y])
                    (doall)))

(identity grid-9445)
(->> grid-9445
     (reduce (fn [[p* s*] [p s]]
               (if (< (:power s*) (:power s)) [p s] [p* s*]))))


(take 10 (iqterate inc 0))

(iterate grid-9445)

(->> (map (fn [i s] [i s]) (iterate inc 0) grid-9445)

     )



(def grid (power-grid 18 300))
(count grid)

(defn total-power-level
  ([grid pos] (total-power-level grid pos 3))
  ([grid [x y] s]
   (->> (for [i (range s) j (range s)] [i j])
        (map (fn [[i j]] [(+ x i) (+ y j)]))
        (map (partial get grid))
        (apply +))))

(total-power-level grid [90 269] 16)

(defn band
  [grid [x y] s]
  (+
    (get grid [(+ x s) (+ y s)])
    (->> (for [i (range s)] [(+ x i) (+ y s)])
         (map (partial get grid))
         (apply +))
    (->> (for [i (range s)] [(+ x s) (+ y i)])
         (map (partial get grid))
         (apply +))))

(defn best-square
  [grid [x y] size]
  (loop [s 1
         lev* (get grid [x y])
         sq lev*
         s* 0]
    (let [lev** (+ sq (band grid [x y] s))]
      (if (> lev** lev*))
      )
    )
  )
(band grid [1 1] 0)
(get grid [1 1])

(defn largest-total-power
  [serial-number size]
  (let [grid (power-grid serial-number size)
        coords
        (for [i (range 1 size) j (range 1 size) s (range (- (inc size) (max i j)))]
          [[i j] s])]
    (->> coords
         (map (juxt identity (partial apply total-power-level grid)))
         (reduce (fn [[pos* lev*] [pos lev]]
                   (if (< lev* lev) [pos lev] [pos* lev*]))))))


(largest-total-power 9445 [298 298])
(largest-total-power 18 50)

(power-level 8 [3, 5])
(power-level 57 [122, 79])
(power-level 39 [217, 196])
(power-level 71 [101, 153])

(total-power-level 18 [33 45] 20)
(total-power-level 42 [21 61])

