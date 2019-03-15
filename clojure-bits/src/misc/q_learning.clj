(ns misc.q-learning)

;; Notes on ClojureD presentation: https://www.youtube.com/watch?v=6SDCwn8MGtQ

(defn q-learning
  [{:keys [episodes gamma alpha s0 terminal
           actions transition reward policy]}]
  (let [Qs {}]
    (reduce
     (fn [Qs episode]
       (println episode)
       (loop [S s0
              Qs Qs]
         (if (not (terminal S))
           (let [A (policy S (actions S) Qs)
                 R (reward S A)
                 S' (transition S A)
                 ;; Differs from the presentation, since I could not find
                 ;; m/max-by. This forms always selects the last maximal action,
                 ;; instead of the first.
                 A' (or (some->> (Qs S') (apply max-key val) (key))
                        (rand-nth (actions S')))
                 Q-SA (get-in Qs [S A] 0)
                 Q-S'A' (get-in Qs [S' A'] 0)
                 Q-SA (+ Q-SA (* alpha (+ R (- (* gamma Q-S'A') Q-SA))))]
             (recur S' (assoc-in Qs [S A] Q-SA)))))))
    (range 0 episodes)))


