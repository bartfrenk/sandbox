(ns scratch
  (:require [oz.core :as oz]
            [oz.server :as server]))

(server/stop!)
(server/start-server!)



(defn play-data [& names]
  (for [n names
        i (range 20)]
    {:time i :item n :quantity (+ (Math/pow (* i (count n )) 0.8)
                                  (rand-int (count n)))}))

(defn line-plot [& names]
  {:data {:values (apply play-data names)}
   :encoding {:x {:field "time"}
              :y {:field "quantity"}
              :color {:field "item" :type "nominal"}}
   :mark "line"})

(oz/view! (line-plot "AAAA" "BBB" "CC"))



