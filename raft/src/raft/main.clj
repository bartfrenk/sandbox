(ns raft.main
  (:require [clojure.spec.alpha :as s]
            [slacker.server :refer [start-slacker-server stop-slacker-server]]
            [slacker.client :refer [slackerc call-remote]]
            [com.stuartsierra.component :as component]
            [taoensso.timbre :as log])
  (:import [java.util Timer TimerTask]))



;; ==== Timer functions ====

(defn create-timer
  ([] (create-timer true))
  ([daemon?] (Timer. daemon?))
  ([name daemon?] (Timer. name daemon?)))

(defmacro schedule
  [timer delay & body]
  `(let [task# (proxy [TimerTask] [] (run [] ~@body))]
     (.schedule ~timer task# ~delay)
     task#))

(defn cancel
  [task]
  (when task
    (.cancel task)))


;; ===== Raft =====

(defn -main
  [& args]
  (println "Hello world!"))

(def slacker-ns "raft")

(defn ping*
  []
  :pong)

(defn ping
  [node]
  (ping*))

(defn api
  [node]
  {slacker-ns {"ping" (partial ping node)}})

(declare set-timeout)

(defn handle-timeout
  [{config :config :as node}]
  (log/infof "Timeout for node %s" (:port config))
  (set-timeout node))

(defn timeout-ms
  [role]
  1000)

(defn set-timeout
  [{:keys [state timer] :as node}]
  (swap! state
         #(let [delay (timeout-ms (:role %))]
            (update % :timeout
                    (fn [timeout]
                      (cancel timeout)
                      (schedule timer delay (handle-timeout node)))))))

(defrecord Node [config server peers timer state]
  component/Lifecycle
  (start [this]
    (if server this
        (let [node (->> (start-slacker-server (api this) (:port config))
                        (assoc this :server))]
          (log/infof "Started RPC server on port %s" (:port config))
          (set-timeout node)
          node)))

  (stop [this]
    (if-not server this
            (do
              (swap! state #(update % :timeout (fn [timeout] (cancel timeout) nil)))
              (stop-slacker-server server)
              (log/infof "Stopped RPC server on port %s" (:port config))
              (assoc this :server nil)))))


(s/def ::port integer?)
(s/def ::config (s/keys :req-un [::port]))

(defn new-node
  ([config] (new-node config []))
  ([config peers]
   {:pre [(s/valid? ::config config)]}
   (map->Node {:config config
               :peers (atom peers)
               :timer (create-timer)
               :state (atom {:role :follower})})))

(defn call-node
  [{host :host port :port} fn-name & args]
  (let [client (slackerc (str host ":" port)) ]
    (call-remote client slacker-ns fn-name args)))

(defn create-peers [n]
  (->> (range 8000 (+ 8000 n))
       (map #(do {:host "localhost" :port %}))))

(def nodes
  (let [peers (create-peers 3)]
    (map (fn [{port :port}]
           (new-node
            {:port port}
            (filter #(not= port (:port %)) peers)))
         peers)))

(alter-var-root #'nodes #(doall (map component/start %)))
(alter-var-root #'nodes #(doall (pmap component/stop %)))


