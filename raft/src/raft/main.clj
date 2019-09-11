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

(s/def ::term pos?)

(defn append-entries
  [node term leader-id])

(defn api
  [node]
  {slacker-ns {"ping" (partial ping node)
               "append-entries" (partial append-entries node)}})


(declare set-timeout)

(defn start-election
  [{:keys [config] :as node}]
  ;; Here we start the election, need to send async rpc requests, and let the
  ;; vote counting be done in a callback
  (log/infof "Starting election for node %s" (:port node)))

(defn handle-timeout
  [{config :config :as node}]
  (letfn [(update-state [state]
            (case (:role state)

              :follower
              (do (-> (assoc state :role :candidate)
                      (update :term inc)
                      (update :timeout set-timeout node :candidate))
                  (start-election node))

              :candidate
              (log/infof "Election timed out")))])
  (log/infof "Timeout for node %s" (:port config)))


(defn timeout-ms
  [role]
  1000)

(defn delay-ms
  [role]
  1000)

(defn set-timeout
  [timeout {timer :timer :as node} role]
  (cancel timeout)
  (schedule timer (delay-ms role) (handle-timeout node)))

(s/def ::role #{:leader :follower :candidate})
(s/def ::timeout (partial instance? TimerTask))
(s/def ::state
  (s/keys :req-un [::role
                   ::term]
          :opt-un [::timeout]))

(defrecord Node [config server peers timer state-ref]
  component/Lifecycle
  (start [this]
    (if server this
        (let [node (->> (start-slacker-server (api this) (:port config))
                        (assoc this :server))]
          (log/infof "Started RPC server on port %s" (:port config))
          (update state-ref :timeout set-timeout :follower node)
          node)))

  (stop [this]
    (if-not server this
            (do
              (swap! state-ref #(update % :timeout (fn [timeout] (cancel timeout) nil)))
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
               :state-ref (atom {:role :follower
                                 :term 0})})))


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
