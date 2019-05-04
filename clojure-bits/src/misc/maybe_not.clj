(ns misc.maybe-not
  (:require [clojure.spec-alpha2 :as s]))

;; Notes while listening to Rich Hickey's Maybe Not talk.

(def m {:a 1 :b 2})


(s/def :project/project-id int?)
(s/def :project/revision-id int?)
(s/def :project/name string?)
(s/def :project/client-name string?)

(s/def :project/revision (s/schema [:project/project-id
                                    :project/revision-id
                                    :project/name
                                    :project/client-name]))

