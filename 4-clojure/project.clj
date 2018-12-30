(defproject clojure-exercises "0.1.0-SNAPSHOT"
  :description "Exercises in Clojure"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]]
  :main ^:skip-aot exercises.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
