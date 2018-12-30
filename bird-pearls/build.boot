(def project 'clj-pearls)
(def version "0.1.0-SNAPSHOT")

(set-env! :resource-paths #{"src"}
          :source-paths   #{"test" "bench"}
          :dependencies   '[[org.clojure/clojure "1.9.0"]
                            [adzerk/boot-test "1.2.0" :scope "test"]
                            [org.clojure/test.check "0.10.0-alpha2" :scope "test"]
                            [criterium "0.4.4" :scope "test"]])


(task-options!
 pom {:project     project
      :version     version
      :description "FIXME: write description"
      :url         "http://example/FIXME"
      :scm         {:url "https://github.com/yourname/clj-pearls"}
      :license     {"Eclipse Public License"
                    "http://www.eclipse.org/legal/epl-v10.html"}})

(deftask build
  "Build and install the project locally."
  []
  (comp (pom) (jar) (install)))

(require '[adzerk.boot-test :refer [test]])
