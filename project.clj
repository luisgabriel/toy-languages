(defproject toy-languages "0.1.0"
  :description "FIXME: write description"
  :license {:name "MIT License"
            :url "http://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [instaparse "1.3.1"]]
  :main ^:skip-aot toy-languages.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
