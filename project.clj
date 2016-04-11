(defproject evospace-pso "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [com.github.yannrichet/JMathPlot "1.0.1"]
                 [clj-http "2.1.0"]
                 [org.clojure/data.json "0.2.6"]
                 [incanter "1.9.0"]
                 [net.boostrot/pso "0.1.0"]]
  :main ^:skip-aot evospace-pso.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}}
  :java-source-paths ["src/java"])
