(defproject sisyphus-wiki "0.1.0-SNAPSHOT"
  :description "Sisyphus Wiki."
  :url "https://github.com/eungju/sisyphus-wiki"
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.eclipse.jgit/org.eclipse.jgit "3.3.1.201403241930-r"]
                 [instaparse "1.3.1"]
                 [hiccup "1.0.5"]
                 [ring/ring-core "1.2.2"]
                 [ring/ring-devel "1.2.2"]
                 [http-kit "2.1.16"]]
  :main ^:skip-aot sisyphus-wiki.core
  :target-path "target/%s"
  :plugins [[lein-ring "0.8.10"]]
  :ring {:handler sisyphus-wiki.core/app}
  :profiles {:uberjar {:aot :all}
             :dev {:dependencies [[midje "1.6.3"]]
                   :plugins [[lein-midje "3.1.3"]]}})
