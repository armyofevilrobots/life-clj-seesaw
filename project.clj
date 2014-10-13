(defproject life_seesaw "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :main life_seesaw.rewrite
  :dependencies [
                 [org.clojure/clojure "1.6.0"]
                 [seesaw "1.4.4"]
                 ]
  :profiles {
             :uberjar {:aot :all }}
  )
