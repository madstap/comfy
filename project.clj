(defproject madstap/comfy "1.0.0-alpha3"
  :description "Clojure(script) utils"
  :url "http://example.com/madstap/comfy"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.9.0-alpha17"]
                 [org.clojure/clojurescript "1.9.671" :scope "provided"]]

  :plugins [[lein-codox "0.10.3"]
            [lein-cljsbuild "1.1.6"]
            [lein-doo "0.1.7"]]

  :codox {:output-path "docs"
          :metadata {:doc/format :markdown}
          :source-uri "http://github.com/madstap/comfy/blob/{version}/{filepath}#L{line}"}

  :cljsbuild
  {:builds

   {:test
    {:source-paths ["src" "test"]
     :compiler {:output-to "target/main.js"
                :output-dir "target"
                :main madstap.comfy.test-runner
                :optimizations :simple}}}}

  :doo {:paths {:rhino "lein run -m org.mozilla.javascript.tools.shell.Main"}}

  :aliases
  {"test-cljs" ["doo" "rhino" "test" "once"]
   "test-clj"  ["with-profile" "+1.9" "test"]
   "test-all"  ["do" ["test-clj"] ["test-cljs"]]}

  :profiles
  {:dev {:dependencies [[com.cemerick/piggieback "0.2.2"]
                        [org.clojure/test.check "0.10.0-alpha2"]
                        [org.clojure/tools.nrepl "0.2.10"]]
         :repl-options {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}}
   :test {:dependencies [[org.mozilla/rhino "1.7.7"]]}
   :1.9 {:dependencies [[org.clojure/clojure "1.9.0-alpha17"]]}})
