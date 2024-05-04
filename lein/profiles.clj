{:user {:plugins      [[nrepl "1.1.1"]
                       [lein-ancient "0.7.0"]
                       [refactor-nrepl "3.10.0"]
                       [cider/cider-nrepl "0.45.0"]]
        :dependencies [
                       ;;[cider/piggieback "0.4.0"]
                       ]
        :repl-options {:init             (set! *print-length* 100)
                       :nrepl-middleware [
                                          cider.nrepl/wrap-apropos
                                          cider.nrepl/wrap-classpath
                                          cider.nrepl/wrap-clojuredocs
                                          cider.nrepl/wrap-complete
                                          cider.nrepl/wrap-content-type
                                          cider.nrepl/wrap-debug
                                          cider.nrepl/wrap-enlighten
                                          cider.nrepl/wrap-format
                                          cider.nrepl/wrap-info
                                          cider.nrepl/wrap-inspect
                                          cider.nrepl/wrap-log
                                          cider.nrepl/wrap-macroexpand
                                          cider.nrepl/wrap-ns
                                          cider.nrepl/wrap-out
                                          cider.nrepl/wrap-slurp
                                          cider.nrepl/wrap-profile
                                          cider.nrepl/wrap-refresh
                                          cider.nrepl/wrap-reload
                                          cider.nrepl/wrap-resource
                                          cider.nrepl/wrap-spec
                                          cider.nrepl/wrap-stacktrace
                                          cider.nrepl/wrap-test
                                          cider.nrepl/wrap-trace
                                          cider.nrepl/wrap-tracker
                                          cider.nrepl/wrap-undef
                                          cider.nrepl/wrap-version
                                          cider.nrepl/wrap-xref

                                          refactor-nrepl.middleware/wrap-refactor
                                          ]}
        :figwheel {:nrepl-middleware
                   [refactor-nrepl.middleware/wrap-refactor
                    cider.nrepl/cider-middleware
                    ;;cemerick.piggieback/wrap-cljs-repl
                    ]}}}}
