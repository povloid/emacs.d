{:user {:plugins      [[lein-ancient "0.6.14"]]
        :dependencies [[nrepl "0.6.0"]
                       [cider/cider-nrepl "0.24.0"]
                       [cider/piggieback "0.4.0"]
                       [refactor-nrepl "2.5.0-SNAPSHOT"]]
        :repl-options {:init             (set! *print-length* 100)
                       :nrepl-middleware [cider.nrepl/wrap-apropos
                                          cider.nrepl/wrap-classpath
                                          cider.nrepl/wrap-complete
                                          cider.nrepl/wrap-debug
                                          cider.nrepl/wrap-format
                                          cider.nrepl/wrap-info
                                          cider.nrepl/wrap-inspect
                                          cider.nrepl/wrap-macroexpand
                                          cider.nrepl/wrap-ns
                                          cider.nrepl/wrap-spec
                                          cider.nrepl/wrap-profile
                                          cider.nrepl/wrap-refresh
                                          cider.nrepl/wrap-resource
                                          cider.nrepl/wrap-stacktrace
                                          cider.nrepl/wrap-test
                                          cider.nrepl/wrap-trace
                                          cider.nrepl/wrap-out
                                          cider.nrepl/wrap-undef
                                          cider.nrepl/wrap-version
                                          refactor-nrepl.middleware/wrap-refactor]}
        :figwheel     {:nrepl-middleware
                       [refactor-nrepl.middleware/wrap-refactor
                        cider.nrepl/cider-middleware
                        ;;cemerick.piggieback/wrap-cljs-repl
                        ]}}}}
