(define-package "cljr-helm" "0.11" "Wraps clojure refactor commands with helm"
  '((clj-refactor "0.13.0")
    (helm-core "1.7.7")
    (cl-lib "0.5"))
  :commit "f2fc7b698a56e4a44d5dfbc6a55d77a93c0fa9a4" :authors
  '(("Phil Jackson" . "phil@shellarchive.co.uk"))
  :maintainers
  '(("Phil Jackson" . "phil@shellarchive.co.uk"))
  :maintainer
  '("Phil Jackson" . "phil@shellarchive.co.uk")
  :keywords
  '("helm" "clojure" "refactor")
  :url "https://github.com/philjackson/cljr-helm")
;; Local Variables:
;; no-byte-compile: t
;; End: