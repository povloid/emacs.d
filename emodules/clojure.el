;;;**************************************************************************************************
;;;* BEGIN Clojure
;;;* tag: <clojure>
;;;*
;;;* description: Язык программирования CLOJURE
;;;*
;;;**************************************************************************************************

(use-package clojure-mode
  :ensure t
  :mode (("clj\\'" . clojure-mode)
         ("cljs\\'" . clojurescript-mode)
         ("cljc\\'" . clojurec-mode)
         (".lein-env\\'" . clojure-mode))
  :config
  (add-hook 'clojure-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook #'subword-mode)
  ;;(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
  (setq tags-revert-without-query t)
  (setq tags-add-tables nil)
  ;; (setq clojure-indent-style :align-arguments)
  ;; (put-clojure-indent 'ch/modify-column 1)
  ;; (put-clojure-indent 's/fdef 1)
  ;; (put-clojure-indent 'ch/add-columns 1)
  ;; (put-clojure-indent 'ch/add-foreign-key-constraint 1)
  ;; (put-clojure-indent 'ch/create-index 1)
  ;; (put-clojure-indent 'ch/create-table 1)
  ;; (put-clojure-indent 'ch/insert-data 1)
  ;; (put-clojure-indent 'ch/update-data 1)
  ;; (put-clojure-indent 'ch/add-unique-constraint 1)
  ;; (put-clojure-indent 'ch/add-foreign-key-constraint 1)
  )

(use-package clojure-mode-extra-font-locking
  :ensure t)

(use-package cider
  :ensure t
  :pin melpa-stable
  :config
  (setq cider-prefer-local-resources t)
  (setq nrepl-hide-special-buffers nil)
  (setq cider-font-lock-dynamically nil)
  (setq cider-repl-use-pretty-printing t)
  (setq cider-repl-result-prefix ";; => ")
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (add-hook 'cider-repl-mode-hook (lambda ()
                                    (setq truncate-lines t)))
  ;; (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode) ;; разноцветные скобки
  (defun figwheel-repl ()
    (interactive)
    (run-clojure "lein figwheel"))
  (defun cider-repl-to-figwhell-repl ()
    (interactive)
    (save-some-buffers)
    (goto-char (point-max))
    (insert "(do (use 'figwheel-sidecar.repl-api) (cljs-repl))")))

(use-package clj-refactor
  :ensure t
  :commands clj-refactor-mode
  :init
  (setq cljr-warn-on-eval nil)
  (cljr-add-keybindings-with-prefix "C-c C-m")
  :hook
  ((cider-mode . clj-refactor-mode)))

(use-package cljr-helm
  :ensure t
  :commands cljr-helm
  :init (define-key clojure-mode-map (kbd "M-RET") 'cljr-helm))

(use-package clojure-snippets
  :ensure t
  :config (clojure-snippets-initialize))

(use-package cljsbuild-mode
  :ensure t
  :commands cljsbuild-start)

(use-package kibit-helper
  :ensure t
  :commands kibit kibit-current-file
  :bind (("C-x C-`" . kibit-accept-proposed-change)))

(use-package sotclojure
  :ensure t :disabled t)
