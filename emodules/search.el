(use-package isearch
  :ensure nil
  :bind
  (:map isearch-mode-map
        ("C-h" . isearch-delete-char)))

(use-package ag
  :ensure
  ;;:commands (ag ag-regexp ag-project)
  )

;; (use-package helm-ag
;;   :ensure helm-ag
;;   :bind ("M-p" . helm-projectile-ag)
;;   :commands (helm-ag helm-projectile-ag)
;;   :init (setq helm-ag-insert-at-point 'symbol
;;               helm-ag-use-agignore 1
;;            helm-ag-command-option "-U"))
