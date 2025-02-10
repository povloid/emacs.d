(use-package flycheck
  :ensure t
  :custom
  (flycheck-display-errors-delay 2))

;; Fly-check красит mode-line в желтый цвет - неочень хорошо
;; (use-package flycheck-color-mode-line
;;   :ensure t
;;   :config
;;   (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))
