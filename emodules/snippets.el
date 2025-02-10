;;;**************************************************************************************************
;;;* BEGIN Snippets
;;;* tag: <snippets>
;;;*
;;;* description: Сниппеты
;;;*
;;;**************************************************************************************************

(use-package yasnippet
  :config
  (add-to-list 'load-path "~/.emacs.d/yasnippet")
  (yas-reload-all)
  ;; Activate global
  (yas-global-mode 1))

(use-package helm-c-yasnippet
  :ensure t
  :config
  (setq helm-yas-space-match-any-greedy t)
  (global-set-key (kbd "C-c y y") 'helm-yas-complete))

(use-package yasnippet-snippets
  :ensure t)

(use-package yasnippet-classic-snippets
  :ensure t)


