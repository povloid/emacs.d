(use-package company
  :hook
  (after-init . global-company-mode)
  :config
  (global-set-key (kbd "M-/") 'company-indent-or-complete-common)
  (global-set-key (kbd "M-m") 'company-complete-selection))
