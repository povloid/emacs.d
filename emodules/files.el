;;------------------------------------------------------------------------------
;; BEGIN: files
;; tag: <files>
;; description: Файлы
;;------------------------------------------------------------------------------

(use-package files
  :ensure nil
  :hook
  (before-save . delete-trailing-whitespace)
  :config
  (setq require-final-newline t)
  ;; backup settings
  (setq backup-by-copying t)
  (setq backup-directory-alist
        '(("." . "~/.cache/emacs/backups")))
  (setq delete-old-versions t)
  (setq kept-new-versions 6)
  (setq kept-old-versions 2)
  (setq version-control t))

(use-package dired
  :ensure nil
  :bind
  ([remap list-directory] . dired)
  :hook
  (dired-mode . dired-hide-details-mode))

(use-package diredfl
  :ensure t
  :config
  (diredfl-global-mode 1))

;; (setq
;;  dired-listing-switches "-lXGh --group-directories-first"
;;  dired-dwim-target t)
;; (add-hook 'dired-mode-hook 'dired-hide-details-mode)

(use-package neotree
  :ensure t
  :config
  (global-set-key (kbd "C-c d") 'neotree-toggle))
