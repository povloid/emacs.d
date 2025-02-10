;; (setq-default cursor-type 'hbar )
(setq-default cursor-type '(hbar . 4))

(use-package paredit
  :ensure t
  :pin melpa-stable
  :config
  (add-hook 'emacs-lisp-mode-hook                  #'paredit-mode)
  ;; enable in the *scratch* buffer
  (add-hook 'clojure-mode-hook                     #'paredit-mode)
  (add-hook 'clojurescript-mode-hook               #'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook                  #'paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode)
  (add-hook 'ielm-mode-hook                        #'paredit-mode)
  (add-hook 'scheme-mode-hook                      #'paredit-mode)
  (add-hook 'lisp-mode-hook                        #'paredit-mode)
  (add-hook 'lisp-interaction-mode-hook            #'paredit-mode))

;; multiple cursore
(use-package multiple-cursors
  :bind (:map modi-mode-map
              ("M-\\" . mc/edit-lines)
              ("M--" . mc/mark-all-like-this))

  :bind (:map region-bindings-mode-map
              ("C-M-_" . mc/mark-all-in-region))

  :init
  (progn
    (global-set-key (kbd "M-\\") 'mc/edit-lines)
    (global-set-key (kbd "M--") 'mc/mark-all-like-this)
    (global-set-key (kbd "C-M-_") 'mc/mark-all-in-region)

    (setq mc/cmds-to-run-for-all
          '(company-indent-or-complete-common
            electric-newline-and-maybe-indent
            helm-M-x
            kill-region
            mark-sexp
            org-beginning-of-line
            org-end-of-line
            org-self-insert-command
            paredit-close-round
            paredit-doublequote
            paredit-forward
            paredit-forward-delete
            paredit-forward-slurp-sexp
            paredit-kill
            paredit-open-round  ;; круглые скобки
            paredit-open-curly  ;; фигурные скобки
            paredit-open-square ;; квадратные скобки
            paredit-semicolon
            paredit-splice-sexp
            paredit-backslash
            sgml-slash
            yaml-electric-backspace))

    (setq mc/cmds-to-run-once
          '(delete-window
            handle-switch-frame
            helm-buffers-list
            helm-projectile-find-file))))

(use-package hi-lock
  :init (global-hi-lock-mode))

(use-package highlight-numbers
  :ensure t
  :defer t
  :init (add-hook 'prog-mode-hook #'highlight-numbers-mode))

;;------------------------------------------------------------------------------
;; BEGIN: My Edit
;; tag: <my edit definition>
;; description:
;;------------------------------------------------------------------------------

(defun buffer-format ()
  "INDENT whole buffer - отформатировать весь буфер"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

;;(global-set-key [f2] 'magit-push-current-to-upstream)
;;(global-set-key [f4] 'buffer-format)
;;(global-set-key [f4] 'lsp-format-buffer)
;;(global-set-key [f5] 'toggle-truncate-lines)

;; -----------------------------------------------------------------------------

(defun dublicate-line ()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank))

(global-set-key (kbd "C-;") 'dublicate-line)

;; -----------------------------------------------------------------------------

;; (global-set-key (kbd "C-SPC") 'set-mark-command)

;; -----------------------------------------------------------------------------
;; - (kill-region BEG END &optional REGION)

(when (eq system-type 'darwin)
  (progn
    (defun pbcut ()
      (interactive)
      (call-process-region (point) (mark) "pbcopy")
      (kill-region (point) (mark)))

    (global-set-key (kbd "C-w") 'pbcut)))

(when (eq system-type 'gnu/linux)
  (progn
    (defun pbcut ()
      (interactive)
      (call-process-region (point) (mark) "xclip" nil nil nil "-selection" "clipboard")
      (kill-region (point) (mark)))

    (global-set-key (kbd "C-w") 'pbcut)))

;; -----------------------------------------------------------------------------
;; - (kill-ring-save BEG END &optional REGION)

(when (eq system-type 'darwin)
  (progn
    (defun pbcopy ()
      (interactive)
      (call-process-region (point) (mark) "pbcopy")
      (kill-ring-save (point) (mark)))

    (global-set-key (kbd "M-w") 'pbcopy)))

(when (eq system-type 'gnu/linux)
  (progn
    (defun pbcopy ()
      (interactive)
      (call-process-region (point) (mark) "xclip" nil nil nil "-selection" "clipboard")
      (kill-ring-save (point) (mark)))

    (global-set-key (kbd "M-w") 'pbcopy)))

;; -----------------------------------------------------------------------------
;; - (yank &optional ARG)

(when (eq system-type 'darwin)
  (progn
    (defun pbpaste ()
      (interactive)
      (call-process-region (point)
			   (if mark-active
			       (mark)
			     (point))
			   "pbpaste" t t))

    (global-set-key (kbd "C-c C-y") 'pbpaste)))


;;(message "[ok]\tLoaded My Definitions Modules")

;; END
;;..............................................................................
