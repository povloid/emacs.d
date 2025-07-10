;;;**************************************************************************************************
;;;* BEGIN System
;;;* tag: <system >
;;;*
;;;* description:
;;;*
;;;**************************************************************************************************

;;; Code:
;; Поведение клавиш на разных операционках
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta))

;; OSX
(when (eq system-type 'darwin)
  (if (not (getenv "TERM_PROGRAM"))
      (let ((path (shell-command-to-string
                   "$SHELL -cl \"printf %s \\\"\\\$PATH\\\"\"")))
        (setenv "PATH" path))))

;; PATH's
;;(setenv "PATH" (shell-command-to-string "source $HOME/.zprofile && printf $PATH"))
;;(setenv "PATH" (concat "/opt/local/bin:/opt/local/sbin:" (getenv "PATH")))
;;(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
;;(setq exec-path (append exec-path '("/usr/local/bin")))

;; check (executable-find "sls") for example
;; check (executable-find "npm") for example

;;(add-to-list 'exec-path "~/bin/")
;;(add-to-list 'exec-path "~/global/npm/bin/")
;;(add-to-list 'exec-path "~/global/yarn/bin/")

(setenv "LC_ALL" "en_US.UTF-8")
(setenv "LANG" "en_US.UTF-8")
(setenv "LANGUAGE" "en_US.UTF-8")

;;; END System
;;;..................................................................................................


(setq warning-minimum-level :emergency)

(add-to-list 'default-frame-alist '(height . 36))
(add-to-list 'default-frame-alist '(width . 120))


;; https://emacs-lsp.github.io/lsp-mode/page/performance/

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold (* 50 1024 1024))

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold (* 100 1024 1024))

(setq read-process-output-max (* 3 1024 1024)) ;; 1mb

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))

(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; nice scrolling
(setq scroll-margin 4
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)
(electric-indent-mode 1)
(setq font-lock-maximum-decoration t)
(show-paren-mode 1)
(setq default-tab-width 2)
(fset 'yes-or-no-p 'y-or-n-p)
(set-default 'truncate-lines -1)

;; Set UTF-8 encoding
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)

;; unix
(set-buffer-file-coding-system 'unix)

;; Disable bell
;; This is annoying, remove this line if you like being visually reminded of events.
(setq ring-bell-function 'ignore)

;; Disable backups and auto-saves
;; I don’t use either, you might want to turn those from nil to t if you do.

(setq make-backup-files nil)
(setq auto-save-default nil)

(setq scroll-step 1)
(setq inhibit-splash-screen t)
(setq use-dialog-box nil)

;; Cursor position
;; Show the current line and column for your cursor.
;; We are not going to have relative-linum-mode in every major mode, so this is useful.

(setq line-number-mode t)
(setq column-number-mode t)

(defun buffer-format ()
  "INDENT whole buffer - отформатировать весь буфер"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))


(global-set-key (kbd "C-h")   'delete-backward-char)
(global-set-key (kbd "C-M-h") 'backward-kill-word)

;;(global-set-key [f4] 'buffer-format)
;;(global-set-key [f5] 'toggle-truncate-lines)
;;(global-set-key [f12] 'kmacro-end-and-call-macro)



;; fix seq
(setq package-install-upgrade-built-in t)

(progn (unload-feature 'seq t) (require 'seq))
