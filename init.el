;; SYSTEM
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta))

(when (eq system-type 'darwin)
  (setenv "PATH" (concat "/opt/local/bin:/opt/local/sbin:" (getenv "PATH")))
  (setenv "PATH" (shell-command-to-string "source $HOME/.bashrc && printf $PATH"))
  (if (not (getenv "TERM_PROGRAM"))
      (let ((path (shell-command-to-string
                   "$SHELL -cl \"printf %s \\\"\\\$PATH\\\"\"")))
        (setenv "PATH" path))))

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))
(add-to-list 'exec-path "~/bin/")

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)
(electric-indent-mode 1)
(setq font-lock-maximum-decoration t)
(show-paren-mode 1)
(setq default-tab-width 2)
(fset 'yes-or-no-p 'y-or-n-p)

(setq-default truncate-lines nil)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(defun buffer-format ()
  "INDENT whole buffer - отформатировать весь буфер"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(global-set-key [f4] 'buffer-format)
(global-set-key [f5] 'toggle-truncate-lines)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-kill-word)

(defun duplicate-line ()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank))

(global-set-key (kbd "C-;") 'duplicate-line)

;; PACKAGE

(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ;; ("melpa-stable" . "https://melpa-stable.milkbox.net/packages/")
                         ;; ("org" . "https://orgmode.org/elpa/")
                         ))

(package-initialize)

(setq package-enable-at-startup nil)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(put 'use-package 'lisp-indent-function 1)
(setq use-package-always-ensure t)

;; Emacs server mode

(use-package server
  :ensure t
  :init
  (server-mode 1)
  :config
  (unless (server-running-p)
    (server-start)))

;; :diminish keyword
(use-package diminish)

;; :bind keyword
;;(use-package bind-key
;;  :ensure t
;;  :pin melpa-stable)

;; :quelpa keyword
(use-package quelpa)
(use-package quelpa-use-package)




(setq scroll-step 1)
(setq inhibit-splash-screen t)
(setq use-dialog-box nil)

;;; interface

(use-package tool-bar
  :ensure nil
  :config
  (tool-bar-mode -1))

(use-package scroll-bar
  :ensure nil
  :config
  (scroll-bar-mode -1))

(use-package menu-bar
  :ensure nil
  :config
  (menu-bar-mode -1)
  :bind
  (([S-f10] . menu-bar-mode)))

;;; fonts & colors

(use-package frame
  :ensure nil
  ;; disable suspending on C-z
  :bind
  (("C-z" . nil))
  ;;:init
  ;;(defvar default-font "PT Mono-14")
  :config
  ;;(set-frame-font default-font)
  ;;(add-to-list 'default-frame-alist `(font . ,default-font))
  (setq initial-frame-alist default-frame-alist)
  (setq display-buffer-alist default-frame-alist)
  (set-fontset-font "fontset-default" 'cyrillic
                    (font-spec :registry "iso10646-1" :script 'cyrillic))
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(default ((t (:family "Fira Code Medium" :foundry "PARA" :slant normal :weight medium :height
                          130 :width normal))))
   '(font-lock-builtin-face ((t (:weight bold))))
   '(font-lock-constant-face ((t (:weight bold))))
   '(font-lock-function-name-face ((t (:weight bold))))
   '(font-lock-keyword-face ((t (:weight bold))))
   '(font-lock-preprocessor-face ((t (:inherit font-lock-builtin-face :weight normal))))
   '(font-lock-type-face ((t (:weight bold))))
   '(font-lock-variable-name-face ((t (:weight bold))))
   '(helm-selection ((t (:background "#b5ffd1" :distant-foreground "black" :underline t))))
   '(helm-selection-line ((t (:background "#FFF876" :underline t))))
   '(tabbar-default ((t (:height 1.2))))))

;; ligatures fonts
(global-prettify-symbols-mode 1)

(use-package hl-line
  :ensure nil
  :config
  (global-hl-line-mode -1))

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

(use-package ibuffer
  :ensure nil
  :bind
  ([remap list-buffers] . ibuffer))

(use-package isearch
  :ensure nil
  :bind
  (:map isearch-mode-map
        ("C-h" . isearch-delete-char)))

(use-package dired
  :ensure nil
  :bind
  ([remap list-directory] . dired)
  :hook
  (dired-mode . dired-hide-details-mode))


;; (use-package leuven-theme
;;   :ensure t
;;   :config
;;   (load-theme 'leuven t))

(use-package solarized-theme
  :if (display-graphic-p)
  :custom (solarized-use-variable-pitch nil)
  :config (load-theme 'solarized-light t))

;; tree ---------------------------------------------------------------------------

(use-package neotree
  :ensure t
  :config
  (global-set-key (kbd "C-c d") 'neotree-toggle))

;; helm ---------------------------------------------------------------------------

(use-package ag
  :ensure t
  :commands (ag ag-regexp ag-project))

(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-buffers-list)
         ("C-M-y" . helm-show-kill-ring)
         ;;([S-f10] . helm-recentf)
         )
  :config
  (setq helm-split-window-in-side-p           nil ; open helm buffer inside current window, not occupy whole other window
        helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
        ;;helm-ff-search-library-in-sexp      t ; search for library in `require' and `declare-function' sexp.
        helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
        ;;helm-ff-file-name-history-use-recentf t
        helm-echo-input-in-header-line t
        helm-buffer-max-length 60 ;; размер в ширину
        ;;helm-always-two-windows t
        ;;helm-split-window-default-side 'right
        helm-autoresize-max-height 30
        helm-autoresize-min-height 30)
  (helm-autoresize-mode 1)
  (helm-mode 1))

(use-package helm-descbinds
  :ensure t
  :bind ("C-c b b" . helm-descbinds))

(use-package helm-swoop
  :ensure t
  :bind (("C-s" . helm-swoop)
         ("C-c s p" . helm-swoop-back-to-last-point)
         ("C-c s m" . helm-multi-swoop)
         ("C-c s a" . helm-multi-swoop-all)))

(use-package helm-ag
  :ensure helm-ag
  :bind ("M-p" . helm-projectile-ag)
  :commands (helm-ag helm-projectile-ag)
  :init (setq helm-ag-insert-at-point 'symbol
              helm-ag-use-agignore 1
              helm-ag-command-option "--path-to-ignore ~/.agignore"))

(use-package perspective
  :init (persp-mode))

(use-package projectile
  :ensure t
  :config
  (projectile-global-mode)
  (setq projectile-enable-caching t))

(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on)
	:bind (("C-c p p" . helm-projectile-switch-project)
				 ("C-c p f" . helm-projectile-find-file)
				 ("C-c p k" . helm-projectile-find-file-in-known-projects)
				 ("C-c p s" . helm-projectile-ag)
				 ("C-c p g" . helm-projectile-grep)
				 ("C-c p b" . helm-projectile-switch-to-buffer)))

(use-package helm-c-yasnippet
  :ensure t
  :config
  (setq helm-yas-space-match-any-greedy t)
  (global-set-key (kbd "C-c y y") 'helm-yas-complete))

(use-package helm-themes
	:ensure t)

(use-package kaolin-themes
	:ensure t)

;; paredit mode -------------------------------------------------------------------

(use-package paredit
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  ;; enable in the *scratch* buffer
  (add-hook 'clojure-mode-hook           #'paredit-mode)
  (add-hook 'clojurescript-mode-hook     #'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook        #'paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode)
  (add-hook 'ielm-mode-hook #'paredit-mode)
  (add-hook 'scheme-mode-hook            #'paredit-mode)
  (add-hook 'lisp-mode-hook #'paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'paredit-mode))

;; multiple cursore
(use-package multiple-cursors
  :bind (:map modi-mode-map
              ("M-\\" . mc/edit-lines)
              ("M--" . mc/mark-all-like-this))

  :bind (:map region-bindings-mode-map
              ("C-M--" . mc/mark-all-in-region))

  :init
  (progn
    (global-set-key (kbd "M-\\") 'mc/edit-lines)
    (global-set-key (kbd "M--") 'mc/mark-all-like-this)
    (global-set-key (kbd "C-M--") 'mc/mark-all-in-region)

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


(use-package ibuffer-vc
  :custom
  (ibuffer-formats
   '((mark modified read-only vc-status-mini " "
           (name 18 18 :left :elide)
           " "
           (size 9 -1 :right)
           " "
           (mode 16 16 :left :elide)
           " "
           (vc-status 10 10 :left)
           " "
           filename-and-process)) "include vc status info")
  :hook
  (ibuffer . (lambda ()
               (ibuffer-vc-set-filter-groups-by-vc-root)
               (unless (eq ibuffer-sorting-mode 'alphabetic)
                 (ibuffer-do-sort-by-alphabetic)))))

(use-package magit
  :ensure t)

(use-package git-gutter
  :ensure t
  :config
  (progn
    (global-git-gutter-mode +1)
    (git-gutter:linum-setup)
    (custom-set-variables
     '(git-gutter:window-width 2)
     '(git-gutter:modified-sign "☁")
     '(git-gutter:added-sign "☀")
     '(git-gutter:deleted-sign "☂")
     '(git-gutter:unchanged-sign " ")
     '(git-gutter:separator-sign "|")
     '(git-gutter:hide-gutter t))
    (set-face-background 'git-gutter:modified "purple") ;; background color
    (set-face-foreground 'git-gutter:added "green")
    (set-face-foreground 'git-gutter:deleted "red")
    (set-face-background 'git-gutter:unchanged "yellow")
    (set-face-foreground 'git-gutter:separator "yellow")
    (add-to-list 'git-gutter:update-hooks 'focus-in-hook))
  :bind (("C-x C-g" . git-gutter:toggle)
         ("C-x v =" . git-gutter:popup-hunk)
         ("C-x p" . git-gutter:previous-hunk)
         ("C-x n" . git-gutter:next-hunk)
         ("C-x v s" . git-gutter:stage-hunk)
         ("C-x v r" . git-gutter:revert-hunk)))

(global-git-gutter-mode +1)

(use-package git-gutter+
  :ensure t
  :config
  (progn
    (global-git-gutter+-mode +1)
    ;;; Jump between hunks
    (define-key git-gutter+-mode-map (kbd "C-x n") 'git-gutter+-next-hunk)
    (define-key git-gutter+-mode-map (kbd "C-x p") 'git-gutter+-previous-hunk)

    ;;; Act on hunks
    (define-key git-gutter+-mode-map (kbd "C-x v =") 'git-gutter+-show-hunk)
    (define-key git-gutter+-mode-map (kbd "C-x r") 'git-gutter+-revert-hunks)
    ;; Stage hunk at point.
    ;; If region is active, stage all hunk lines within the region.
    (define-key git-gutter+-mode-map (kbd "C-x t") 'git-gutter+-stage-hunks)
    (define-key git-gutter+-mode-map (kbd "C-x c") 'git-gutter+-commit)
    (define-key git-gutter+-mode-map (kbd "C-x C") 'git-gutter+-stage-and-commit)
    (define-key git-gutter+-mode-map (kbd "C-x C-y") 'git-gutter+-stage-and-commit-whole-buffer)
    (define-key git-gutter+-mode-map (kbd "C-x U") 'git-gutter+-unstage-whole-buffer))
  :bind (("C-x g" . git-gutter+-mode)
         ("C-x G" . global-git-gutter+-mode)))

(global-git-gutter+-mode +1)

(use-package yasnippet
  :config
  (add-to-list 'load-path "~/.emacs.d/yasnippet")
  (yas-reload-all)
  (yas-global-mode 1))

(use-package flycheck
  :diminish flycheck-mode
  :hook
  (prog-mode . flycheck-mode))

(use-package flycheck-color-mode-line
  :ensure t
  :config
  (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

;; clojure =-------------------------------------------------------------------------------------------

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
  :config
  (setq cider-prefer-local-resources t)
  (setq nrepl-hide-special-buffers nil)
  (setq cider-font-lock-dynamically nil)
  (setq cider-repl-use-pretty-printing t)
  (setq cider-repl-result-prefix ";; => ")
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  ;;(add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)
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

;; clojure =-------------------------------------------------------------------------------------------

(use-package company
  :diminish company-mode
  :hook
  (after-init . global-company-mode))

(use-package reverse-im
  :config
  ;;(add-to-list 'load-path "~/.xkb/contrib")
  (add-to-list 'reverse-im-modifiers 'super)
  (add-to-list 'reverse-im-input-methods
               (if (require 'unipunct nil t)
                   "russian-unipunct"
                 "russian-computer"))
  (reverse-im-mode t))



;; erlang -------------------------------------------------------------------------------------------------------

(use-package erlang
  :ensure t
  :config
  (when (eq system-type 'windows-nt)
    (setq erlang-root-dir "C:/Program Files/erl7.2")
    (add-to-list 'exec-path "C:/Program Files/erl7.2/bin")))

;; markdown -----------------------------------------------------------------------------------------------------

(use-package markdown-mode
  :ensure t)

;; yaml ---------------------------------------------------------------------------------------------------------

(use-package yaml-mode
  :ensure t
  :commands (yaml-mode)
  :mode "\\.yml\\'")


;; JavaScript ---------------------------------------------------------------------------------------------------

(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :config
  ;; Turn off js2 mode errors & warnings (we lean on eslint/standard)
  ;;(setq js2-mode-show-parse-errors nil)
  ;;(setq js2-mode-show-strict-warnings nil)
  (setq js2-strict-missing-semi-warning nil)
  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode))

(use-package js2-refactor
  :ensure t
  :config
  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  (js2r-add-keybindings-with-prefix "C-c C-r")
  (define-key js2-mode-map (kbd "C-k") #'js2r-kill))

(use-package json-reformat
  :ensure t
  :commands json-reformat-region
  :init (progn (setq json-reformat:indent-width 2)
               (setq json-reformat:pretty-string? t)))

(use-package json-mode
  :ensure t
  :mode ("\\.json$" . json-mode)
  :init
  (progn
    (add-hook 'json-mode
              (lambda ()
                (make-local-variable 'js-indent-level)
                (setq js-indent-level 2)))))

;; SQL -----------------------------------------------------------------------------------------------------------

(use-package sql-indent
  :ensure t)

;; termial --------------------------------------------------------

(defun transit-keys-combination-to-term (keys-combination)
  (let ((k (kbd keys-combination)))
    (define-key term-raw-map k
      (lookup-key (current-global-map) k))))

(add-hook 'term-load-hook
          (lambda ()
            (transit-keys-combination-to-term "M-x")
            (transit-keys-combination-to-term "C-x")
            ;;(set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix)
            ;; (Let ((base03 "#002b36")
            ;;       (base02 "#073642")
            ;;       (base01 "#586e75")
            ;;       (base00 "#657b83")
            ;;       (base0 "#839496")
            ;;       (base1 "#93a1a1")
            ;;       (base2 "#eee8d5")
            ;;       (base3 "#fdf6e3")
            ;;       (yellow "#b58900")
            ;;       (orange "#cb4b16")
            ;;       (red "#dc322f")
            ;;       (magenta "#d33682")
            ;;       (violet "#6c71c4")
            ;;       (blue "#268bd2")
            ;;       (cyan "#2aa198")
            ;;       (green "#859900"))
            ;;   (setq ansi-term-color-vector
            ;;         (vconcat `(unspecified ,base02 ,red ,green ,yellow ,blue
            ;;                                ,magenta ,cyan ,base2))))
            ))

(setq tt-id 0)
(defun get-tts-id ()
  (concat "term-" (number-to-string (setq tt-id (+ 1 tt-id)))))

(defun tt-0 (name)
  (interactive "sbuffername:")
  (ansi-term "/bin/bash")
  (insert "export LANG=en_US.UTF-8; source ~/.bash_profile")
  (term-send-input)
  (rename-buffer (concat "ttt: " name)))

(defun ttt (name)
  (interactive "sbuffername:")
  (tt-0 name))

(defun tti ()
  (interactive)
  (tt-0 (get-tts-id)))

;; TAIL CONFIG ------------------------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
	 [default default default italic underline success warning error])
 '(ansi-color-names-vector
	 ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
	 (quote
		("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(fci-rule-color "#eee8d5")
 '(git-gutter:added-sign "☀")
 '(git-gutter:deleted-sign "☂")
 '(git-gutter:hide-gutter t)
 '(git-gutter:modified-sign "☁")
 '(git-gutter:separator-sign "|")
 '(git-gutter:unchanged-sign " ")
 '(git-gutter:window-width 2)
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
	 (--map
		(solarized-color-blend it "#fdf6e3" 0.25)
		(quote
		 ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
	 (quote
		(("#eee8d5" . 0)
		 ("#B4C342" . 20)
		 ("#69CABF" . 30)
		 ("#69B7F0" . 50)
		 ("#DEB542" . 60)
		 ("#F2804F" . 70)
		 ("#F771AC" . 85)
		 ("#eee8d5" . 100))))
 '(hl-bg-colors
	 (quote
		("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342")))
 '(hl-fg-colors
	 (quote
		("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3")))
 '(hl-paren-colors (quote ("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900")))
 '(ibuffer-formats
	 (quote
		((mark modified read-only vc-status-mini " "
					 (name 18 18 :left :elide)
					 " "
					 (size 9 -1 :right)
					 " "
					 (mode 16 16 :left :elide)
					 " "
					 (vc-status 10 10 :left)
					 " " filename-and-process))))
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
	 (quote
		("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(package-selected-packages
	 (quote
		(kaolin-themes helm-themes sql-indent json-mode json-reformat js2-refactor js2-mode yaml-mode markdown-mode erlang reverse-im company kibit-helper cljsbuild-mode clojure-snippets cljr-helm clj-refactor cider clojure-mode-extra-font-locking clojure-mode flycheck-color-mode-line flycheck git-gutter+ git-gutter magit ibuffer-vc multiple-cursors paredit helm-c-yasnippet helm-projectile projectile perspective helm-ag helm-swoop helm-descbinds helm ag neotree solarized-theme quelpa-use-package quelpa diminish use-package)))
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(solarized-use-variable-pitch nil)
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
	 (quote
		((20 . "#dc322f")
		 (40 . "#c85d17")
		 (60 . "#be730b")
		 (80 . "#b58900")
		 (100 . "#a58e00")
		 (120 . "#9d9100")
		 (140 . "#959300")
		 (160 . "#8d9600")
		 (180 . "#859900")
		 (200 . "#669b32")
		 (220 . "#579d4c")
		 (240 . "#489e65")
		 (260 . "#399f7e")
		 (280 . "#2aa198")
		 (300 . "#2898af")
		 (320 . "#2793ba")
		 (340 . "#268fc6")
		 (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
	 (quote
		(unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496")))
 '(xterm-color-names
	 ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright
	 ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Fira Code Medium" :foundry "PARA" :slant normal :weight medium :height 150 :width normal))))
 '(font-lock-builtin-face ((t (:weight bold))))
 '(font-lock-constant-face ((t (:weight bold))))
 '(font-lock-function-name-face ((t (:weight bold))))
 '(font-lock-keyword-face ((t (:weight bold))))
 '(font-lock-preprocessor-face ((t (:inherit font-lock-builtin-face :weight normal))))
 '(font-lock-type-face ((t (:weight bold))))
 '(font-lock-variable-name-face ((t (:weight bold))))
 '(helm-selection ((t (:background "#b5ffd1" :distant-foreground "black" :underline t))))
 '(helm-selection-line ((t (:background "#FFF876" :underline t))))
 '(tabbar-default ((t (:height 1.2)))))
