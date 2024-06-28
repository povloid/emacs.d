;;; package --- Pacmans config
;;; Commentary:
;;;**************************************************************************************************
;;;* BEGIN System
;;;* tag: <system >
;;;*
;;;* description: Какието описания для операционных систем
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
(setq exec-path (append exec-path '("/usr/local/bin")))

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

;;------------------------------------------------------------------------------
;; BEGIN: Use packages and repositories
;; tag: <use-package repository>
;; description: Пакеты и репозитории
;;------------------------------------------------------------------------------

(require 'package)
(setq package-archives

      ;; '(("elpa"     . "https://elpa.gnu.org/packages/")
      ;;   ("melpa-stable" . "https://stable.melpa.org/packages/")
      ;;   ("melpa"        . "https://melpa.org/packages/"))

      '(("elpa"     . "https://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://www.mirrorservice.org/sites/melpa.org/packages/")
        ("melpa"        . "https://www.mirrorservice.org/sites/stable.melpa.org/packages/"))

      package-archive-priorities
      '(
	("melpa-stable" . 10)
        ("elpa"         . 5)
        ("melpa"        . 0)
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

;; :quelpa keyword
(use-package quelpa)
(use-package quelpa-use-package)

;; END Use packages and repositories
;;..............................................................................

;;;**************************************************************************************************
;;;* BEGIN Bitwarden
;;;* tag: <bitwarden>
;;;*
;;;* description:
;;;*
;;;**************************************************************************************************

(setq bitwarden-session nil)

(defun bitwarden-unlock ()
  (interactive)
  (let ((password (read-passwd "Master Password:  ")))
    (let ((session-id (setq bitwarden-session (shell-command-to-string (concat "bw --raw unlock \"" password "\"")))))
      (if (string= session-id "Invalid master password.")
          (message "Invalid master password.")
        (progn
          (setq bitwarden-session session-id)
          (message "Session started"))))))

(defun bitwarden-lock ()
  (interactive)
  (let ((cmd (concat "export BW_SESSION=\""  bitwarden-session "\"; bw lock")))
    (shell-command-to-string cmd)
    (setq bitwarden-session nil)))

(defun bitwarden-sync ()
  "bitwarden sync"
  (interactive)
  (let ((cmd (concat "export BW_SESSION=\""  bitwarden-session "\"; bw sync")))
    (json-parse-string
     (shell-command-to-string cmd))))

(defun bitwarden-get-item (name)
  (interactive)
  (let ((cmd (concat "export BW_SESSION=\""  bitwarden-session "\"; bw get item " name)))
    ;;(message cmd)
    (json-parse-string
     (shell-command-to-string cmd))))

(defun bitwarden-get-field-value (name fields)
  (car
   (seq-filter
    (lambda (field)
      (string= (gethash "name" field) name))
    fields)))

;; example
;;(gethash "login" (bitwarden-get-item "overtok-rds"))
;;(bitwarden-get-field-value "port" (gethash "fields" (bitwarden-get-item "overtok-rds")))


;;; END Bitwarden
;;;..................................................................................................

;;;**************************************************************************************************
;;;* BEGIN common configuration
;;;* tag: <common>
;;;*
;;;* description: Общие параметры
;;;*
;;;**************************************************************************************************

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

;;------------------------------------------------------------------------------
;; BEGIN: Emacs server
;; tag: <server>
;; description: Старт сервера
;;------------------------------------------------------------------------------

(use-package server
  :ensure t
  :init
  (server-mode 1)
  :config
  (unless (server-running-p)
    (server-start)))

;; (use-package keyfreq
;;   :config
;;   (keyfreq-mode 1)
;;   (keyfreq-autosave-mode 1))

;; END Emacs server
;;..............................................................................

;;------------------------------------------------------------------------------
;; BEGIN: interface
;; tag: <interface>
;; description: Интерфей и тема
;;------------------------------------------------------------------------------

(setq scroll-step 1)
(setq inhibit-splash-screen t)
(setq use-dialog-box nil)

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
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   ;; '(default ((t (:family "PT Mono" :foundry "PARA" :slant normal :weight medium :height 130 :width normal))))
   ;; '(default ((t (:family "Fira Mono" :foundry "PARA" :slant normal :weight medium :height 170 :width normal))))
   ;;'(default ((t (:family "JetBrains Mono" :foundry "PARA" :slant normal :weight medium :height 130 :width normal))))
   '(default ((t (:family "Iosevka" :foundry "PARA" :slant normal :weight medium :height 130 :width normal))))
   ;;'(default ((t (:family "Iosevka Slab" :foundry "PARA" :slant normal :weight light :height 200 :width normal))))
   ;;'(default ((t (:family "Iosevka Slab" :foundry "PARA" :slant normal :weight light :height 130 :width normal))))
   ;;'(default ((t (:family "Source Code Pro" :foundry "PARA" :slant normal :weight light :height 150 :width normal))))
   ;;'(default ((t (:family "Go Mono" :foundry "PARA" :slant normal :weight light :height 170 :width normal))))
   '(font-lock-comment-face ((t (:weight normal :slant italic))))
   '(font-lock-builtin-face ((t (:weight bold))))
   '(font-lock-constant-face ((t (:weight bold))))
   '(font-lock-function-name-face ((t (:weight bold))))
   '(font-lock-keyword-face ((t (:weight bold))))
   '(font-lock-preprocessor-face ((t (:inherit font-lock-builtin-face :weight normal))))
   '(font-lock-type-face ((t (:weight bold))))
   '(font-lock-variable-name-face ((t (:weight bold))))
   ;; '(helm-selection ((t (:background "#b5ffd1" :distant-foreground "black" :underline t))))
   ;; '(helm-selection-line ((t (:background "#FFF876" :underline t))))
   '(tabbar-default ((t (:height 1.2))))
   '(flymake-errline ((((class color)) (:background "Gray30"))))
   '(flymake-warnline ((((class color)) (:background "Gray20"))))))

;; font scaling sefw
(use-package default-text-scale
  :ensure t
  :config
  (global-set-key (kbd "C-M-<f12>") 'default-text-scale-increase)
  (global-set-key (kbd "C-M-<f11>") 'default-text-scale-decrease))

;; Подсветка текущей строки курсора - пока ненужно.
;; Некоторые темы ее отрисовывaют неадекватно
;; ----------------------------------------------------
;; Highligh current line
;; hl-line is awesome! It’s not very awesome in the terminal version of emacs though, so we don’t use that. Besides, it’s only used for programming.
;; (when window-system
;;   (add-hook 'prog-mode-hook 'hl-line-mode))

;; (use-package hl-line
;;   :ensure nil
;;   :config
;;   (global-hl-line-mode 1))

;; Pretty symbols
;; Changes lambda to an actual symbol and a few others as well, only in the GUI version though.
;; ligatures fonts
;; (global-prettify-symbols-mode t)
;;
;; (when window-system
;;   (use-package pretty-mode
;;     :ensure t
;;     :config
;;     (global-pretty-mode t)))

;; (use-package leuven-theme
;;   :ensure t)

(use-package solarized-theme
  :if (display-graphic-p)
  :custom (solarized-use-variable-pitch nil))

(use-package cyberpunk-theme
  :ensure t)

(use-package apropospriate-theme
  :ensure t
  :config
  ;;(load-theme 'apropospriate-dark t)
  ;; or
  ;;(load-theme 'apropospriate-light t)
  )

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  ;;:config
  ;;(load-theme 'sanityinc-tomorrow-night t)
  )

(use-package doom-themes
  :ensure t)

(use-package gruvbox-theme
  :ensure t
  ;;:config
  ;;(load-theme 'gruvbox-light-medium t)
  )

(use-package monokai-theme
  :ensure t)

(use-package zenburn-theme
  :ensure t
  :config
  ;; (load-theme 'zenburn t)
  )

(use-package modus-themes
  :ensure t
  :config
  (load-theme 'modus-vivendi t))

(set-cursor-color "white")

(use-package all-the-icons
   :ensure t)

;; END Themes
;;..............................................................................

;;------------------------------------------------------------------------------
;; BEGIN: Modeline
;; tag: <modeline>
;; description:
;; The modeline is the heart of emacs, it offers information at all times, it’s persistent and verbose enough to gain a full understanding of modes and states you are in.
;; Due to the fact that we attempt to use emacs as a desktop environment replacement, and external bar showing the time, the battery percentage and more system info would be great to have. I have however abandoned polybar in favor of a heavily modified modeline, this offers me more space on the screen and better integration.
;; One modeline-related setting that is missing and is instead placed at the bottom is diminish.
;;------------------------------------------------------------------------------

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-buffer-file-name-style 'truncate-with-project)
  (doom-modeline-icon nil) ;; slow work
  (doom-modeline-major-mode-icon nil)
  (doom-modeline-minor-modes nil)
  :hook
  (after-init . doom-modeline-mode)
  :config
  ;;(setq doom-modeline-height 0)
  ;;(set-face-attribute 'mode-line nil :family "Iosevka Slab" :height 130)
  ;;(set-face-attribute 'mode-line-inactive nil :family "Iosevka Slab" :height 130)
  (doom-modeline-def-modeline 'main
    '(bar matches buffer-info remote-host buffer-position parrot selection-info)
    '(;;misc-info
      persp-name lsp github debug minor-modes input-method major-mode process vcs)))

;; No separator!
(setq powerline-default-separator nil)

;; Cursor position
;; Show the current line and column for your cursor.
;; We are not going to have relative-linum-mode in every major mode, so this is useful.

(setq line-number-mode t)
(setq column-number-mode t)

;; Clock
;; If you prefer the 12hr-format, change the variable to nil instead of t.
;; Time format
(setq display-time-24hr-format t)
(setq display-time-format "%H:%M - %d %B %Y")
;;Enabling the mode
(display-time-mode -1)

;; END Modeline
;;..............................................................................

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

;; END files
;;..............................................................................

;;------------------------------------------------------------------------------
;; BEGIN: Russian lang
;; tag: <russian lang>
;; description: Великий и Могучий
;;------------------------------------------------------------------------------

(use-package reverse-im
  :config
  ;;(add-to-list 'load-path "~/.xkb/contrib")
  (add-to-list 'reverse-im-modifiers 'super)
  (add-to-list 'reverse-im-input-methods
               (if (require 'unipunct nil t)
                   "russian-unipunct"
                 "russian-computer"))
  (reverse-im-mode t))

;; END Russian lang
;;..............................................................................

;;; END common configuration
;;;..................................................................................................

;;;**************************************************************************************************
;;;* BEGIN Cursor and edit fetures
;;;* tag: <cursor edit>
;;;*
;;;* description: Фичи для редактирования текста
;;;*
;;;**************************************************************************************************

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

;;; END Cursor and edit fetures
;;;..................................................................................................

;;;**************************************************************************************************
;;;* BEGIN Buffer
;;;* tag: <buffer>
;;;*
;;;* description: Управление буферами
;;;*
;;;**************************************************************************************************

(use-package ibuffer
  :ensure nil
  :bind
  ([remap list-buffers] . ibuffer))


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

;;; END Buffer
;;;..................................................................................................


;;;**************************************************************************************************
;;;* BEGIN Autocomplete
;;;* tag: <autocomplete>
;;;*
;;;* description: Автодополнение
;;;*
;;;**************************************************************************************************

(use-package company
  :hook
  (after-init . global-company-mode))

;;; END Autocomplete
;;;..................................................................................................

;;;**************************************************************************************************
;;;* BEGIN FlyCheck
;;;* tag: <flycheck>
;;;*
;;;* description: Проверка на лету
;;;*
;;;**************************************************************************************************

(use-package flycheck
  :ensure t
  :custom
  (flycheck-display-errors-delay 2))

;; Fly-check красит mode-line в желтый цвет - неочень хорошо
;; (use-package flycheck-color-mode-line
;;   :ensure t
;;   :config
;;   (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

;;; END FlyCheck
;;;..................................................................................................


;;;**************************************************************************************************
;;;* BEGIN Spell checking
;;;* tag: <spell>
;;;*
;;;* description: Проверка правописания
;;;*
;;;**************************************************************************************************

(use-package ispell
  :defer t
  :ensure nil)

;; (use-package flyspell
;;   :defer t
;;   :ensure nil
;;   :hook ((text-mode . flyspell-mode)
;;          (prog-mode . flyspell-prog-mode))
;;   :custom
;;   (flyspell-delay 4)
;;   ;;:bind ("C-x ;" . flyspell-auto-correct-previous-word)
;;   :init
;;   ;; (progn
;;   ;;   ;; Below variables need to be set before `flyspell' is loaded.
;;   ;;   (setq flyspell-use-meta-tab nil)
;;   ;;   ;; Binding for `flyspell-auto-correct-previous-word'.
;;   ;;   (setq flyspell-auto-correct-binding (kbd "<S-f12>")))
;;   )

;;; END Spell checking
;;;..................................................................................................


;;;**************************************************************************************************
;;;* BEGIN TODO
;;;* tag: <todo>
;;;*
;;;* description: Списки что надо сделать
;;;*
;;;**************************************************************************************************

(use-package fic-mode
  :commands fic-mode
  :ensure t
  :init (add-hook 'prog-mode-hook 'fic-mode)
  :config

  (defun fic-view-listing ()
    "Use occur to list related FIXME keywords"
    (interactive)
    (occur "\\<\\(FIXME\\|WRITEME\\|WRITEME!\\|TODO\\|BUG\\):?")))

;;; END TODO
;;;..................................................................................................

;;;**************************************************************************************************
;;;* BEGIN Helm
;;;* tag: <helm>
;;;*
;;;* description: Штурвал
;;;*
;;;**************************************************************************************************

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

;; (use-package helm-themes
;;   :ensure t)

;;; END Helm
;;;..................................................................................................

;;;**************************************************************************************************
;;;* BEGIN Search files and strings
;;;* tag: <search files and strings>
;;;*
;;;* description: Инструменты поиска
;;;*
;;;**************************************************************************************************

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

;;; END Search files and strings
;;;..................................................................................................


;;;**************************************************************************************************
;;;* BEGIN Project and perspective
;;;* tag: <project and perspective>
;;;*
;;;* description: Управление проектом и перспектвами
;;;*
;;;**************************************************************************************************

;; (use-package helm-projectile
;;   :ensure t
;;   :config
;;   :bind (("C-c p p" . helm-projectile-switch-project)
;;          ("C-c p f" . helm-projectile-find-file)
;;          ("C-c p k" . helm-projectile-find-file-in-known-projects)
;;          ("C-c p s" . helm-projectile-ag)
;;          ("C-c p g" . helm-projectile-grep)
;;          ("C-c p b" . helm-projectile-switch-to-buffer)))

(defun projectile-default-project-name-2 (project-root)
  ;; чтобы корректно переключались перспективы вместе с проектом
  (expand-file-name project-root))

;;(expand-file-name "~/emacs.d")

(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "s-p")   'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1)
  (setq projectile-project-search-path '("~/git/"))
  (setq projectile-enable-caching t)
  (setq projectile-completion-system 'helm)
  (setq projectile-file-exists-local-cache-expire (* 60))
  (setq projectile-project-name-function 'projectile-default-project-name-2)

  (add-to-list 'projectile-globally-ignored-files "*.png")
  (add-to-list 'projectile-globally-ignored-files "*.jpg")
  (add-to-list 'projectile-globally-ignored-files "*.zip")
  (setq projectile-require-project-root nil)
  (setq projectile-indexing-method 'native)
  (setq projectile-enable-caching t)
  (setq projectile-globally-ignored-directories
        (append '(
                  ".git"
                  ".svn"
                  "out"
                  "repl"
                  "target"
                  "venv"
                  "node_modules")
                projectile-globally-ignored-directories))
  (setq projectile-globally-ignored-files
        (append '(
                  ".DS_Store"
                  "*.gz"
                  "*.pyc"
                  "*.jar"
                  "*.tar.gz"
                  "*.tgz"
                  "*.zip")
                projectile-globally-ignored-files)))

;; (use-package perspective
;;   :config
;;   (persp-mode))


;; Пока отключил, попробую работать напрямую через tmux
;; (use-package persp-projectile
;;   :ensure t
;;   :defer 1
;;   :bind (("C-c p w" . projectile-persp-switch-project)))

;; Нужно для того чтобы открывать каждый проект в своем фрейме
;; пока не ончень порой удобно, решено оставить на будущее
;;
;; (use-package nameframe
;;   :ensure t)
;; (use-package nameframe-projectile
;;   :ensure t
;;   :config
;;   (nameframe-projectile-mode t))

;; Let projectile call make
;; (global-set-key (kbd "<f5>") 'projectile-compile-project)

;;
;; Todos/projectile
;;
(use-package org-projectile
  :ensure t
  :after org
  :after projectile
  :config
  (setq org-projectile-per-project-filepath "pacmans_todo.org")
  (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
  (global-set-key (kbd "C-c c") 'org-capture)
  (global-set-key (kbd "C-c n p") 'org-projectile-project-todo-completing-read))

;;; END Project and perspective
;;;..................................................................................................

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

;;; END Snippets
;;;..................................................................................................


;;;**************************************************************************************************
;;;* BEGIN Version controls
;;;* tag: <versions control>
;;;*
;;;* description: Контроль версий
;;;*
;;;**************************************************************************************************

;;------------------------------------------------------------------------------
;; BEGIN: Git
;; tag: <git>
;; description: Управление версиями через git
;;------------------------------------------------------------------------------

(use-package magit
  :ensure t)


 (use-package git-gutter
  :ensure t
  :config
  (progn
    (global-git-gutter-mode +1)
    (git-gutter:linum-setup)
    ;; (custom-set-variables
    ;;  '(git-gutter:window-width 2)
    ;;  '(git-gutter:modified-sign "☁")
    ;;  '(git-gutter:added-sign "☀")
    ;;  '(git-gutter:deleted-sign "☂")
    ;;  '(git-gutter:unchanged-sign " ")
    ;;  '(git-gutter:separator-sign "|")
    ;;  '(git-gutter:hide-gutter t))
    ;; (set-face-background 'git-gutter:modified "purple") ;; background color
    ;; (set-face-foreground 'git-gutter:added "green")
    ;; (set-face-foreground 'git-gutter:deleted "red")
    ;; (set-face-background 'git-gutter:unchanged "yellow")
    ;; (set-face-foreground 'git-gutter:separator "yellow")
    ;; (add-to-list 'git-gutter:update-hooks 'focus-in-hook)
    )
  :bind (("C-x C-g" . git-gutter:toggle)
         ("C-x v =" . git-gutter:popup-hunk)
         ("C-x p" . git-gutter:previous-hunk)
         ("C-x n" . git-gutter:next-hunk)
         ("C-x v s" . git-gutter:stage-hunk)
         ("C-x v r" . git-gutter:revert-hunk)))

(use-package git-gutter+
  :ensure t
  :config
  (progn
    (global-git-gutter+-mode +1)
    ;; Jump between hunks
    (define-key git-gutter+-mode-map (kbd "C-x n") 'git-gutter+-next-hunk)
    (define-key git-gutter+-mode-map (kbd "C-x p") 'git-gutter+-previous-hunk)
    ;; Act on hunks
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

;; END Git
;;..............................................................................

;;------------------------------------------------------------------------------
;; BEGIN: Mercurial
;; tag: <mercurial>
;; description:
;;------------------------------------------------------------------------------

;; Monky

(use-package monky :ensure t)

;; Configuration

(use-package hgignore-mode :ensure t)
(use-package hgrc-mode :ensure t)

;; END Mercurial
;;..............................................................................

;;; END Version controls
;;;..................................................................................................

;;;**************************************************************************************************
;;;* BEGIN treemacs
;;;* tag: <treemacs>
;;;*
;;;* description:
;;;*
;;;**************************************************************************************************

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

;; (use-package treemacs-evil
;;   :after (treemacs evil)
;;   :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

;; (use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
;;   :after (treemacs)
;;   :ensure t
;;   :config (treemacs-set-scope-type 'Tabs))

;;; END treemacs
;;;..................................................................................................


;;;**************************************************************************************************
;;;* BEGIN LSP
;;;* tag: <lsp>
;;;*
;;;* description:
;;;*
;;;**************************************************************************************************

;; need to install material design icons!
(use-package lsp-mode
  :commands lsp
  :hook ((python-mode . lsp)
	 (js2-mode . lsp)
	 (js2-jsx-mode . lsp)
	 (rjsx-mode . lsp))
  :custom
  (lsp-javascript-auto-closing-tags t)
  (lsp-javascript-suggest-complete-js-docs t)
  ;; (lsp-headerline-breadcrumb-enable nil)
  :config
  (lsp-enable-which-key-integration t))

(setq lsp-idle-delay 5)
(setq lsp-log-io nil)

;; optionally
;; (use-package lsp-ui
;;   :commands lsp-ui-mode
;;   :config
;;   (setq lsp-treemacs-sync-mode 1)

;;   (setq lsp-ui-peek-enable t)
;;   (setq lsp-ui-doc-header t)
;;   (setq lsp-ui-doc-include-signature t)
;;   (setq lsp-ui-doc-enable t)
;;   (setq lsp-ui-doc-use-webkit t)
;;   (setq lsp-ui-doc-position 'top)
;;   (setq lsp-ui-doc-delay 0)
;;   (setq lsp-ui-doc-show-with-cursor t)
;;   (setq lsp-ui-doc-show-with-mouse t)
;;   (setq lsp-ui-sideline-delay 1) ; don't show sideline immediately
;;   (setq lsp-eldoc-hook nil)

;;   (setq lsp-ui-imenu-window-width 300) set window width
;;   ;;(setq lsp-ui-imenu--custom-mode-line-format ?) mode line format
;;   (setq lsp-ui-imenu-auto-refresh t) auto refresh when necessary
;;   (setq lsp-ui-imenu-refresh-delay 2) delay to refresh imenu

;;   (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
;;   (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

(use-package lsp-ui
  :ensure t
  :init (setq
	      lsp-treemacs-sync-mode           1

              lsp-enable-snippet               nil
              lsp-ui-sideline-enable           t
              lsp-ui-peek-enable               t

	      lsp-ui-doc-enable                t
              lsp-ui-doc-position              'at-point
              lsp-ui-doc-header                nil
              lsp-ui-doc-border                "red"
              lsp-ui-doc-include-signature     t
	      lsp-ui-doc-show-with-cursor      t
	      lsp-ui-doc-show-with-mouse       t

              lsp-ui-sideline-update-mode      'point
              lsp-ui-sideline-delay            5
              lsp-ui-sideline-ignore-duplicate t

              lsp-ui-peek-always-show          t
              lsp-ui-flycheck-enable           t

	      lsp-eldoc-hook                   nil)
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references)
              ("C-c u" . lsp-ui-imenu))
  :config
  (setq lsp-ui-sideline-ignore-duplicate t)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))


;; (use-package company-lsp
;;   :commands company-lsp)

(use-package lsp-treemacs
  :after lsp)

(use-package helm-lsp
  :commands helm-lsp-workspace-symbol)

(setq lsp-language-id-configuration '((java-mode . "java")
                                      (python-mode . "python")
                                      (gfm-view-mode . "markdown")
                                      (rust-mode . "rust")
                                      (css-mode . "css")
                                      (xml-mode . "xml")
                                      (c-mode . "c")
                                      (c++-mode . "cpp")
                                      (objc-mode . "objective-c")
                                      (web-mode . "html")
                                      (html-mode . "html")
                                      (sgml-mode . "html")
                                      (mfhtml-mode . "html")
                                      (go-mode . "go")
                                      (haskell-mode . "haskell")
                                      (php-mode . "php")
                                      (json-mode . "json")
                                      (js2-mode . "javascript")
                                      (js2-jsx-mode . "javascript")
                                      (rjsx-mode . "javascript")
                                      (typescript-mode . "typescript")))

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))

;;; END LSP
;;;..................................................................................................


;;;**************************************************************************************************
;;;* BEGIN Python
;;;* tag: <python>
;;;*
;;;* description: Some definition for python language
;;;*
;;;**************************************************************************************************

;; pip3 install 'python-language-server[all]'
;; pip3 install -U setuptools
;; pip3 install virtualenvwrapper flake8 pep8 importmagic autopep8 yapf nose

;; Надо попробовать этот вариант
;; (use-package lsp-python-ms
;;   :hook (python-mode . lsp))

(use-package virtualenvwrapper
  :ensure t
  :config
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell))

(setq lsp-python-executable-cmd "python3")

(setq python-shell-interpreter "python3"
      python-shell-interpreter-args "-i")


;;; END Python
;;;..................................................................................................


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

;;; END Clojure
;;;..................................................................................................

;;;**************************************************************************************************
;;;* BEGIN HTML and CSS
;;;* tag: <web html css>
;;;*
;;;* description: CSS
;;;*
;;;**************************************************************************************************

(use-package web-mode
  :ensure t
  :mode
  ("\\.phtml\\'" "\\.tpl\\.php\\'" "\\.[agj]sp\\'" "\\.as[cp]x\\'"
   "\\.erb\\'" "\\.mustache\\'" "\\.djhtml\\'" "\\.html?\\'" "\\.ejs\\'")
  :init
  (setq web-mode-markup-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-enable-auto-pairing t
        web-mode-enable-auto-expanding t
        web-mode-enable-css-colorization t)
  :config
  ;; Template
  (setq web-mode-engines-alist
        '(("php"    . "\\.phtml\\'")
          ("blade"  . "\\.blade\\."))))

(use-package web-beautify
  :ensure t
  :commands (web-beautify-css
             web-beautify-css-buffer
             web-beautify-html
             web-beautify-html-buffer
             web-beautify-js
             web-beautify-js-buffer))

(use-package web-completion-data :ensure t)
(use-package web-mode-edit-element :ensure t)

;; (use-package sass-mode
;;   :ensure t)

(use-package scss-mode
  :ensure t)

(use-package emmet-mode
  :ensure t
  :bind (:map emmet-mode-keymap
              ("M-e" . emmet-expand-line))
  :config (add-hook 'web-mode-hook 'emmet-mode))


;;; END HTML and CSS
;;;..................................................................................................


;;;**************************************************************************************************
;;;* BEGIN Nginx
;;;* tag: <nginx>
;;;*
;;;* description: nginx server
;;;*
;;;**************************************************************************************************

(use-package nginx-mode
  :ensure t)

;;; END Nginx
;;;..................................................................................................

;;;**************************************************************************************************
;;;* BEGIN Javascript and json
;;;* tag: <javascript json>
;;;*
;;;* description: Язык программирования JavaScript и формат JSON
;;;*
;;;**************************************************************************************************

;;------------------------------------------------------------------------------
;; BEGIN: JavaScript
;; tag: <javascript>
;; description: Язык программирвоания JavaScript
;;------------------------------------------------------------------------------

(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :config
  ;; Turn off js2 mode errors & warnings (we lean on eslint/standard)
  ;;(setq js2-mode-show-parse-errors nil)
  ;;(setq js2-mode-show-strict-warnings nil)
  (setq js2-strict-missing-semi-warning nil)
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(js2-error ((t (:background "#110000" :box nil)))))
  :hook ((js2-mode . lsp))
  :bind (:map js2-mode-map
              ("M-." . lsp-find-definition)))


(use-package rjsx-mode
  :after js2-mode
  :ensure t
  :mode (("\\.jsx$" . rjsx-mode)
         ("components/.+\\.js$" . rjsx-mode))
  :config
  ;; Turn off js2 mode errors & warnings (we lean on eslint/standard)
  ;;(setq js2-mode-show-parse-errors nil)
  ;;(setq js2-mode-show-strict-warnings nil)
  (setq js2-strict-missing-semi-warning nil)
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(js2-error ((t (:background "#110000" :box nil)))))
  :hook (rjsx-mode . lsp))

;; before
;; npm i -g prettier

;; {
;;     "singleQuote": true,
;;     "semi": false,
;;     "tabWidth": 4
;; }

(use-package prettier-js
  ;; Данные форматирования всегда берем из .prettierrc файлов
  ;; :config
  ;; (setq prettier-js-args '(
  ;;                          ;;"--trailing-comma" "es5"
  ;;                          "--single-quote" "true"
  ;;                          ;;"--print-width" "120"
  ;;                          ;;"--arrow-parens" "always"
  ;;                          ;;"--use-tabs" "false"
  ;;                          "--semi" "false"
  ;;                          ))
  ;;
  :hook ((js2-mode . prettier-js-mode)
	 (js2-jsx-mode . prettier-js-mode)
         (rjsx-mode . prettier-js-mode)
	 (typescript-mode . prettier-js-mode)))


(use-package js-doc
  ;; :bind (:map js2-mode-map
  ;;            ("C-c i" . js-doc-insert-function-doc)
  ;;            ("@" . js-doc-insert-tag))
  :config
  (setq js-doc-mail-address "t34box@gmail.com"
        js-doc-author (format "Pavel Kopychenko <%s>" js-doc-mail-address)
        js-doc-url "https://github.com/povloid"
        js-doc-license "MIT License"))

(use-package typescript-mode
  :mode (("\\.ts$" . typescript-mode)
	 ("\\.tsx$" . typescript-mode))
  :hook (typescript-mode . lsp)
  :config
  (setq  typescript-indent-level 2))

;; END JavaScript
;;..............................................................................

;;------------------------------------------------------------------------------
;; BEGIN: JSON
;; tag: <json>
;; description: Формат JSON
;;------------------------------------------------------------------------------

(use-package json-mode
  :ensure t
  :mode ("\\.json$" . json-mode)
  :init
  (progn
    (add-hook 'json-mode
              (lambda ()
                (make-local-variable 'js-indent-level)
                (setq js-indent-level 2)))))

(use-package json-reformat
  :ensure t
  :commands json-reformat-region
  :init (progn (setq json-reformat:indent-width 2)
               (setq json-reformat:pretty-string? t)))

;; END JSON
;;..............................................................................

;;------------------------------------------------------------------------------
;; BEGIN: Yaml
;; tag: <yaml>
;; description: Формат YAML
;;------------------------------------------------------------------------------

(use-package yaml-mode
  :ensure t
  :commands (yaml-mode)
  :mode "\\.yml\\'")

(use-package yaml-tomato
  :ensure t)
(setq lsp-idle-delay 2)
(setq lsp-log-io nil)
(setq lsp-idle-delay 2)
(setq lsp-log-io nil)
(setq lsp-idle-delay 2)
(setq lsp-log-io nil)

;; END Yaml
;;..............................................................................

;;; END Javascript and json
;;;..................................................................................................

;;;**************************************************************************************************
;;;* BEGIN GraphQL
;;;* tag: <graphql>
;;;*
;;;* description: Настройки для graphql
;;;*
;;;**************************************************************************************************

(use-package graphql-mode
  :ensure t
  :commands (graphql-mode)
  :mode "\\.graphql\\'")

;;; END GraphQL
;;;..................................................................................................


;;;**************************************************************************************************
;;;* BEGIN EDIT CONFIGS
;;;* tag: <configw>
;;;*
;;;* description: Configuration and log files
;;;*
;;;**************************************************************************************************

;; Default unix configuration
;; Config-general-mode is applied for all unix configuration files.

(use-package config-general-mode
  :ensure t
  :mode ("\\.conf$" "\\.*rc$"))

;; Apache

(use-package apache-mode
  :ensure t
  :mode ("\\.htaccess\\'" "httpd\\.conf\\'" "srm\\.conf\\'"
         "access\\.conf\\'" "sites-\\(available\\|enabled\\)/"))

;; SSH configuration

(use-package ssh-config-mode
  :ensure t
  :mode ("/\\.ssh/config\\'" "/system/ssh\\'" "/sshd?_config\\'" "/known_hosts\\'" "/authorized_keys2?\\'")
  :hook (ssh-config-mode . turn-on-font-lock)

  :config
  (autoload 'ssh-config-mode "ssh-config-mode" t))

;; Logview

(use-package logview
  :ensure t
  :mode ("syslog\\(?:\\.[0-9]+\\)" "\\.log\\(?:\\.[0-9]+\\)?\\'"))

;; пример как редактировать через ssh и sudo
;; /ssh:ubuntu@ec2-54-234-199-41.compute-1.amazonaws.com|sudo:remotehost:/etc/hosts

;;; END EDIT CONFIGS
;;;..................................................................................................

;;;**************************************************************************************************
;;;* BEGIN Graphviz
;;;* tag: <graphviz>
;;;*
;;;* description: Графики
;;;*
;;;**************************************************************************************************

(use-package graphviz-dot-mode
  :ensure t
  :init
  (defvar default-tab-width nil)

  :mode ("\\.dot\\'"))

;;; END Graphviz
;;;..................................................................................................


;;;**************************************************************************************************
;;;* BEGIN CSV
;;;* tag: <csv>
;;;*
;;;* description: Формат CSV
;;;*
;;;**************************************************************************************************

(use-package csv-mode
  :ensure t
  :defer t
  :config

  ;; Define separators
  (setq csv-separators '("," ";" ":" " ")))


;; Subpackages
(use-package csv-nav
  :ensure t
  :disabled t)

;;; END CSV
;;;..................................................................................................

;;;**************************************************************************************************
;;;* BEGIN Erlang/OTP
;;;* tag: <erlang otp>
;;;*
;;;* description: Язык программирования и платформа Erlang/OTP
;;;*
;;;**************************************************************************************************

(use-package erlang
  :ensure t
  :config
  (when (eq system-type 'windows-nt)
    ;; TODO: надо сделать условие от операционной системы
    (setq erlang-root-dir "C:/Program Files/erl7.2")
    (add-to-list 'exec-path "C:/Program Files/erl7.2/bin")))

(message "[ok]\tLoaded Erlang Modules")

;;; END Erlang
;;;..................................................................................................

;;;**************************************************************************************************
;;;* BEGIN Markdown
;;;* tag: <markdown>
;;;*
;;;* description: формат Markdown
;;;*
;;;**************************************************************************************************

(use-package markdown-mode
  :ensure t
  :mode ("\\.md$"))

;; (use-package markdown-mode+
;;   :ensure t
;;   :after markdown-mode
;;   :defer t)

(message "[ok]\tLoaded Markdown Modules")

;;; END Markdown
;;;..................................................................................................

;;;**************************************************************************************************
;;;* BEGIN SQL
;;;* tag: <sql>
;;;*
;;;* description: Реляционные базы данных
;;;*
;;;**************************************************************************************************

(use-package sql-indent
  :ensure t)

;; sudo gem install anbt-sql-formatter
;; or
;; brew install pgformatter
;; or
;; npm i -g sql-formatter-cli
;; or
;; sqlparse
;; https://github.com/andialbrecht/sqlparse
;; pip install sqlparse
;; pip3 install sqlparse <-- переустановка в случае проблем
;; It is a Python module that installs the command sqlformat. Usage is simple, e.g.:
;; sqlformat --reindent --keywords upper --identifiers lower my_file.sql
;; Use "-" as FILE to read from stdin

(defun sql-beautify-region (begin end)
  "Beautify SQL in region from BEGIN to END."
  (interactive "r")
  (save-excursion
    ;;(shell-command-on-region begin end "anbt-sql-formatter" nil t)
    (shell-command-on-region begin end "pg_format -t -s 2 -u 2 -U 2" nil t)
    ;;(shell-command-on-region begin end "sql-formatter-cli -s \"pl/sql\" " nil t)
    ;;(shell-command-on-region begin end "sqlformat -r -s -a --keywords upper --identifiers lower --comma_first true -" nil t)
    ))

;; change sqlbeautify to anbt-sql-formatter if you
;;ended up using the ruby gem

(defun sql-beautify-buffer ()
  "Beautify SQL in buffer."
  (interactive)
  (sql-beautify-region (point-min) (point-max)))

(use-package sql
  :defer t
  :config
  (setq sql-postgres-program "/Applications/Postgres.app/Contents/Versions/latest/bin/psql"))

(message "[ok]\tLoaded SQL Modules")

;;; END SQL
;;;..................................................................................................

;;;**************************************************************************************************
;;;* BEGIN DOCKER
;;;* tag: <docker>
;;;*
;;;* description: Поддрежка doker
;;;*
;;;**************************************************************************************************

;;(use-package docker :ensure t)
;;(use-package docker-api :ensure t)
;;(use-package dockerfile-mode :ensure t)

;;; END DOCKER
;;;..................................................................................................

;;;**************************************************************************************************
;;;* BEGIN SSH
;;;* tag: <ssh>
;;;*
;;;* description:
;;;*
;;;**************************************************************************************************

(use-package ssh :ensure t)
(use-package ssh-deploy :ensure t)

(message "[ok]\tLoaded SSH Modules")

;;; END SSH
;;;..................................................................................................

;;;**************************************************************************************************
;;;* BEGIN SEARCHING
;;;* tag: <searching>
;;;*
;;;* description: Searching and translate
;;;*
;;;**************************************************************************************************

;;----------------------------------------------------------
;; BEGIN: Google
;; tag: <google>
;; description:
;;----------------------------------------------------------

(use-package google :ensure t)
(use-package google-maps :ensure t)
(use-package google-translate
  :ensure t
  :init
  (setq google-translate-default-source-language "en")
  (setq google-translate-default-target-language "ru")
  (setq google-translate-translation-directions-alist '(("en" . "ru")))
  :bind ("C-`" . google-translate-smooth-translate))

(message "[ok]\tLoaded Google Modules")

;; END Google
;;..........................................................

;; END Codesearch
;;..........................................................


;;; END SEARCHING
;;;..................................................................................................

;;;**************************************************************************************************
;;;* BEGIN C/C++
;;;* tag: <c c++>
;;;*
;;;* description: С/C++
;;;*
;;;**************************************************************************************************

;; brew install ccls

;; cmake -H. -BDebug -DCMAKE_BUILD_TYPE=Debug -DCMAKE_EXPORT_COMPILE_COMMANDS=YES
;; ln -s Debug/compile_commands.json .

(use-package ccls
  :ensure t
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda () (require 'ccls) (lsp))))

(use-package cmake-mode
  :mode (("/CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)))

(use-package clang-format
  :defines (clang-format-fallback-style)
  :after (cc-mode)
  :config
  (set-default 'clang-format-fallback-style "Google")
  ;; (add-hook 'c-mode-common-hook #'(lambda()
  ;;                                   (add-hook 'before-save-hook
  ;;                                             'clang-format-buffer t t)))
  )


(use-package clang-format+
  :ensure t
  :defer t
  :hook ((c-mode c++-mode objc-mode) . #'clang-format+-mode))


(use-package modern-cpp-font-lock :ensure t)

(use-package cmake-mode
  :ensure t
  :mode
  ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

(use-package cmake-font-lock
  :ensure t
  :after (cmake-mode)
  :hook (cmake-mode . cmake-font-lock-activate))

(message "[ok]\tLoaded C/C++ Modules")

;;; END C/C++
;;;..................................................................................................

;;;**************************************************************************************************
;;;* BEGIN JAVA
;;;* tag: <java>
;;;*
;;;* description: Язык и платформа JAVA
;;;*
;;;**************************************************************************************************

;; Global (lsp-java)

(use-package lsp-java
  :config
  (add-hook 'java-mode-hook 'lsp)
  (setq lsp-java-format-enabled t)
  (setq lsp-java-format-settings-url "https://github.com/google/styleguide/blob/gh-pages/eclipse-java-google-style.xml"))

(use-package dap-mode
  :after lsp-mode
  :config
  (dap-auto-configure-mode))

(use-package dap-java :ensure nil)

;; Snippets

(use-package java-snippets
  :ensure t)

;; Javadoc

(use-package javadoc-lookup
  :ensure t
  :config
  (global-set-key (kbd "C-h j") 'javadoc-lookup)
  ;;(javadoc-add-roots "/opt/java/jdk/api"
  ;;		     "~/git/inko-med/projects...")

  (javadoc-add-artifacts [org.lwjgl.lwjg lwjgl "2.8.2"]
                       [com.nullprogram native-guide "0.2"]
                       [org.apache.commons commons-math3 "3.0"])
  )

;; Groovy

(use-package groovy-mode
  :ensure t
  :mode ("\.groovy$" "\.gradle$")
  :interpreter ("gradle" "groovy")
  :config
  (autoload 'run-groovy "inf-groovy" "Run an inferior Groovy process")
  (autoload 'inf-groovy-keys "inf-groovy" "Set local key defs for inf-groovy in groovy-mode")

  ;; Some keys for
  (add-hook 'groovy-mode-hook
            #'(lambda ()
               (inf-groovy-keys))))

;; Subpackages
(use-package groovy-imports :ensure t)

(use-package flycheck-gradle
  :ensure t
  :defer t)

(message "[ok]\tLoaded Java Modules")

;;; END JAVA
;;;..................................................................................................

;;;**************************************************************************************************
;;;* BEGIN Productivity
;;;* tag: <productivity>
;;;*
;;;* description:
;;;*
;;;**************************************************************************************************

;; Dark room
;; Sometimes I just want to ignore everything except the current document. Darkroom is exactly designed for that
(use-package darkroom :ensure t)

;; ORG/Web tools
;; Org-web-tools is a nice package which allows to sniff a webpage and convert it into org-mode format. This is really useful to homogeneise documentation.

(use-package org-web-tools :ensure t)


(message "[ok]\tLoaded Productivity Modules")

;;; END Productivity
;;;..................................................................................................

;;;**************************************************************************************************
;;;* BEGIN  Wick key mode
;;;* tag: <>
;;;*
;;;* description:  which-key and why I love emacs
;;;*
;;;* In order to use emacs, you don’t need to know how to use emacs. It’s self documenting,
;;;* and coupled with this insanely useful package, it’s even easier. In short, after you start the
;;;* input of a command and stop, pondering what key must follow, it will automatically open
;;;* a non-intrusive buffer at the bottom of the screen offering you suggestions for completing
;;;* the command, that’s it, nothing else.
;;;* It’s beautiful
;;;**************************************************************************************************

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(message "[ok]\tLoaded Wich-Key Modules")

;;; END
;;;..................................................................................................

;;;**************************************************************************************************
;;;* BEGIN Windows
;;;* tag: <window>
;;;*
;;;* description:
;;;*
;;;**************************************************************************************************

(use-package switch-window
  :ensure t
  :config
  (setq switch-window-input-style 'minibuffer)
  (setq switch-window-increase 4)
  (setq switch-window-threshold 2)
  (setq switch-window-shortcut-style 'qwerty)
  (setq switch-window-qwerty-shortcuts
        '("a" "s" "d" "f" "j" "k" "l" "i" "o"))
  (setq switch-window-auto-resize-window nil)
  (setq switch-window-default-window-size 0.6)
  :bind
  ([remap other-window] . switch-window))

;; Following window splits
;; After you split a window, your focus remains in the previous one.
;; This annoyed me so much I wrote these two, they take care of it.

(defun split-and-follow-horizontally ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)

(defun split-and-follow-vertically ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)

(message "[ok]\tLoaded Windows Modules")

;;; END Windows
;;;..................................................................................................

;;;**************************************************************************************************
;;;* BEGIN Minor conveniences
;;;* tag: <minor conveniences>
;;;*
;;;* description:
;;;*
;;;**************************************************************************************************

;; Subwords
;; Emacs treats camelCase strings as a single word by default, this changes said behaviour.

(global-subword-mode 1)

;; Electric
;; If you write any code, you may enjoy this. Typing the first character in a set of 2,
;; completes the second one after your cursor. Opening a bracket? It’s closed for you already.
;; Quoting something? It’s closed for you already.
;; You can easily add and remove pairs yourself, have a look.

(setq electric-pair-pairs '(
                            (?\{ . ?\})
                            (?\( . ?\))
                            (?\[ . ?\])
                            (?\" . ?\")
                            ))
;; отключил, очень мешает порой редактировать код
(electric-pair-mode -1)

;; Beacon
;; While changing buffers or workspaces, the first thing you do is look for your cursor.
;; Unless you know its position, you can not move it efficiently.
;; Every time you change buffers, the current position of your cursor will be briefly highlighted now.
;; Подсветка перемещения курсора

;; (use-package beacon
;;   :ensure t
;;   :config
;;   (beacon-mode 1))

;; Expand region
;; A pretty simple package, takes your cursor and semantically expands the region, so words,
;; sentences, maybe the contents of some parentheses, it’s awesome, try it out.

(use-package expand-region
  :ensure t
  :bind ("C-q" . er/expand-region))

;; Hungry deletion
;; On the list of things I like doing, deleting big whitespaces is pretty close to the bottom.
;; Backspace or Delete will get rid of all whitespace until the next non-whitespace character is encountered.
;; You may not like it, thus disable it if you must, but it’s pretty decent.

;; не очень удобно для меня
;; (use-package hungry-delete
;;   :ensure t
;;   :config
;;   (global-hungry-delete-mode))

(message "[ok]\tLoaded  Minor Conveniences Modules")

;;; END Minor conveniences
;;;..................................................................................................

;;;**************************************************************************************************
;;;* BEGIN Kill ring
;;;* tag: <kill ting>
;;;*
;;;* description:
;;;*
;;;**************************************************************************************************

;; There is a lot of customization to the kill ring, and while I have not used it much before, I decided that it was time to change that.

;; Maximum entries on the ring
;; The default is 60, I personally need more sometimes.

(setq kill-ring-max 100)

;; popup-kill-ring
;; Out of all the packages I tried out, this one, being the simplest, appealed to me most.
;; With a simple M-y you can now browse your kill-ring like browsing autocompletion items. C-n and C-p totally work for this.

(use-package popup-kill-ring
  :ensure t
  :bind ("M-y" . popup-kill-ring))

(message "[ok]\tLoaded  Kill ring Modules")

;;; END Kill ring
;;;..................................................................................................


;;;**************************************************************************************************
;;;* BEGIN Org
;;;* tag: <org mode>
;;;*
;;;* description: One of the absolute greatest features of emacs is called “org-mode”.
;;;* This very file has been written in org-mode, a lot of other configurations are written in org-mode,
;;;* same goes for academic papers, presentations, schedules, blogposts and guides.
;;;* Org-mode is one of the most complex things ever, lets make it a bit more usable with some basic configuration.
;;;* Those are all rather self-explanatory.
;;;*
;;;**************************************************************************************************

;; Common settings
(setq org-ellipsis " ")
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-confirm-babel-evaluate nil)
(setq org-export-with-smart-quotes t)
(setq org-src-window-setup 'current-window)
(setq org-hide-emphasis-markers t) ;; скрыть спецсимволы для форматирования текста
(add-hook 'org-mode-hook 'org-indent-mode)

;; Syntax highlighting for documents exported to HTML
(use-package htmlize
  :ensure t)

;; Line wrapping
(add-hook 'org-mode-hook
          #'(lambda ()
             (visual-line-mode 1)))



;; Keybindings

(global-set-key (kbd "C-c '") 'org-edit-src-code)

;; Org Bullets
;; Makes it all look a bit nicer, I hate looking at asterisks.

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode))))

;; Easy-to-add emacs-lisp template
;; Hitting tab after an “<el” in an org-mode file will create a template for elisp insertion.

(add-to-list 'org-structure-template-alist
             '("el" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC"))


(message "[ok]\tLoaded Org Modules")

;;; END Org
;;;..................................................................................................

;;;**************************************************************************************************
;;;* BEGIN Training
;;;* tag: <training>
;;;*
;;;* description: Тренировочные программы
;;;*
;;;**************************************************************************************************

(use-package speed-type
  :ensure t)

(message "[ok]\tLoaded Training Modules")

;;; END Traning
;;;..................................................................................................

;;;**************************************************************************************************
;;;* BEGIN My definition
;;;* tag: <my definition>
;;;*
;;;* description: Мои определения
;;;*
;;;**************************************************************************************************

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

(global-set-key [f2] 'magit-push-current-to-upstream)
;;(global-set-key [f4] 'buffer-format)
(global-set-key [f4] 'lsp-format-buffer)
(global-set-key [f5] 'toggle-truncate-lines)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "C-M-h") 'backward-kill-word)

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

;; -----------------------------------------------------------------------------
;; - (kill-ring-save BEG END &optional REGION)

(when (eq system-type 'darwin)
  (progn
    (defun pbcopy ()
      (interactive)
      (call-process-region (point) (mark) "pbcopy")
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


(message "[ok]\tLoaded My Definitions Modules")

;; END
;;..............................................................................

;;;**************************************************************************************************
;;;* BEGIN Eshell
;;;* tag: <eshell>
;;;*
;;;* description:
;;;*
;;;**************************************************************************************************

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))


;; (use-package fish-completion
;;   :ensure t
;;   :config
;;   (global-fish-completion-mode))

;; (use-package eshell-prompt-extras
;; :ensure t
;; :config
;; (setq epe-show-python-info nil)
;; )

(use-package eshell-git-prompt
  :ensure t
  :config
  (eshell-git-prompt-use-theme 'git-radar))

(setq scroll-step 1)

(message "[ok]\tLoaded Eshell Modules")

;;; END Eshell
;;;..................................................................................................
;;------------------------------------------------------------------------------
;; BEGIN: VTerm
;; tag: <>
;; description:
;;------------------------------------------------------------------------------

;; ненужно, тепеть используем tmux
;; (use-package vterm
;;   :ensure t)

;; END VTerm
;;..............................................................................




;;------------------------------------------------------------------------------
;; BEGIN: TTT Terminal
;; tag: <ansiterm ttt>
;; description: Терминал на основе ansi-term
;;------------------------------------------------------------------------------

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
  (ansi-term "/bin/zsh")
  (rename-buffer (concat "ttt: " name))
  ;;(term-send-raw-string "export LANG=en_US.UTF-8; source ~/.bash_profile")
  (term-send-raw-string "source ~/.zprofile")
  (term-send-input))

(defun ttt (name)
  (interactive "sbuffername:")
  (tt-0 name))

(defun tti ()
  (interactive)
  (tt-0 (get-tts-id)))

(message "[ok]\tLoaded TTT Terminal Modules")

;; END TTT Terminal
;;..............................................................................


;;;**************************************************************************************************
;;;* BEGIN Jira
;;;* tag: <jira>
;;;*
;;;* description: Jira integration
;;;*
;;;**************************************************************************************************
;;; "https://overtok.atlassian.net/"

;; (use-package dash-functional
;;   :ensure t)

;; (use-package jiralib2
;;   :ensure t)

;; (use-package language-detection
;;   :ensure t)

;; (use-package ejira
;;   :load-path ("~/ejira")
;;   :init
;;   (setq jiralib2-url "https://overtok.atlassian.net/"
;;         jiralib2-auth 'basic
;;         jiralib2-user-login-name "pavelk@overtok.com"
;;         jiralib2-token nil
;;         ejira-org-directory "~/jira"
;;         ejira-projects '("OT")

;;         ;; Configure JIRA priorities
;;         ejira-priorities-alist '(("Highest" . ?A)
;;                                  ("High"    . ?B)
;;                                  ("Medium"  . ?C)
;;                                  ("Low"     . ?D)
;;                                  ("Lowest"  . ?E))

;;         ;; Map JIRA states to org states.
;;         ejira-todo-states-alist '(("Unscheduled" . 1)
;;                                   ("Groomed" . 2)
;;                                   ("Ready For Development" . 3)
;;                                   ("In Development" . 4)
;;                                   ("Ready For Review" . 5)
;;                                   ("Ready For Deploy" . 6)
;;                                   ("Done" . 7))

;;         ;; Set the highest/lowest org priorities
;;         org-priority-highest ?A
;;         org-priority-lowest ?E
;;         org-todo-keywords
;;         '((sequence
;;            "UNSCHEDULED(u)"
;;            "GROOMED(g)"
;;            "READY-FOR-DEVELOPMENT(r)"
;;            "IN-DEVELOPMENT(i)"
;;            "READY-FOR-REVIEW(v)"
;;            "READY-FOR-DEPLOY(p)"
;;            "|"
;;            "DONE(d)")))
;;   :config
;;   ;; Tries to auto-set custom fields by looking into /editmeta
;;   ;; of an issue and an epic.
;;   ;;; (add-hook 'jiralib2-post-login-hook #'ejira-guess-epic-sprint-fields)

;;   (setq ejira-update-jql-unresolved-fn #'ejira-jql-my-unresolved-project-tickets)

;;   ;; They can also be set manually if autoconfigure is not used.
;;   ;; (setq ejira-sprint-field       'customfield_10001
;;   ;;       ejira-epic-field         'customfield_10002
;;   ;;       ejira-epic-summary-field 'customfield_10004)

;;   (require 'ejira-agenda)

;;   ;; Make the issues visisble in your agenda by adding `ejira-org-directory'
;;   ;; into your `org-agenda-files'.
;;   (add-to-list 'org-agenda-files ejira-org-directory)

;;   ;; Add an agenda view to browse the issues that
;;   (org-add-agenda-custom-command
;;    '("j" "My JIRA issues"
;;      ((ejira-jql "resolution = unresolved and assignee = currentUser()"
;;                  ((org-agenda-overriding-header "Assigned to me")))))))


;; (message "[ok]\tLoaded Jira Modules")

;;; END Jira
;;;..................................................................................................


;;;**************************************************************************************************
;;;* BEGIN UUID
;;;* tag: <>
;;;*
;;;* description:
;;;*
;;;**************************************************************************************************

;; ID: 90aebf38-b33a-314b-1198-c9bffea2f2a2
(defun uuid-create ()
  "Return a newly generated UUID. This uses a simple hashing of variable data."
  (let ((s (md5 (format "%s%s%s%s%s%s%s%s%s%s"
                        (user-uid)
                        (emacs-pid)
                        (system-name)
                        (user-full-name)
                        user-mail-address
                        (current-time)
                        (emacs-uptime)
                        (garbage-collect)
                        (random)
                        (recent-keys)))))
    (format "%s-%s-3%s-%s-%s"
            (substring s 0 8)
            (substring s 8 12)
            (substring s 13 16)
            (substring s 16 20)
            (substring s 20 32))))

(defun uuid-insert ()
  "Inserts a new UUID at the point."
  (interactive)
  (insert (uuid-create)))

(message "[ok]\tLoaded UUID Modules")

;;; END UUID
;;;..................................................................................................


;;;**************************************************************************************************
;;;* BEGIN Haskell
;;;* tag: <haskell>
;;;*
;;;* description:
;;;*
;;;**************************************************************************************************

;;(use-package haskell-mode
;;  :mode (("\\.hs$" . haskell-mode)))

;;(use-package lsp-haskell
;;  :hook
;;  (haskell-mode . lsp)
;;  (haskell-literate-mode . lsp))

;;(use-package flycheck-haskell
;;  :config
;;  (with-eval-after-load "flycheck"
;;    (with-eval-after-load "haskell"
;;      (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))))

(message "[ok]\tLoaded Haskell Modules")

;;; END Haskell
;;;..................................................................................................



;;; END My definition
;;;..................................................................................................

;; TAIL CONFIG ------
