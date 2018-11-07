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
                         ("melpa-stable" . "https://melpa-stable.milkbox.net/packages/")
                         ;;("org" . "https://orgmode.org/elpa/")
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
  (helm-projectile-on))

(use-package helm-c-yasnippet
  :ensure t
  :config
  (setq helm-yas-space-match-any-greedy t)
  (global-set-key (kbd "C-c y y") 'helm-yas-complete))

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

;;(load-file (concat user-emacs-directory "internal.el"))

;; (use-package paradox
;;   :config
;;   (paradox-enable))

;; (use-package smex)

;; (use-package expand-region
;;   :bind
;;   ("C-=" . er/expand-region))

;; (use-package ivy
;;   :diminish ivy-mode
;;   :custom
;;   ;; (ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
;;   (ivy-count-format "%d/%d " "Show anzu-like counter.")
;;   (ivy-use-selectable-prompt t "Make the prompt line selectable")
;;   :custom-face
;;   (ivy-current-match ((t (:background "gray1"))))
;;   :bind
;;   (("C-c C-r" . ivy-resume))
;;   :config
;;   (ivy-mode t))

;; (use-package ivy-xref
;;   :custom
;;   (xref-show-xrefs-function #'ivy-xref-show-xrefs "Use Ivy to show xrefs"))

;; (use-package counsel
;;   :bind
;;   (([remap menu-bar-open] . counsel-tmm)
;;    ([remap insert-char] . counsel-unicode-char)
;;    ([remap isearch-forward] . counsel-grep-or-swiper))
;;   :config
;;   (counsel-mode))

;; (use-package counsel-projectile
;;   :config
;;   (counsel-projectile-mode))

;; (use-package swiper)

;; (use-package counsel-extras
;;   :ensure nil
;;   :quelpa
;;   (counsel-extras :repo "a13/counsel-extras" :fetcher github :version original)
;;   :bind
;;   (("s-p" . counsel-extras-xmms2-jump)))

;; (use-package ivy-rich
;;   :custom
;;   (ivy-rich-switch-buffer-name-max-length 60 "Increase max length of buffer name.")
;;   :config
;;   (dolist (cmd
;;            '(ivy-switch-buffer
;;              ivy-switch-buffer-other-window
;;              counsel-projectile-switch-to-buffer))
;;     (ivy-set-display-transformer cmd #'ivy-rich-switch-buffer-transformer)))

;; (use-package avy
;;   :config
;;   (avy-setup-default)
;;   :bind
;;   (("C-:" . avy-goto-char)
;;    ;; ("C-'" . avy-goto-char-2)
;;    ("M-g M-g" . avy-goto-line)
;;    ("M-g w" . avy-goto-word-1)))

;; (use-package ace-jump-buffer
;;   :bind
;;   (("M-g b" . ace-jump-buffer)))

;; (use-package ace-window
;;   :custom
;;   (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l) "Use home row for selecting.")
;;   (aw-scope 'frame "Highlight only current frame.")
;;   :bind
;;   (("M-o" . ace-window)))

;; (use-package ace-link
;;   :bind
;;   ("C-c l l" . counsel-ace-link)
;;   :config
;;   (ace-link-setup-default))

;; (use-package link-hint
;;   :ensure t
;;   :bind
;;   (("C-c l o" . link-hint-open-link)
;;    ("<XF86Search>" . link-hint-open-link)
;;    ("C-c l c" . link-hint-copy-link)
;;    ("S-<XF86Search>" . link-hint-copy-link)))

;; (use-package jabber
;;   :config
;;   (setq jabber-history-enabled t
;;         jabber-use-global-history nil
;;         fsm-debug nil)
;;   ;; load jabber-account-list from encrypted file
;;   (defgroup jabber-local nil
;;     "Local settings"
;;     :group 'jabber)

;;   (defcustom jabber-secrets-file "~/.secrets.el.gpg"
;;     "Jabber secrets file, sets jabber-account-list variable)"
;;     :group 'jabber-local)

;;   (defadvice jabber-connect-all (before load-jabber-secrets (&optional arg))
;;     "Try to load account list from secrets file"
;;     (unless jabber-account-list
;;       (when (file-readable-p jabber-secrets-file)
;;         (load-file jabber-secrets-file))))

;;   (ad-activate 'jabber-connect-all)

;;   ;; customized
;;   (custom-set-variables
;;    '(jabber-auto-reconnect t)
;;    '(jabber-chat-buffer-format "*-jc-%n-*")
;;    '(jabber-groupchat-buffer-format "*-jg-%n-*")
;;    '(jabber-chat-foreign-prompt-format "▼ [%t] %n> ")
;;    '(jabber-chat-local-prompt-format "▲ [%t] %n> ")
;;    '(jabber-muc-colorize-foreign t)
;;    '(jabber-muc-private-buffer-format "*-jmuc-priv-%g-%n-*")
;;    '(jabber-rare-time-format "%e %b %Y %H:00")
;;    '(jabber-resource-line-format "   %r - %s [%p]")
;;    '(jabber-roster-buffer "*-jroster-*")
;;    '(jabber-roster-line-format "%c %-17n")
;;    '(jabber-roster-show-bindings nil)
;;    '(jabber-roster-show-title nil)
;;    '(jabber-roster-sort-functions (quote (jabber-roster-sort-by-status jabber-roster-sort-by-displayname jabber-roster-sort-by-group)))
;;    '(jabber-show-offline-contacts nil)
;;    '(jabber-show-resources nil)))

;; (use-package jabber-otr)

;; (use-package slack
;;   :commands (slack-start)
;;   :init
;;   (setq slack-buffer-emojify t) ;; if you want to enable emoji, default nil
;;   (setq slack-prefer-current-team t)
;;   :config
;;   (load-file jabber-secrets-file))

;; ;; TODO: move somewhere
;; (use-package alert
;;   :commands (alert)
;;   :init
;;   (setq alert-default-style 'notifier))

;; (use-package atomic-chrome
;;   :custom
;;   (atomic-chrome-url-major-mode-alist
;;    '(("reddit\\.com" . markdown-mode)
;;      ("github\\.com" . gfm-mode)
;;      ("redmine" . textile-mode))
;;    "Major modes for URLs.")
;;   (atomic-chrome-start-server))

;; (use-package shr-tag-pre-highlight
;;   :after shr
;;   :config
;;   (add-to-list 'shr-external-rendering-functions
;;                '(pre . shr-tag-pre-highlight))

;;   (when (version< emacs-version "26")
;;     (with-eval-after-load 'eww
;;       (advice-add 'eww-display-html :around
;;                   'eww-display-html--override-shr-external-rendering-functions))))

;; (use-package google-this
;;   :diminish google-this-mode
;;   :config
;;   (google-this-mode 1)
;;   :custom
;;   (google-this-keybind (kbd "C-c g")))

;; (use-package mu4e-alert
;;   :after mu4e
;;   :init
;;   (mu4e-alert-set-default-style 'notifications)
;;   :hook ((after-init . mu4e-alert-enable-mode-line-display)
;;          (after-init . mu4e-alert-enable-notifications)))

;; (use-package mu4e-maildirs-extension
;;   :after mu4e
;;   :defines mu4e-maildirs-extension-before-insert-maildir-hook
;;   :init
;;   (mu4e-maildirs-extension)
;;   :config
;;   ;; don't draw a newline
;;   (setq mu4e-maildirs-extension-before-insert-maildir-hook '()))

;; (use-package multitran)

;; (use-package sudo-edit)

;; (use-package keyfreq
;;   :config
;;   (keyfreq-mode 1)
;;   (keyfreq-autosave-mode 1))

;; (use-package which-key
;;   :diminish which-key-mode
;;   :config
;;   (which-key-mode))

;; (use-package helpful)

;; (use-package emamux)

(use-package docker
  :config
  (docker-global-mode))

(use-package dockerfile-mode
  :mode "Dockerfile\\'")

(use-package docker-compose-mode)

;; (use-package restclient)

;; (use-package ob-restclient)

;; (use-package company-restclient
;;   :config
;;   (add-to-list 'company-backends 'company-restclient))

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

;; (use-package magithub
;;   :after magit
;;   :custom
;;   (magithub-clone-default-directory "~/git/")
;;   :config
;;   (magithub-feature-autoinject t))

;; (use-package diff-hl
;;   :hook
;;   ((magit-post-refresh . diff-hl-magit-post-refresh)
;;    (prog-mode . diff-hl-mode)
;;    (org-mode . diff-hl-mode)
;;    (dired-mode . diff-hl-dired-mode)))


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

;; (use-package edit-indirect)

;; (use-package dumb-jump
;;   :custom
;;   (dumb-jump-selector 'ivy)
;;   (dumb-jump-prefer-searcher 'ag))

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

(use-package flycheck-pos-tip
  :ensure t
  :config
  (eval-after-load 'flycheck
    '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))


;; (use-package avy-flycheck
;;   :config
;;   (avy-flycheck-setup))

;; (use-package nameless
;;   :hook
;;   (emacs-lisp-mode .  nameless-mode)
;;   :config
;;   (setq nameless-private-prefix t))

;; (use-package suggest)

;; (use-package ipretty
;;   :config
;;   (ipretty-mode 1))

;; (use-package geiser)

;; clojure =-------------------------------------------------------------------------------------------

(defun clojure-write-tags ()
  (when (or (eq 'clojure-mode major-mode)
            (eq 'clojurescript-mode major-mode)
            (eq 'clojurec-mode major-mode))
    (when-let ((project-dir (clojure-project-dir)))
      (let ((default-directory project-dir))
        (shell-command "find src/ -type f | xargs etags --regex='/[ \\t\\(]*def[a-z\\-]* \\([a-z-!]+\\)/\\1/' --regex='/[ \\t\\(]*ns \\([a-z.]+\\)/\\1/'")))))

(defun clojure-tags-navigate ()
  (interactive)
  (require 'helm-tags)
  (when (not (helm-etags-get-tag-file))
    (clojure-write-tags))
  (helm-etags-select '(4)))


(use-package clojure-mode
  :ensure t
  :mode (("clj\\'" . clojure-mode)
         ("cljs\\'" . clojurescript-mode)
         ("cljc\\'" . clojurec-mode)
         (".lein-env\\'" . clojure-mode))
  :config
  (add-hook 'after-save-hook 'clojure-write-tags)
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

(use-package 4clojure
  :ensure t
  :commands (4clojure-check-answers 4clojure-open-question))

(use-package typed-clojure-mode
  :ensure t
  :commands typed-clojure-mode
  :hook
  ((clojure-mode . typed-clojure-mode)))

(use-package flycheck-clojure
  :ensure t
  :disabled t
  :config
  (flycheck-clojure-setup)
  :hook
  ((clojure-mode . (lambda ()
                     ;; currently not working with cljs
                     (add-to-list 'flycheck-disabled-checkers 'clojure-cider-typed)
                     (add-to-list 'flycheck-disabled-checkers 'clojure-cider-kibit)
                     (add-to-list 'flycheck-disabled-checkers 'clojure-cider-eastwood)))))

(use-package clojure-snippets
  :ensure t
  :config (clojure-snippets-initialize))

(use-package align-cljlet
  :ensure t
  :config
  (define-key clojure-mode-map (kbd "C-M-<tab>") 'align-cljlet))

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

(use-package slime
  :disabled
  :config
  (setq inferior-lisp-program "/usr/bin/sbcl"
        lisp-indent-function 'common-lisp-indent-function
        slime-complete-symbol-function 'slime-fuzzy-complete-symbol
        slime-startup-animation nil)
  (slime-setup '(slime-fancy))
  (setq slime-net-coding-system 'utf-8-unix))

;;(use-package scala-mode)

;; (use-package sbt-mode
;;   :commands sbt-start sbt-command
;;   :config
;;   ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
;;   ;; allows using SPACE when in the minibuffer
;;   (substitute-key-definition
;;    'minibuffer-complete-word
;;    'self-insert-command
;;    minibuffer-local-completion-map))

;; (use-package ensime
;;   :bind (:map ensime-mode-map
;;               ("C-x C-e" . ensime-inf-eval-region)))

(use-package lua-mode)

;; (use-package conkeror-minor-mode
;;   :hook
;;   (js-mode . (lambda ()
;;                (when (string-match "conkeror" (or (buffer-file-name) ""))
;;                  (conkeror-minor-mode 1)))))

(use-package company
  :diminish company-mode
  :hook
  (after-init . global-company-mode))

(use-package company-quickhelp
  :config
  (company-quickhelp-mode 1)
  (setq company-quickhelp-delay 3))

(use-package company-shell
  :config
  (add-to-list 'company-backends 'company-shell))

(use-package company-emoji
  :config
  (add-to-list 'company-backends 'company-emoji)
  (set-fontset-font t 'symbol
                    (font-spec :family
                               (if (eq system-type 'darwin)
                                   "Apple Color Emoji"
                                 "Symbola"))
                    nil 'prepend))

(use-package company-web
  :ensure t)

;; (use-package org
;;   :ensure org-plus-contrib
;;   :init
;;   (setq org-src-tab-acts-natively t))

;; (use-package org-bullets
;;   :init
;;   ;; org-bullets-bullet-list
;;   ;; default: "◉ ○ ✸ ✿"
;;   ;; large: ♥ ● ◇ ✚ ✜ ☯ ◆ ♠ ♣ ♦ ☢ ❀ ◆ ◖ ▶
;;   ;; Small: ► • ★ ▸
;;   (setq org-bullets-bullet-list '("•"))
;;   ;; others: ▼, ↴, ⬎, ⤷,…, and ⋱.
;;   ;; (setq org-ellipsis "⤵")
;;   (setq org-ellipsis "…")
;;   :hook
;;   (org-mode . org-bullets-mode))

;; (use-package htmlize
;;   :config
;;   (setq org-html-htmlize-output-type 'css)
;;   (setq org-html-htmlize-font-prefix "org-"))

;; (use-package org-password-manager
;;   :hook
;;   (org-mode . org-password-manager-key-bindings))

;; (use-package org-jira
;;   :config
;;   (setq jiralib-url "http://jira:8080"))

;; (use-package dashboard
;;   :config
;;   (dashboard-setup-startup-hook)
;;   (setq initial-buffer-choice '(lambda ()
;;                                  (setq initial-buffer-choice nil)
;;                                  (get-buffer "*dashboard*")))
;;   (setq dashboard-items '((recents  . 5)
;;                           (bookmarks . 5)
;;                           (projects . 5)
;;                           ;; (agenda . 5)
;;                           (registers . 5))))
;; (use-package page-break-lines
;;   :config
;;   (global-page-break-lines-mode))

;; ------------------------------------------------
;; (use-package rainbow-delimiters
;;   :hook
;;   (prog-mode . rainbow-delimiters-mode))

;; (use-package rainbow-identifiers
;;   :hook
;;   (prog-mode . rainbow-identifiers-mode))

;; (use-package rainbow-mode
;;   :diminish rainbow-mode
;;   :hook prog-mode)
;; ------------------------------------------------

;; (use-package spaceline
;;   :config
;;   (require 'spaceline-config)
;;   (spaceline-spacemacs-theme))

;; (use-package fancy-battery
;;   :hook
;;   (after-init . fancy-battery-mode))

;; (use-package clipmon
;;   :config
;;   (clipmon-mode))

;; (use-package yahoo-weather
;;   :custom
;;   (yahoo-weather-location "Moscow, RU"))

;; (use-package all-the-icons
;;   :init
;;   (set-frame-font "all-the-icons" t)
;;   :config
;;   (add-to-list
;;    'all-the-icons-mode-icon-alist
;;    '(package-menu-mode all-the-icons-octicon "package" :v-adjust 0.0)))

;; (use-package all-the-icons-dired
;;   :hook
;;   (dired-mode . all-the-icons-dired-mode))

;; (use-package spaceline-all-the-icons
;;   :after spaceline
;;   :config
;;   (spaceline-all-the-icons-theme)
;;   (spaceline-all-the-icons--setup-package-updates)
;;   (spaceline-all-the-icons--setup-git-ahead)
;;   (spaceline-all-the-icons--setup-paradox))

;; (use-package all-the-icons-ivy
;;   :custom
;;   (all-the-icons-ivy-buffer-commands '() "Don't use for buffers.")
;;   (all-the-icons-ivy-file-commands
;;         '(counsel-find-file
;;           counsel-file-jump
;;           counsel-recentf
;;           counselа-projectile-find-file
;;           counsel-projectile-find-dir) "Prettify more commands.")
;;   :config
;;   (all-the-icons-ivy-setup))

;; (use-package dired-hide-dotfiles
;;   :bind
;;   (:map dired-mode-map
;;         ("." . dired-hide-dotfiles-mode))
;;   :hook
;;   (dired-mode . dired-hide-dotfiles-mode))

;; (use-package point-im
;;   :ensure nil
;;   :defines point-im-reply-id-add-plus
;;   :quelpa
;;   (point-im :repo "a13/point-im.el" :fetcher github :version original)
;;   :config
;;   (setq point-im-reply-id-add-plus nil)
;;   :hook
;;   (jabber-chat-mode . point-im-mode))

;; (use-package iqa
;;   :custom
;;   (iqa-user-init-file (concat user-emacs-directory "init.org") "Edit init.org by default.")
;;   :config
;;   (iqa-setup-default))

;; (use-package esh-autosuggest
;;   :hook (eshell-mode . esh-autosuggest-mode)
;;   :ensure t)

(use-package font-lock+
  :ensure t
  :quelpa
  (font-lock+ :repo "emacsmirror/font-lock-plus" :fetcher github))

;; (use-package eshell-toggle
;;   :ensure nil
;;   :quelpa
;;   (eshell-toggle :repo "4DA/eshell-toggle" :fetcher github :version original)
;;   :bind
;;   (("M-`" . eshell-toggle)))

;; (use-package magit-keys
;;   :ensure nil
;;   :quelpa
;;   (magit-keys :repo "a13/magit-keys.el" :fetcher github :version original)
;;   :config
;;   (magit-keys-mode t))

(use-package reverse-im
  :config
  ;;(add-to-list 'load-path "~/.xkb/contrib")
  (add-to-list 'reverse-im-modifiers 'super)
  (add-to-list 'reverse-im-input-methods
               (if (require 'unipunct nil t)
                   "russian-unipunct"
                 "russian-computer"))
  (reverse-im-mode t))

;; defined in internal.el
;;(when (and custom-file (file-exists-p custom-file))
;;  (load-file custom-file))

;; Local Variables:
;; eval: (add-hook 'after-save-hook (lambda ()(org-babel-tangle)) nil t)
;; End:


;; modes

;;; Code:

(add-to-list 'exec-path "~/bin/")

(setq scroll-step 1)

(setq inhibit-splash-screen t)
(setq use-dialog-box nil)

;;(setq enable-recursive-minibuffers t)

;;(put 'narrow-to-region 'disabled nil)
;;(put 'downcase-region 'disabled nil)

;;(setq-default indent-tabs-mode nil)
;;(setq-default c-basic-offset 4)

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

;; (use-package time
;;   :ensure nil
;;   :config
;;   (setq display-time-default-load-average nil)
;;   (setq display-time-24hr-format t)
;;   (display-time-mode t))

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
                          150 :width normal))))
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

;; (use-package custom
;;   :ensure nil
;;   :config
;;   (setq custom-enabled-themes '(deeper-blue))
;;   (load-theme 'deeper-blue))

;;; highlighting

(use-package paren
  :ensure nil
  :config
  (show-paren-mode t))

(use-package hl-line
  :ensure nil
  :config
  (global-hl-line-mode -1))

;; (use-package man
;;   :ensure nil
;;   :config
;;   (set-face-attribute 'Man-overstrike nil :inherit font-lock-type-face :bold t)
;;   (set-face-attribute 'Man-underline nil :inherit font-lock-keyword-face :underline t))

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

;; (use-package uniquify
;;   :ensure nil
;;   :config
;;   (setq uniquify-buffer-name-style 'forward))

;; ;;; language/keyboard etc
;; (use-package mule
;;   :ensure nil
;;   :config
;;   (set-language-environment "UTF-8"))

;;; ibuffer
(use-package ibuffer
  :ensure nil
  :bind
  ([remap list-buffers] . ibuffer))

;; (use-package simple
;;   :ensure nil
;;   :diminish
;;   ((visual-line-mode . " ↩")
;;    (auto-fill-function . " ↵"))
;;   :config
;;   (column-number-mode t)
;;   (toggle-truncate-lines 1)
;;   :bind
;;   ;; remap ctrl-w/ctrl-h
;;   (("C-c h" . help-command)
;;    ("C-w" . backward-kill-word)
;;    ("C-x C-k" . kill-region)
;;    ("C-h" . delete-backward-char)))

;; (use-package smerge-mode
;;   :ensure nil
;;   :diminish smerge-mode)

;; (use-package autorevert
;;   :ensure nil
;;   :diminish auto-revert-mode)

(use-package isearch
  :ensure nil
  :bind
  (:map isearch-mode-map
        ("C-h" . isearch-delete-char)))

;; (use-package delsel
;;   :ensure nil
;;   :bind
;;   ("C-c C-g" . minibuffer-keyboard-quit))


;;; tramp

;; (use-package tramp
;;   :ensure nil
;;   :config
;;   (setq tramp-default-method "ssh")
;;   ;; TODO: tramp-root-connect-list
;;   ;; `("\\.lpr\\." "10\\.199\\." "10\\.0\\." ,(regexp-quote (system-name)))
;;   (setq tramp-default-proxies-alist nil)
;;   (add-to-list 'tramp-default-proxies-alist
;;                '(nil "\\`root\\'" "/ssh:%h:"))
;;   (add-to-list 'tramp-default-proxies-alist
;;                '("10\\.199\\." nil nil))
;;   (add-to-list 'tramp-default-proxies-alist
;;                '("10\\.0\\." nil nil))
;;   (add-to-list 'tramp-default-proxies-alist
;;                `((regexp-quote ,(system-name)) nil nil)))


;; (use-package epa
;;   :ensure nil
;;   :config
;;   (setf epa-pinentry-mode nil))

;; (use-package calendar
;;   :ensure nil
;;   :config
;;   (setq calendar-week-start-day 1))

;; (use-package select
;;   :ensure nil
;;   :config
;;   (setq select-enable-clipboard t))

;; spellchecker

(use-package ispell
  :ensure nil
  :config
  (setq ispell-local-dictionary-alist
        '(("russian"
           "[АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЬЫЪЭЮЯабвгдеёжзийклмнопрстуфхцчшщьыъэюяіїєґ’A-Za-z]"
           "[^АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЬЫЪЭЮЯабвгдеёжзийклмнопрстуфхцчшщьыъэюяіїєґ’A-Za-z]"
           "[-']"  nil ("-d" "uk_UA,ru_RU,en_US") nil utf-8))
        ispell-program-name "hunspell"
        ispell-dictionary "russian"
        ispell-really-aspell nil
        ispell-really-hunspell t
        ispell-encoding8-command t
        ispell-silently-savep t))

(use-package flyspell
  :ensure nil
  :config
  (setq flyspell-delay 1))

(use-package sh-script
  :ensure nil
  :mode (("zshecl" . sh-mode)
         ("\\.zsh\\'" . sh-mode))
  :config
  ;; zsh
  (setq system-uses-terminfo nil))

;; ;; dired and eshell

;; (use-package eshell
;;   :ensure nil)

;; (use-package em-smart
;;   :ensure nil
;;   :init
;;   (eshell-smart-initialize)
;;   :config
;;   (setq eshell-where-to-jump 'begin)
;;   (setq eshell-review-quick-commands nil)
;;   (setq eshell-smart-space-goes-to-end t))

;; (use-package ls-lisp
;;   :ensure nil
;;   :config
;;   (setq ls-lisp-emulation 'MS-Windows)
;;   (setq ls-lisp-ignore-case t)
;;   (setq ls-lisp-verbosity nil))

(use-package dired
  :ensure nil
  :bind
  ([remap list-directory] . dired)
  :hook
  (dired-mode . dired-hide-details-mode))

;; (use-package dired-x
;;   :ensure nil
;;   :config
;;   ;; do not bind C-x C-j since it's used by jabber.el
;;   (setq dired-bind-jump nil))

;; web

;; (use-package eww
;;   :ensure nil
;;   :config
;;   (setq shr-use-fonts nil)
;;   (setq eww-search-prefix "https://duckduckgo.com/html/?kd=-1&q="))

;; (use-package browse-url
;;   :ensure nil
;;   :bind
;;   (([f5] . browse-url))
;;   :config
;;   (setq browse-url-browser-function 'browse-url-generic
;;         browse-url-generic-program "x-www-browser")

;;   (defun feh-browse (url &rest ignore)
;;     "Browse image using feh."
;;     (interactive (browse-url-interactive-arg "URL: "))
;;     (start-process (concat "feh " url) nil "feh" url))

;;   (defun mpv-browse (url &rest ignore)
;;     "Browse video using mpv."
;;     (interactive (browse-url-interactive-arg "URL: "))
;;     (start-process (concat "mpv --loop-file=inf" url) nil "mpv" "--loop-file=inf" url))

;;   (defvar browse-url-images-re
;;     '("\\.\\(jpe?g\\|png\\)\\(:large\\|:orig\\)?\\(\\?.*\\)?$"
;;       "^https?://img-fotki\\.yandex\\.ru/get/"
;;       "^https?://pics\\.livejournal\\.com/.*/pic/"
;;       "^https?://l-userpic\\.livejournal\\.com/"
;;       "^https?://img\\.leprosorium\\.com/[0-9]+$")
;;     "Image URLs regular expressions list.")

;;   (defvar browse-url-videos-re
;;     '("\\.\\(gifv?\\|avi\\|AVI\\|mp[4g]\\|MP4\\|webm\\)$"
;;       "^https?://\\(www\\.youtube\\.com\\|youtu\\.be\\|coub\\.com\\|vimeo\\.com\\|www\\.liveleak\\.com\\)/"
;;       "^https?://www\\.facebook\\.com/.*/videos?/"))

;;   (setq browse-url-browser-function
;;         (append
;;          (mapcar (lambda (re)
;;                    (cons re #'eww-browse-url))
;;                  browse-url-images-re)
;;          (mapcar (lambda (re)
;;                    (cons re #'mpv-browse))
;;                  browse-url-videos-re)
;;          '(("." . browse-url-xdg-open)))))

;; (use-package webjump
;;   :bind
;;   (([S-f5] . webjump))
;;   :config
;;   (setq webjump-sites
;;         (append '(("debian packages" .
;;                    [simple-query "packages.debian.org" "http://packages.debian.org/" ""]))
;;                 webjump-sample-sites)))


;; (use-package cus-edit
;;   :ensure nil
;;   :config
;;   (setq custom-file (concat user-emacs-directory "custom.el")))

;; ;; (use-package mu4e-vars
;; ;;   :load-path "/usr/share/emacs/site-lisp/mu4e"
;; ;;   :ensure nil
;; ;;   :config
;; ;;   ;;location of my maildir
;; ;;   ;; enable inline images
;; ;;   (setq mu4e-view-show-images t)
;; ;;   ;; use imagemagick, if available
;; ;;   (when (fboundp 'imagemagick-register-types)
;; ;;     (imagemagick-register-types))

;; ;;   (setq mu4e-maildir (expand-file-name "~/.mail/work"))
;; ;;   ;; ivy does all the work
;; ;;   (setq mu4e-completing-read-function 'completing-read)

;; ;;   ;;command used to get mail
;; ;;   ;; use this for testing
;; ;;   (setq mu4e-get-mail-command "true")
;; ;;   ;; use this to sync with mbsync
;; ;;   ;;(setq mu4e-get-mail-command "mbsync gmail")

;; ;;   ;;rename files when moving
;; ;;   ;;NEEDED FOR MBSYNC
;; ;;   (setq mu4e-change-filenames-when-moving t))

;; (use-package smtpmail
;;   :ensure nil
;;   :config
;;   ;;set up queue for offline email
;;   ;;use mu mkdir  ~/Maildir/queue to set up first
;;   (setq smtpmail-queue-mail nil  ;; start in normal mode
;;         smtpmail-queue-dir "~/Maildir/queue/cur"))

;; (use-package net-utils
;;   :bind
;;   (:prefix-map net-utils-prefix-map
;;                :prefix "C-c n"
;;                ("p" . ping)
;;                ("i" . ifconfig)
;;                ("w" . iwconfig)
;;                ("n" . netstat)
;;                ("p" . ping)
;;                ("a" . arp)
;;                ("r" . route)
;;                ("h" . nslookup-host)
;;                ("d" . dig)
;;                ("s" . smbclient)))

;; (use-package autoinsert
;;   :hook
;;   (find-file . auto-insert))


(use-package erlang
  :ensure t
  :config
  (when (eq system-type 'windows-nt)
    (setq erlang-root-dir "C:/Program Files/erl7.2")
    (add-to-list 'exec-path "C:/Program Files/erl7.2/bin")))

(use-package markdown-mode
  :ensure t)

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

;; (use-package xref-js2
;;  :ensure t
;;  :config
;;  (define-key js2-mode-map (kbd "M-.") nil)
;;  (add-hook 'js2-mode-hook (lambda ()
;;                             (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))))

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

;; devops -----------------------------------------------------------------------

(use-package terraform-mode
  :ensure t
  :mode "\\.tf\\'")

(use-package company-terraform
  :ensure t)

;; ------------------------------------------------------------------------


;; RUST -------------------------------------------------------------------

(use-package rust-mode
  :mode "\\.rs\\'"
  :config
  (setq rust-format-on-save t)
  (add-hook 'rust-mode-hook 'racer-mode)
  (add-hook 'racer-mode-hook 'eldoc-mode)
  (add-hook 'rust-mode-hook 'company-mode)
  (add-hook 'rust-mode-hook 'flycheck-mode)
  (setq company-tooltip-align-annotations t))

(use-package racer
  :ensure t
  :pin melpa-stable
  :commands (racer-mode)
  :init
  (setq racer-rust-src-path (shell-command-to-string "echo -n $(rustc --print sysroot)/lib/rustlib/src/rust/src"))
  :config
  (add-hook-exec 'racer-mode #'eldoc-mode)
  (add-hook-exec 'racer-mode #'company-mode)
  (add-hook-exec 'racer-mode #'cargo-minor-mode))

(use-package cargo
  :ensure t
  :pin melpa-stable
  :commands cargo-minor-mode)

(use-package flycheck-rust
  :ensure t
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))


;; TAIL CONFIG ------------------------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
	 [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
	 (vector "#003f8e" "#ff9da4" "#d1f1a9" "#ffeead" "#bbdaff" "#ebbbff" "#99ffff" "#ffffff"))
 '(beacon-color "#ff9da4")
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
	 (quote
		("b34636117b62837b3c0c149260dfebe12c5dad3d1177a758bb41c4b15259ed7e" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default)))
 '(fci-rule-color "#003f8e")
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(frame-background-mode (quote dark))
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
		(solarized-color-blend it "#002b36" 0.25)
		(quote
		 ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
	 (quote
		(("#073642" . 0)
		 ("#546E00" . 20)
		 ("#00736F" . 30)
		 ("#00629D" . 50)
		 ("#7B6000" . 60)
		 ("#8B2C02" . 70)
		 ("#93115C" . 85)
		 ("#073642" . 100))))
 '(hl-bg-colors
	 (quote
		("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
	 (quote
		("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
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
		(cargo racer xref-js2 typed-clojure-mode subatomic-theme sql-indent solarized-theme rust-mode reverse-im quelpa-use-package perspective neotree monokai-theme markdown-mode magit lua-mode kibit-helper js2-refactor indium ibuffer-vc ialign helm-swoop helm-projectile helm-descbinds helm-c-yasnippet helm-ag git-gutter git-gutter+ font-lock+ flycheck-pos-tip flycheck-color-mode-line erlang dockerfile-mode docker-compose-mode docker diminish cyberpunk-theme company-web company-terraform company-tern company-shell company-quickhelp company-emoji color-theme-sanityinc-tomorrow clojure-snippets clojure-mode-extra-font-locking clojure-cheatsheet cljsbuild-mode cljr-helm atom-dark-theme align-cljlet ag ac-js2 4clojure)))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(solarized-use-variable-pitch nil)
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
	 (quote
		((20 . "#ff9da4")
		 (40 . "#ffc58f")
		 (60 . "#ffeead")
		 (80 . "#d1f1a9")
		 (100 . "#99ffff")
		 (120 . "#bbdaff")
		 (140 . "#ebbbff")
		 (160 . "#ff9da4")
		 (180 . "#ffc58f")
		 (200 . "#ffeead")
		 (220 . "#d1f1a9")
		 (240 . "#99ffff")
		 (260 . "#bbdaff")
		 (280 . "#ebbbff")
		 (300 . "#ff9da4")
		 (320 . "#ffc58f")
		 (340 . "#ffeead")
		 (360 . "#d1f1a9"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
	 (quote
		(unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83")))
 '(xterm-color-names
	 ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
	 ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))
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
