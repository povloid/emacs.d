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
                         ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
                         ;; ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ;; ("sunrise" . "http://joseito.republika.pl/sunrise-commander/")
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

;; :diminish keyword
(use-package diminish)

;; :bind keyword
;;(use-package bind-key)

;; :quelpa keyword
(use-package quelpa)
(use-package quelpa-use-package)


(use-package leuven-theme
  :ensure t
  :config
  (load-theme 'leuven t))


;; helm ---------------------------------------------------------------------------

(use-package ag
  :ensure t
  :commands (ag ag-regexp ag-project))

(use-package helm
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-buffers-list)
         ("C-M-y" . helm-show-kill-ring)
         ;;([S-f10] . helm-recentf)
         ))

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
              helm-ag-command-option "--path-to-ignore ~/.agignore"))

(use-package projectile
  :ensure t
  :config
  (projectile-global-mode)
  (setq projectile-enable-caching t))

(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on))

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
  :ensure t
  )

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
  :diminish yas-minor-mode
  :config
  (yas-reload-all)
  (setq yas-prompt-functions '(yas-completing-prompt yas-ido-prompt))
  :hook
  (prog-mode  . yas-minor-mode))

(use-package flycheck
  :diminish flycheck-mode
  :hook
  (prog-mode . flycheck-mode))

(use-package avy-flycheck
  :config
  (avy-flycheck-setup))

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
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
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
  :commands cider-mode cider-jack-in-clojurescript
  :init
  (progn
    (defun figwheel-repl ()
      (interactive)
      (run-clojure "lein figwheel"))

    (defun cider-repl-to-figwhell-repl ()
      (interactive)
      (save-some-buffers)
      (goto-char (point-max))
      (insert "(do (use 'figwheel-sidecar.repl-api) (cljs-repl))"))
    (setq cider-auto-jump-to-error 'errors-only))
  :config
  (setq nrepl-log-messages t)
  (setq nrepl-sync-request-timeout 60)
  (define-key cider-mode-map (kbd "C-c M-J") 'cider-jack-in-clojurescript)
  (define-key cider-mode-map (kbd "C-x c e") 'clojure-tags-navigate)
  (setq cider-prefer-local-resources t)
  (setq nrepl-hide-special-buffers nil)
  (setq cider-font-lock-dynamically nil)
  (setq cider-repl-use-pretty-printing t)
  (setq cider-repl-result-prefix ";; => ")
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode))

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

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-identifiers
  :hook
  (prog-mode . rainbow-identifiers-mode))

(use-package rainbow-mode
  :diminish rainbow-mode
  :hook prog-mode)

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
  :init
  (defvar default-font "PT Mono-13")
  :config
  (set-frame-font default-font)
  (add-to-list 'default-frame-alist `(font . ,default-font))
  (setq initial-frame-alist default-frame-alist)
  (setq display-buffer-alist default-frame-alist)
  (set-fontset-font "fontset-default" 'cyrillic
                    (font-spec :registry "iso10646-1" :script 'cyrillic)))

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
  (global-hl-line-mode 1))

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


;; use-package sh-script
;;   :ensure nil
;;   :mode (("zshecl" . sh-mode)
;;          ("\\.zsh\\'" . sh-mode))
;;   :config
;;   ;; zsh
;;   (setq system-uses-terminfo nil))

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



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(git-gutter:added-sign "☀")
 '(git-gutter:deleted-sign "☂")
 '(git-gutter:hide-gutter t)
 '(git-gutter:modified-sign "☁")
 '(git-gutter:separator-sign "|")
 '(git-gutter:unchanged-sign " ")
 '(git-gutter:window-width 2)
 '(package-selected-packages
   (quote
    (cljsbuild-mode 4clojure scala-mode reverse-im rainbow-mode rainbow-identifiers rainbow-delimiters quelpa-use-package paredit multiple-cursors magithub lua-mode leuven-theme kibit-helper ibuffer-vc helm-swoop helm-projectile helm-descbinds helm-ag git-gutter git-gutter+ font-lock+ dockerfile-mode docker-compose-mode docker diminish company-shell company-quickhelp company-emoji clojure-snippets clojure-mode-extra-font-locking cider avy-flycheck ag))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
