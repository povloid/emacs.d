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

(use-package all-the-icons
   :ensure t)

