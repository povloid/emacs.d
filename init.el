;;; package --- Pacmans config
;;; Commentary:

(load-file "emodules/core.el")

(load-file "emodules/elpa.el")
(load-file "emodules/cursor.el")
(load-file "emodules/themes.el")
(load-file "emodules/git.el")
(load-file "emodules/helm.el")
(load-file "emodules/search.el")
(load-file "emodules/autocomplete.el")
(load-file "emodules/files.el")
(load-file "emodules/search.el")
(load-file "emodules/snippets.el")
(load-file "emodules/flycheck.el")

(load-file "emodules/ru.el")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Iosevka" :foundry "PARA" :slant normal :weight medium :height 130 :width normal))))
 '(flymake-errline ((((class color)) (:background "Gray30"))))
 '(flymake-warnline ((((class color)) (:background "Gray20"))))
 '(font-lock-builtin-face ((t (:weight bold))))
 '(font-lock-comment-face ((t (:weight normal :slant italic))))
 '(font-lock-constant-face ((t (:weight bold))))
 '(font-lock-function-name-face ((t (:weight bold))))
 '(font-lock-keyword-face ((t (:weight bold))))
 '(font-lock-preprocessor-face ((t (:inherit font-lock-builtin-face :weight normal))))
 '(font-lock-type-face ((t (:weight bold))))
 '(font-lock-variable-name-face ((t (:weight bold))))
 '(tabbar-default ((t (:height 1.2)))))
