(use-package reverse-im
  :config
  ;;(add-to-list 'load-path "~/.xkb/contrib")
  (add-to-list 'reverse-im-modifiers 'super)
  (add-to-list 'reverse-im-input-methods
               (if (require 'unipunct nil t)
                   "russian-unipunct"
                 "russian-computer"))
  (reverse-im-mode t))


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

