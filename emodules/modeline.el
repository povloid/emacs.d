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

