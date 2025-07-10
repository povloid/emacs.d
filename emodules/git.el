;;------------------------------------------------------------------------------
;; BEGIN: Git
;; tag: <git>
;; description: Управление версиями через git
;;------------------------------------------------------------------------------



(use-package magit
  :ensure t)


;(use-package git-gutter
; :ensure t
; :config
; (progn
;    (global-git-gutter-mode +1)
;    (git-gutter:linum-setup)
;    ;; (custom-set-variables
;    ;;  '(git-gutter:window-width 2)
;    ;;  '(git-gutter:modified-sign "☁")
;    ;;  '(git-gutter:added-sign "☀")
;    ;;  '(git-gutter:deleted-sign "☂")
;    ;;  '(git-gutter:unchanged-sign " ")
;    ;;  '(git-gutter:separator-sign "|")
;    ;;  '(git-gutter:hide-gutter t))
;    ;; (set-face-background 'git-gutter:modified "purple") ;; background color
;    ;; (set-face-foreground 'git-gutter:added "green")
;    ;; (set-face-foreground 'git-gutter:deleted "red")
;    ;; (set-face-background 'git-gutter:unchanged "yellow")
;    ;; (set-face-foreground 'git-gutter:separator "yellow")
;    ;; (add-to-list 'git-gutter:update-hooks 'focus-in-hook)
;    )
; :bind (("C-x C-g" . git-gutter:toggle)
;         ("C-x v =" . git-gutter:popup-hunk)
;         ("C-x p" . git-gutter:previous-hunk)
;         ("C-x n" . git-gutter:next-hunk)
;         ("C-x v s" . git-gutter:stage-hunk)
;         ("C-x v r" . git-gutter:revert-hunk)))

;(use-package git-gutter+
;  :ensure t
;  :config
;  (progn
;    (global-git-gutter+-mode +1)
;    ;; Jump between hunks
;    (define-key git-gutter+-mode-map (kbd "C-x n") 'git-gutter+-next-hunk)
;    (define-key git-gutter+-mode-map (kbd "C-x p") 'git-gutter+-previous-hunk)
;    ;; Act on hunks
;    (define-key git-gutter+-mode-map (kbd "C-x v =") 'git-gutter+-show-hunk)
;    (define-key git-gutter+-mode-map (kbd "C-x r") 'git-gutter+-revert-hunks)
;    ;; Stage hunk at point.
;    ;; If region is active, stage all hunk lines within the region.
;    (define-key git-gutter+-mode-map (kbd "C-x t") 'git-gutter+-stage-hunks)
;    (define-key git-gutter+-mode-map (kbd "C-x c") 'git-gutter+-commit)
;    (define-key git-gutter+-mode-map (kbd "C-x C") 'git-gutter+-stage-and-commit)
;    (define-key git-gutter+-mode-map (kbd "C-x C-y") 'git-gutter+-stage-and-commit-whole-buffer)
;    (define-key git-gutter+-mode-map (kbd "C-x U") 'git-gutter+-unstage-whole-buffer))
;  :bind (("C-x g" . git-gutter+-mode)
;         ("C-x G" . global-git-gutter+-mode)))

;(global-git-gutter+-mode +1)

;; END Git
;;..............................................................................
