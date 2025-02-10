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
