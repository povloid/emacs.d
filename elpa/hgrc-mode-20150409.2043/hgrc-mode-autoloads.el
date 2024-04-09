;;; hgrc-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "hgrc-mode" "hgrc-mode.el" (0 0 0 0))
;;; Generated autoloads from hgrc-mode.el

(autoload 'hgrc-mode "hgrc-mode" "\
A major mode for editing hgrc files.

\(fn)" t nil)

(dolist (pattern '("/\\.hgrc\\'" "/\\.hg/hgrc\\'" "/Mercurial\\.ini\\'")) (add-to-list 'auto-mode-alist (cons pattern 'hgrc-mode)))

;;;***

;;;### (autoloads nil nil ("hgrc-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; hgrc-mode-autoloads.el ends here
