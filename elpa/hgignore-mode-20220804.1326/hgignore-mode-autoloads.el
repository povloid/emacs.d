;;; hgignore-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "hgignore-mode" "hgignore-mode.el" (0 0 0 0))
;;; Generated autoloads from hgignore-mode.el

(autoload 'hgignore-mode "hgignore-mode" "\
Major mode for editing .hgignore files.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.hgignore\\'" . hgignore-mode))

(register-definition-prefixes "hgignore-mode" '("hgignore--"))

;;;***

;;;### (autoloads nil nil ("hgignore-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; hgignore-mode-autoloads.el ends here
