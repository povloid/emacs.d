;;; kibit-helper-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "kibit-helper" "kibit-helper.el" (0 0 0 0))
;;; Generated autoloads from kibit-helper.el

(autoload 'kibit "kibit-helper" "\
Run kibit on the current Leiningen project.
Display the results in a hyperlinked *compilation* buffer." t nil)

(autoload 'kibit-current-file "kibit-helper" "\
Run kibit on the current file of a Leiningen project.
Display the results in a hyperlinked *compilation* buffer." t nil)

(autoload 'kibit-accept-proposed-change "kibit-helper" nil t nil)

(register-definition-prefixes "kibit-helper" '("kibit-compilation-do"))

;;;***

;;;### (autoloads nil nil ("kibit-helper-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; kibit-helper-autoloads.el ends here
