;;; org-project-capture-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-project-capture" "org-project-capture.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-project-capture.el

(autoload 'org-project-capture-goto-location-for-project "org-project-capture" "\
Goto the location at which TODOs for PROJECT are stored.

\(fn PROJECT)" t nil)

(autoload 'org-project-capture-single-file "org-project-capture" "\
Set `org-project-capture-strategy' so that captures occur in a single file." t nil)

(autoload 'org-project-capture-per-project "org-project-capture" "\
Set `org-project-capture-strategy' so that captures occur within each project." t nil)

(autoload 'org-project-capture-project-todo-completing-read "org-project-capture" "\
Select a project using a `completing-read' and record a TODO.

If CAPTURE-TEMPLATE is provided use it as the capture template
for the TODO. ADDITIONAL-OPTIONS will be supplied as though they
were part of the capture template definition.

\(fn &rest ADDITIONAL-OPTIONS &key CAPTURE-TEMPLATE &allow-other-keys)" t nil)

(autoload 'org-project-capture-capture-for-current-project "org-project-capture" "\
Capture a TODO for the current active project.

If CAPTURE-TEMPLATE is provided use it as the capture template
for the TODO. ADDITIONAL-OPTIONS will be supplied as though they
were part of the capture template definition.

\(fn &rest ADDITIONAL-OPTIONS &key CAPTURE-TEMPLATE &allow-other-keys)" t nil)

(register-definition-prefixes "org-project-capture" '("org-project-capture-"))

;;;***

;;;### (autoloads nil "org-project-capture-backend" "org-project-capture-backend.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-project-capture-backend.el

(register-definition-prefixes "org-project-capture-backend" '("org-project-capture-"))

;;;***

;;;### (autoloads nil nil ("org-project-capture-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-project-capture-autoloads.el ends here
