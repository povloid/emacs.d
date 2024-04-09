;;; org-projectile-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-projectile" "org-projectile.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from org-projectile.el

(autoload 'org-projectile-goto-location-for-project "org-projectile" "\
Goto the location at which TODOs for PROJECT are stored.

\(fn PROJECT)" t nil)

(make-obsolete 'org-projectile-goto-location-for-project 'org-project-capture-goto-location-for-project '"3.0.1")

(autoload 'org-projectile-single-file "org-projectile" "\
Set `org-projectile-strategy' so that captures occur in a single file." t nil)

(make-obsolete 'org-projectile-single-file 'org-project-capture-single-file '"3.0.1")

(autoload 'org-projectile-per-project "org-projectile" "\
Set `org-projectile-strategy' so that captures occur within each project." t nil)

(make-obsolete 'org-projectile-per-project 'org-project-capture-per-project '"3.0.1")

(autoload 'org-projectile-project-todo-completing-read "org-projectile" "\
Select a project using a `completing-read' and record a TODO.

If CAPTURE-TEMPLATE is provided use it as the capture template
for the TODO. ADDITIONAL-OPTIONS will be supplied as though they
were part of the capture template definition.

\(fn &rest ADDITIONAL-OPTIONS &key CAPTURE-TEMPLATE &allow-other-keys)" t nil)

(make-obsolete 'org-projectile-project-todo-completing-read 'org-project-capture-project-todo-completing-read '"3.0.1")

(autoload 'org-projectile-capture-for-current-project "org-projectile" "\
Capture a TODO for the current active project.

If CAPTURE-TEMPLATE is provided use it as the capture template
for the TODO. ADDITIONAL-OPTIONS will be supplied as though they
were part of the capture template definition.

\(fn &rest ADDITIONAL-OPTIONS &key CAPTURE-TEMPLATE &allow-other-keys)" t nil)

(make-obsolete 'org-projectile-capture-for-current-project 'org-project-capture-capture-for-current-project '"3.0.1")

(register-definition-prefixes "org-projectile" '("org-project"))

;;;***

;;;### (autoloads nil "org-projectile-helm" "org-projectile-helm.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-projectile-helm.el

(autoload 'org-projectile-helm-template-or-project "org-projectile-helm" "\
Select a project or `org-capture' template and record a TODO.

If provided, CAPTURE-TEMPLATE-FOR-PROJECT will be the capture
template used for project TODO capture.

\(fn &optional CAPTURE-TEMPLATE-FOR-PROJECT)" t nil)

(register-definition-prefixes "org-projectile-helm" '("org-projectile-helm-"))

;;;***

;;;### (autoloads nil nil ("org-projectile-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-projectile-autoloads.el ends here
