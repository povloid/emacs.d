;;; ssh-deploy-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ssh-deploy" "ssh-deploy.el" (0 0 0 0))
;;; Generated autoloads from ssh-deploy.el

(autoload 'ssh-deploy-diff-files "ssh-deploy" "\
Find difference between FILE-A and FILE-B,
do it asynchronous if ASYNC is aboe zero
and use threads if ASYNC-WITH-THREADS is above zero,
if VERBOSE is above zero print messages.

\(fn FILE-A FILE-B &optional ASYNC ASYNC-WITH-THREADS VERBOSE)" nil nil)

(autoload 'ssh-deploy-diff-directories "ssh-deploy" "\
Find difference between DIRECTORY-A and DIRECTORY-B but exclude,
ON-EXPLICIT-SAVE defines automatic uploads,
FORCE-ON-EXPLICIT-SAVE whether automatic uploads are forced,
DEBUG is the debug flag, ASYNC is for asynchronous,
ASYNC-WITH-THREADS for threads instead of processes,
REVISION-FOLDER is for revisions,
REMOTE-CHANGES are whether to look for remote change,
EXCLUDE-LIST is what files to exclude.

\(fn DIRECTORY-A DIRECTORY-B &optional ON-EXPLICIT-SAVE FORCE-ON-EXPLICIT-SAVE DEBUG ASYNC ASYNC-WITH-THREADS REVISION-FOLDER REMOTE-CHANGES EXCLUDE-LIST)" nil nil)

(autoload 'ssh-deploy-remote-changes "ssh-deploy" "\
Check if a local revision for PATH-LOCAL on ROOT-LOCAL and
if remote file has changed on ROOT-REMOTE,
do it optionally asynchronously if ASYNC is true,
check for copies in REVISION-FOLDER and skip if path is in EXCLUDE-LIST.
Use multi-threading if ASYNC-WITH-THREADS is above zero,
VERBOSE if value above zero.

\(fn PATH-LOCAL &optional ROOT-LOCAL ROOT-REMOTE ASYNC REVISION-FOLDER EXCLUDE-LIST ASYNC-WITH-THREADS VERBOSE)" nil nil)

(autoload 'ssh-deploy-delete-both "ssh-deploy" "\
Delete PATH-LOCAL relative to ROOT-LOCAL as well as on ROOT-REMOTE,
do it asynchronously if ASYNC is non-nil,
debug if DEBUG is non-nil, check if path is excluded in EXCLUDE-LIST.
Use async threads is ASYNC-WITH-THREADS is above zero.

\(fn PATH-LOCAL &optional ROOT-LOCAL ROOT-REMOTE ASYNC DEBUG EXCLUDE-LIST ASYNC-WITH-THREADS)" nil nil)

(autoload 'ssh-deploy-rename "ssh-deploy" "\
Rename OLD-PATH-LOCAL to NEW-PATH-LOCAL
under ROOT-LOCAL as well as on ROOT-REMOTE,
do it asynchronously if ASYNC is non-nil,
debug if DEBUG is non-nil
but check if path is excluded in EXCLUDE-LIST first.
Use multi-threading if ASYNC-WITH-THREADS is above zero.

\(fn OLD-PATH-LOCAL NEW-PATH-LOCAL &optional ROOT-LOCAL ROOT-REMOTE ASYNC DEBUG EXCLUDE-LIST ASYNC-WITH-THREADS)" nil nil)

(autoload 'ssh-deploy-remote-sql "ssh-deploy" "\
Open remote sql on REMOTE-PATH, TYPE determines type and defaults to mysql.

\(fn REMOTE-PATH &optional TYPE)" nil nil)

(autoload 'ssh-deploy-browse-remote "ssh-deploy" "\
Browse PATH-LOCAL in `dired-mode' on remote where it is inside
ROOT-LOCAL and mirrored on ROOT-REMOTE and not in EXCLUDE-LIST.

\(fn PATH-LOCAL &optional ROOT-LOCAL ROOT-REMOTE EXCLUDE-LIST)" nil nil)

(autoload 'ssh-deploy-remote-terminal-eshell "ssh-deploy" "\
Browse PATH-LOCAL inside ROOT-LOCAL on
ROOT-REMOTE in `eshell-mode' if not in EXCLUDE-LIST.

\(fn PATH-LOCAL &optional ROOT-LOCAL ROOT-REMOTE EXCLUDE-LIST)" nil nil)

(autoload 'ssh-deploy-remote-terminal-shell "ssh-deploy" "\
Browse PATH-LOCAL inside ROOT-LOCAL on
ROOT-REMOTE in `eshell-mode' if not in EXCLUDE-LIST.

\(fn PATH-LOCAL &optional ROOT-LOCAL ROOT-REMOTE EXCLUDE-LIST)" nil nil)

(autoload 'ssh-deploy-store-revision "ssh-deploy" "\
Store PATH in revision-folder ROOT.

\(fn PATH &optional ROOT)" nil nil)

(autoload 'ssh-deploy-diff "ssh-deploy" "\
Find differences between PATH-LOCAL and PATH-REMOTE,
where PATH-LOCAL is inside ROOT-LOCAL.  DEBUG enables feedback message,
check if PATH-LOCAL is not in EXCLUDE-LIST.
ASYNC make the process work asynchronously,
if ASYNC-WITH-THREADS is above zero use threads,
ON-EXPLICIT-SAVE for automatic uploads,
FORCE-ON-EXPLICIT-SAVE whether these uploads are forced or not,
REVISION-FOLDER for revision-folder,
REMOTE-CHANGES for automatic notification of remote change,
VERBOSE messaging if above zero.

\(fn PATH-LOCAL PATH-REMOTE &optional ROOT-LOCAL DEBUG EXCLUDE-LIST ASYNC ASYNC-WITH-THREADS ON-EXPLICIT-SAVE FORCE-ON-EXPLICIT-SAVE REVISION-FOLDER REMOTE-CHANGES VERBOSE)" nil nil)

(autoload 'ssh-deploy-upload "ssh-deploy" "\
Upload PATH-LOCAL to PATH-REMOTE and ROOT-LOCAL via Tramp,
FORCE uploads despite remote change,
ASYNC determines if transfer should be asynchronously,
check version in REVISION-FOLDER.
If you want asynchronous threads pass ASYNC-WITH-THREADS above zero.

\(fn PATH-LOCAL PATH-REMOTE &optional FORCE ASYNC REVISION-FOLDER ASYNC-WITH-THREADS)" nil nil)

(autoload 'ssh-deploy-download "ssh-deploy" "\
Download PATH-REMOTE to PATH-LOCAL via Tramp,
ASYNC determines if transfer should be asynchrous or not,
check for revisions in REVISION-FOLDER.
If you want asynchronous threads pass ASYNC-WITH-THREADS above zero.

\(fn PATH-REMOTE PATH-LOCAL &optional ASYNC REVISION-FOLDER ASYNC-WITH-THREADS)" nil nil)

(autoload 'ssh-deploy-upload-handler "ssh-deploy" "\
Upload current path to remote if it is configured for
deployment and if remote version hasn't changed or FORCE is specified.

\(fn &optional FORCE)" t nil)

(autoload 'ssh-deploy-upload-handler-forced "ssh-deploy" "\
Upload current path to remote host if it is configured for deployment." t nil)

(autoload 'ssh-deploy-remote-changes-handler "ssh-deploy" "\
Check if local revision exists or remote file has changed
if path is configured for deployment." t nil)

(autoload 'ssh-deploy-remote-sql-mysql-handler "ssh-deploy" "\
Open `sql-mysql' on remote path if path is configured for deployment." t nil)

(autoload 'ssh-deploy-remote-sql-postgres-handler "ssh-deploy" "\
Open `sql-postgres' on remote path if path is configured for deployment." t nil)

(autoload 'ssh-deploy-open-remote-file-handler "ssh-deploy" "\
Check if local revision exists or remote file has changed
if path is configured for deployment." t nil)

(autoload 'ssh-deploy-download-handler "ssh-deploy" "\
Download current path from remote if it is configured for deployment." t nil)

(autoload 'ssh-deploy-diff-handler "ssh-deploy" "\
Compare current path with remote host if it is configured for deployment." t nil)

(autoload 'ssh-deploy-delete-handler "ssh-deploy" "\
Delete current file or directory." t nil)

(autoload 'ssh-deploy-rename-handler "ssh-deploy" "\
Rename current file or directory." t nil)

(autoload 'ssh-deploy-remote-terminal-eshell-handler "ssh-deploy" "\
Open current relative path on remote host in `eshell'
but only if it's configured for deployment." t nil)

(autoload 'ssh-deploy-remote-terminal-eshell-base-handler "ssh-deploy" "\
Open base path on remote host in `eshell'
but only if it's configured for deployment." t nil)

(autoload 'ssh-deploy-remote-terminal-shell-handler "ssh-deploy" "\
Open current relative path on remote host in `shell'
but only if it's configured for deployment." t nil)

(autoload 'ssh-deploy-remote-terminal-shell-base-handler "ssh-deploy" "\
Open base path on remote host in `shell'
but only if it's configured for deployment." t nil)

(autoload 'ssh-deploy-browse-remote-handler "ssh-deploy" "\
Open current relative path on remote host in `dired-mode'
if it is configured for deployment." t nil)

(autoload 'ssh-deploy-browse-remote-base-handler "ssh-deploy" "\
Open base path on remote host in `dired-mode'
if it is configured for deployment." t nil)

(autoload 'ssh-deploy-run-deploy-script-handler "ssh-deploy" "\
Run `ssh-deploy-script' with `funcall'." t nil)

(autoload 'ssh-deploy-add-after-save-hook "ssh-deploy" "\
Add the `after-save-hook'." nil nil)

(autoload 'ssh-deploy-add-find-file-hook "ssh-deploy" "\
Add the `find-file-hook'." nil nil)

(register-definition-prefixes "ssh-deploy" '("ssh-deploy-"))

;;;***

;;;### (autoloads nil "ssh-deploy-diff-mode" "ssh-deploy-diff-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ssh-deploy-diff-mode.el

(register-definition-prefixes "ssh-deploy-diff-mode" '("ssh-deploy-diff-mode"))

;;;***

;;;### (autoloads nil "ssh-deploy-hydra" "ssh-deploy-hydra.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from ssh-deploy-hydra.el

(autoload 'ssh-deploy-hydra "ssh-deploy-hydra" "\
Attach hydra at SHORTCUT.

\(fn SHORTCUT)" nil nil)

(register-definition-prefixes "ssh-deploy-hydra" '("ssh-deploy--fboundp-macro"))

;;;***

;;;### (autoloads nil nil ("ssh-deploy-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ssh-deploy-autoloads.el ends here
