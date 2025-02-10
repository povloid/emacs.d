(require 'package)
(setq package-archives

      ;; '(("elpa"     . "https://elpa.gnu.org/packages/")
      ;;   ("melpa-stable" . "https://stable.melpa.org/packages/")
      ;;   ("melpa"        . "https://melpa.org/packages/"))

      '(("elpa"     . "https://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://www.mirrorservice.org/sites/melpa.org/packages/")
        ("melpa"        . "https://www.mirrorservice.org/sites/stable.melpa.org/packages/"))

      package-archive-priorities
      '(
	("melpa-stable" . 10)
        ("elpa"         . 5)
        ("melpa"        . 0)
	))

(when (< emacs-major-version 27)
  (package-initialize))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(put 'use-package 'lisp-indent-function 1)

(setq use-package-always-ensure t)


