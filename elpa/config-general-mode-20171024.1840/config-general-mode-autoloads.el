;;; config-general-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "config-general-mode" "config-general-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from config-general-mode.el

(defvar config-general-mode-map (let ((map (make-sparse-keymap))) (define-key map (kbd "C-c C-7") 'sh-backslash-region) (define-key map (kbd "C-c C-/") 'sh-backslash-region) (define-key map (kbd "C-c C-0") 'config-general-align-vars) (define-key map (kbd "C-c C-=") 'config-general-align-vars) (define-key map (kbd "C-c C-f") 'find-file-at-point) (define-key map (kbd "C-c C-t") 'config-general-toggle-flag) (define-key map (kbd "C-c C-j") 'imenu) (define-key map (kbd "<C-return>") 'config-general-do-electric-return) (define-key map (kbd "C-k") 'config-general-kill-line-or-block-or-continuation) (define-key map [remap delete-backward-char] 'backward-delete-char-untabify) map) "\
Keymap used in Config::General mode.")

(autoload 'config-general-mode "config-general-mode" "\
Config::General config file mode.

Config::General is a Perl module for parsing config files with
some enhanced features. `config-general-mode' makes it easier to
edit such config files with emacs.

It is based on `conf-mode' with the following features:

- good syntax highlighting for config files
- completion support with `<tab>' (using `dabbrev')
- imenu support for <blocks>
- electric paring mode (for quotes, parens, etc) enabled
- automatic indenting
- jump to include file with `<ret>'

Usage

Edit your config file as usual.  Use `<tab>' for completion of
values and variables.  Use \\[config-general-toggle-flag] to
toggle flags (like true to false).  Use
\\[config-general-align-vars] on a region to automatically align
on the `=` character. Use \\[sh-backslash-region] to break up a
region with long lines into shorter ones using backslash
notation.  Use \\[config-general-do-electric-return] to visit an
included file or (when not on a link) insert a new line below the
current one, indent and move point there. Use
\\[config-general-kill-line-or-block-or-continuation] to delete
lines, including continuation lines or whole blocks.  Use
\\[imenu] to jump to a block definition (same as using `imenu'
with the mouse).

\\{config-general-mode-map}

\(fn)" t nil)

(register-definition-prefixes "config-general-mode" '("config-general-"))

;;;***

;;;### (autoloads nil nil ("config-general-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; config-general-mode-autoloads.el ends here
