;;; caps-lock-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "caps-lock" "caps-lock.el" (0 0 0 0))
;;; Generated autoloads from caps-lock.el

(defvar caps-lock-mode nil "\
Non-nil if Caps-Lock mode is enabled.
See the `caps-lock-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `caps-lock-mode'.")

(custom-autoload 'caps-lock-mode "caps-lock" nil)

(autoload 'caps-lock-mode "caps-lock" "\
Make self-inserting keys invert the capitalization.

This is a global minor mode.  If called interactively, toggle the
`Caps-Lock mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='caps-lock-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "caps-lock" '("caps-lock-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8-emacs-unix
;; End:
;;; caps-lock-autoloads.el ends here
