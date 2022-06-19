;;; mood-line-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "mood-line" "mood-line.el" (0 0 0 0))
;;; Generated autoloads from mood-line.el

(defvar mood-line-mode nil "\
Non-nil if Mood-Line mode is enabled.
See the `mood-line-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `mood-line-mode'.")

(custom-autoload 'mood-line-mode "mood-line" nil)

(autoload 'mood-line-mode "mood-line" "\
Toggle mood-line on or off.

This is a global minor mode.  If called interactively, toggle the
`Mood-Line mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='mood-line-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "mood-line" '("mood-line-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8-emacs-unix
;; End:
;;; mood-line-autoloads.el ends here
