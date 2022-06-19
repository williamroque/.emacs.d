;;; evil-owl-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "evil-owl" "evil-owl.el" (0 0 0 0))
;;; Generated autoloads from evil-owl.el

(defvar evil-owl-mode nil "\
Non-nil if Evil-Owl mode is enabled.
See the `evil-owl-mode' command
for a description of this minor mode.")

(custom-autoload 'evil-owl-mode "evil-owl" nil)

(autoload 'evil-owl-mode "evil-owl" "\
A minor mode to preview marks and registers before using them.

This is a global minor mode.  If called interactively, toggle the
`Evil-Owl mode' mode.  If the prefix argument is positive, enable
the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='evil-owl-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "evil-owl" '("evil-owl-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8-emacs-unix
;; End:
;;; evil-owl-autoloads.el ends here
