;;; talonscript-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "talonscript-mode" "talonscript-mode.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from talonscript-mode.el

(autoload 'talonscript-mode "talonscript-mode" "\
Major mode for editing .talon files (for Talon Voice).

.talon commands are used to register commands for Talon's speech
recognition.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.talon\\'" . talonscript-mode))

(register-definition-prefixes "talonscript-mode" '("talonscript-font-lock-definitions"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8-emacs-unix
;; End:
;;; talonscript-mode-autoloads.el ends here
