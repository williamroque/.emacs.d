(setq-default user-full-name "William Aguiar Roque")
(setq-default user-mail-address "william.aroque@gmail.com")

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/")
             '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (eval-when-compile (require 'use-package)))

(setq-default use-package-always-ensure t)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(setq-default gc-cons-threshold (* 8 1024 1024))

(setq default-directory (concat (getenv "HOME") "/"))

(setq-default inhibit-startup-screen t)

(with-eval-after-load "server"
  (defun server-execute (proc files nowait commands dontkill frame tty-name)
    ;; This is run from timers and process-filters, i.e. "asynchronously".
    ;; But w.r.t the user, this is not really asynchronous since the timer
    ;; is run after 0s and the process-filter is run in response to the
    ;; user running `emacsclient'.  So it is OK to override the
    ;; inhibit-quit flag, which is good since `commands' (as well as
    ;; find-file-noselect via the major-mode) can run arbitrary code,
    ;; including code that needs to wait.
    (with-local-quit
      (condition-case err
          (let ((buffers (server-visit-files files proc nowait)))
            (mapc 'funcall (nreverse commands))
            ;; If we were told only to open a new client, obey
            ;; `initial-buffer-choice' if it specifies a file
            ;; or a function.
            (unless (or files commands)
              (let ((buf
                     (cond ((stringp initial-buffer-choice)
                            (find-file-noselect initial-buffer-choice))
                           ((functionp initial-buffer-choice)
                            (funcall initial-buffer-choice)))))
                (switch-to-buffer
                 (if (buffer-live-p buf) buf (get-buffer-create "*scratch*"))
                 'norecord)))

            ;; Delete the client if necessary.
            (cond
             (nowait
              ;; Client requested nowait; return immediately.
              (server-log "Close nowait client" proc)
              (server-delete-client proc))
             ((and (not dontkill) (null buffers))
              ;; This client is empty; get rid of it immediately.
              (server-log "Close empty client" proc)
              (server-delete-client proc)))
            (cond
             ((or isearch-mode (minibufferp))
              nil)
             ((and frame (null buffers))
              (run-hooks 'server-after-make-frame-hook))
             ((not (null buffers))
              (run-hooks 'server-after-make-frame-hook)
              (server-switch-buffer (car buffers) nil (cdr (car files)))
              (run-hooks 'server-switch-hook)
              (unless nowait)))
            (when (and frame (null tty-name))
              (server-unselect-display frame)))
        ((quit error)
         (when (eq (car err) 'quit)
           (message "Quit emacsclient request"))
         (server-return-error proc err))))))

(setq-default auto-save-no-message t)

(setq-default custom-file "~/Desktop/custom.el")

(setq-default locale-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(defun shell-command-sentinel (process signal)
  (when (memq (process-status process) '(exit signal))
    (shell-command-set-point-after-cmd (process-buffer process))
    (let ((inhibit-message t))
      (message "%s: %s."
             (car (cdr (cdr (process-command process))))
             (substring signal 0 -1)))))

(defvar emacs-configuration-directory (file-name-directory (or load-file-name (buffer-file-name)))
  "Get directory of emacs configuration.")

(setq-default initial-scratch-message "")

(setq initial-major-mode 'org-mode)

(with-eval-after-load 'org
  (define-key lisp-interaction-mode-map (kbd "C-c t") 'org-mode)
  (define-key org-mode-map (kbd "C-c t") 'lisp-interaction-mode))

(defun kill-all-buffers-except-scratch ()
  "Kill all buffers except for *scratch*."
  (interactive)
  (mapc #'(lambda (name)
            (when (not (string-prefix-p "*scratch" name))
              (kill-buffer name)))
        (helm-buffer-list))
  (message "Killed all buffers except *scratch*."))

(use-package scratch
  :config
  (global-set-key (kbd "C-c s") #'scratch))

;; for debugging lists
(defun print-elements-of-list (list)
  "Print each element of LIST on a line of its own."
  (while list
    (print (car list))
    (setq list (cdr list))))


;; to add after index of list
(defun insert-after (lst index newelt)
  (if (or (null lst) (>= index (length lst)))
      (push newelt lst)
    (push newelt (cdr (nthcdr index lst))))
  lst)


;; easy way to wrap function for keyboard shortcut
(defun wrap-fun (fun &rest args)
  `(lambda ()
     (interactive)
     (apply #',fun ',args)))


(defun org-keyword-activep (keyword &optional default-value)
  (pcase (org-collect-keywords (list keyword))
    (`((,keyword . ,val))
     (not (equal (car val) "nil")))
    (- default-value)))


(defmacro with-suppressed-message (&rest body)
  "Suppress new messages temporarily in the echo area and the `*Messages*' buffer while BODY is evaluated."
  (declare (indent 0))
  (let ((message-log-max nil))
    `(with-temp-message (or (current-message) "") ,@body)))


(defmacro define-and-bind-text-object (key start-regex end-regex)
  (let ((inner-name (make-symbol "inner-name"))
        (outer-name (make-symbol "outer-name")))
    `(progn
       (evil-define-text-object ,inner-name (count &optional beg end type)
         (evil-select-paren ,start-regex ,end-regex beg end type count nil))
       (evil-define-text-object ,outer-name (count &optional beg end type)
         (evil-select-paren ,start-regex ,end-regex beg end type count t))
       (define-key evil-inner-text-objects-map ,key (quote ,inner-name))
       (define-key evil-outer-text-objects-map ,key (quote ,outer-name)))))


(use-package ox-json)

;; colorscheme stuff
(defvar ansi-color-names-vector
  ["#3c3836" "#fb4933" "#b8bb26" "#fabd2f" "#83a598" "#d3869b" "#8ec07c" "#ebdbb2"])
(defvar pdf-view-midnight-colors '("#fdf4c1" . "#1d2021"))


;; set theme
(use-package doom-themes
  :config
  (load-theme 'doom-tomorrow-night t))

(defconst color-background        "#121112")
(defconst color-light-background  "#1D1F21")
(defconst color-foreground        "#FDF4C1")
(defconst color-light-subdued     "#EBDBB2")
(defconst color-dark-subdued      "#313533")
(defconst color-darkish-subdued   "#515553")
(defconst color-medium-subdued    "#717171")
(defconst color-red               "#CC6666")
(defconst color-light-red         "#886950")
(defconst color-orange            "#FE8019")
(defconst color-light-orange      "#F0C674")
(defconst color-green             "#6CA17A")
(defconst color-light-green       "#B5BD68")
(defconst color-yellow            "#E0DBA9")
(defconst color-blue              "#81A2BE")
(defconst color-light-blue        "#41728E")
(defconst color-brown             "#BD9977")
(defconst color-dark-brown        "#605846")

(set-face-attribute 'default nil
                    :weight 'medium
                    :height 140
                    :width 'normal
                    :family "Victor Mono"
                    :background color-background
                    :foreground "#fdf4c1")


(set-face-attribute 'fixed-pitch nil
                    :family "Victor Mono"
                    :height 140)


(set-face-attribute 'variable-pitch nil
                    :family "Optima"
                    :height 160)


(set-face-attribute 'font-lock-preprocessor-face nil
                    :inherit 'bold
                    :foreground color-light-red)


(set-face-attribute 'mode-line nil
                    :box nil
                    :background "#121112"
                    :foreground color-light-subdued)


(set-face-attribute 'mode-line-buffer-id nil
                    :weight 'bold)


(set-face-attribute 'mode-line-inactive nil
                    :box nil
                    :background color-light-background
                    :foreground color-light-subdued)


(set-face-attribute 'font-lock-keyword-face nil
                    :weight 'bold
                    :foreground color-red)


(set-face-attribute 'font-lock-comment-face nil
                    :slant 'italic)


(set-face-attribute 'font-lock-function-name-face nil
                    :foreground color-green)


(set-face-attribute 'font-lock-constant-face nil
                    :foreground color-red)


(set-face-attribute 'font-lock-variable-name-face nil
                    :foreground color-yellow)


(set-face-attribute 'font-lock-type-face nil
                    :foreground color-green)


(set-face-attribute 'lazy-highlight nil 
                    :weight 'normal
                    :background color-light-blue
                    :foreground color-foreground)


(set-face-attribute 'line-number nil
                    :background color-background)


(set-face-attribute 'line-number-current-line nil
                    :background color-background
                    :foreground color-orange)


(set-face-attribute 'shadow nil
                    :foreground color-dark-subdued)


(with-eval-after-load 'hl-line
  (set-face-attribute 'hl-line nil
                      :background "#1e1e1e"))

(defun toggle-line-numbers ()
  "Toggle line numbers."
  (interactive)
  (setq-local display-line-numbers
              (if (equal display-line-numbers nil) 'relative nil)))


(add-hook 'prog-mode-hook #'toggle-line-numbers)
(add-hook 'LaTeX-mode-hook #'toggle-line-numbers)

(defvar began-line-toggle nil
  "Whether line numbers have currently been toggled.")


(defun cautious-line-toggle ()
  "Toggle line numbers temporarily if began-line-toggle is nil"
  (interactive)
  (if (and (not began-line-toggle) (not (derived-mode-p 'prog-mode)))
      (progn
        (toggle-line-numbers)
        (setq began-line-toggle t))))


;; show line numbers when using numerical prefix 
;; (add-hook 'prefix-command-preserve-state-hook #'cautious-line-toggle)


(add-hook 'pre-command-hook #'(lambda ()
                                (interactive)
                                (if began-line-toggle
                                    (progn
                                      (toggle-line-numbers)
                                      (setq began-line-toggle nil)))))

(add-hook 'term-mode-hook (lambda () (setq-local display-line-numbers nil)))

;; remove menu and tool bar
(tool-bar-mode -1)
(menu-bar-mode -1)


;; remove scrollbar
(toggle-scroll-bar -1)


;; remove title
(defun erase-title ()
  (setq frame-title-format
        (if (equal frame-title-format "") "\n" "")))

(run-with-timer 0 5 #'erase-title)

(setq-default frame-resize-pixelwise t)

(fringe-mode 0)
(setq-default left-margin-width 2)
(setq-default right-margin-width 2)

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

(setq-default ns-use-proxy-icon nil)

(if (window-system) (set-frame-size (selected-frame) 110 30))

(add-hook 'server-switch-hook (lambda () (select-frame-set-input-focus (selected-frame))))

(defvar default-frame-height 30)
(defvar default-frame-width 110)

(defvar default-frame-pixel-height nil)
(defvar default-frame-pixel-width nil)


(add-to-list 'after-make-frame-functions
             #'(lambda (frame)
                 (when (or (not default-frame-pixel-height) (not default-frame-width))
                   (setq-default default-frame-pixel-height (frame-outer-height frame))
                   (setq-default default-frame-pixel-width (frame-outer-width frame)))))


(add-hook 'before-make-frame-hook
          #'(lambda ()
              (add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))
              (add-to-list 'default-frame-alist `(height . ,default-frame-height))
              (add-to-list 'default-frame-alist `(width  . ,default-frame-width))))

(defvar should-center-frame t)


(defun center-frame (frame)
  (if should-center-frame
      (modify-frame-parameters
       frame '((user-position . t) (top . 0.5) (left . 0.5)))))

(add-to-list 'after-make-frame-functions #'center-frame)

(global-set-key (kbd "M-w") 'evil-quit)

(define-key global-map (kbd "M-t") #'transpose-frame)

(global-set-key (kbd "M-h") 'evil-window-left)
(global-set-key (kbd "M-j") 'evil-window-down)
(global-set-key (kbd "M-k") 'evil-window-up)
(global-set-key (kbd "M-l") 'evil-window-right)

(setq-default mouse-autoselect-window t)

(setq-default split-width-threshold 0)
(setq-default split-height-threshold nil)

(use-package transpose-frame)

(global-set-key (kbd "C-c w") #'evil-window-set-width)
(global-set-key (kbd "C-c h") #'evil-window-set-height)

(use-package perspective
  :config
  (setq-default persp-suppress-no-prefix-key-warning t)

  (persp-mode))

(use-package mood-line
  :config
  (mood-line-mode)
  
  (setq-default display-time-default-load-average nil)
  (setq-default display-time-day-and-date t)
  (setq-default display-time-format " %H:%M")
  (display-time)
  (set-face-attribute 'mood-line-buffer-name nil
                      :inherit 'mode-line-buffer-id
                      :foreground "#ebdbb2")
  
  
  (set-face-attribute 'mood-line-major-mode nil
                      :inherit 'bold
                      :foreground "#ebdbb2")
  
  
  (set-face-attribute 'mood-line-modified nil
                      :foreground "#ebdbb2")
  
  
  (set-face-attribute 'mood-line-status-error nil
                      :foreground "#ebdbb2")
  
  
  (set-face-attribute 'mood-line-status-info nil
                      :foreground "#ebdbb2")
  
  
  (set-face-attribute 'mood-line-status-neutral nil
                      :foreground "#ebdbb2")
  
  
  (set-face-attribute 'mood-line-status-success nil
                      :foreground "#ebdbb2")
  
  
  (set-face-attribute 'mood-line-status-warning nil
                      :foreground "#ebdbb2")
  
  
  (set-face-attribute 'mood-line-unimportant nil
                      :foreground "#ebdbb2"))

(let ((alist `((?! . ,(regexp-opt '("!!" "!=" "!==")))
               (?# . ,(regexp-opt '("##" "###" "####" "#(" "#?" "#[" "#_" "#_(" "#{")))
               (?$ . ,(regexp-opt '("$>")))
               (?% . ,(regexp-opt '("%%")))
               (?& . ,(regexp-opt '("&&")))
               (?* . ,(regexp-opt '("*" "**" "***" "**/" "*/" "*>")))
               (?+ . ,(regexp-opt '("+" "++" "+++" "+>")))
               (?- . ,(regexp-opt '("--" "---" "-->" "-<" "-<<" "->" "->>" "-}" "-~")))
               (?. . ,(regexp-opt '(".-" ".." "..." "..<" ".=")))
               (?/ . ,(regexp-opt '("/*" "/**" "//" "///" "/=" "/==" "/>")))
               (?: . ,(regexp-opt '(":" "::" ":::" ":=")))
               (?\; . ,(regexp-opt '(";;")))
               (?< . ,(regexp-opt '("<!--" "<$" "<$>" "<*" "<*>" "<+" "<+>" "<-" "<--" "<->" "</" "</>" "<<" "<<-" "<<<" "<<=" "<=" "<=" "<=<" "<==" "<=>" "<>" "<|" "<|>" "<~" "<~~")))
               (?= . ,(regexp-opt '("=/=" "=:=" "=<<" "==" "===" "==>" "=>" "=>>")))
               (?> . ,(regexp-opt '(">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>")))
               (?? . ,(regexp-opt '("??" "?=")))
               (?\[ . ,(regexp-opt '("[]")))
               (?\\ . ,(regexp-opt '("\\\\" "\\\\\\")))
               (?^ . ,(regexp-opt '("^=")))
               (?w . ,(regexp-opt '("www")))
               (?x . ,(regexp-opt '("x")))
               (?{ . ,(regexp-opt '("{-")))
               (?| . ,(regexp-opt '("|=" "|>" "||" "||=")))
               (?~ . ,(regexp-opt '("~-" "~=" "~>" "~@" "~~" "~~>")))
               ;;;(?F . ,(regexp-opt '("F_vec")))
               )))
  (dolist (char-regexp alist)
    (set-char-table-range composition-function-table (car char-regexp)
                          `([,(cdr char-regexp) 0 font-shape-gstring]))))

(add-hook 'vterm-mode-hook (lambda ()
                             (set-fontset-font (face-attribute 'default :fontset)
                                               '(#x0370 . #x03FF) (font-spec :family "Fira Code") nil 'prepend)))

(defun add-visual-replacement (from to)
  "Make `prettify-symbols-mode' replace string FROM with string TO.

Updates `prettify-symbols-alist'.  You may need to toggle
`prettify-symbols-mode' to make the changes take effect.

Each character of TO is vertically aligned using the baseline,
such that base-left of the character is aligned with base-right
of the preceding character.  Refer to `reference-point-alist'
for more information."
  (push (cons from (let ((composition nil))
                     (dolist (char (string-to-list to)
                                   (nreverse (cdr composition)))
                       (push char composition)
                       (push '(Br . Bl) composition))))
        prettify-symbols-alist))


(add-hook 'org-mode-hook (lambda ()
                           (add-visual-replacement "---" "──")

                           (push '("\\sqrt" . "√") prettify-symbols-alist)

                           (push '("\\text" . "​") prettify-symbols-alist)

                           (push '("\\left(" . "(") prettify-symbols-alist)
                           (push '("\\right)" . ")") prettify-symbols-alist)
                           (add-visual-replacement "\\right)^2" ")²")
                           (add-visual-replacement "\\right)^3" ")³")

                           (push '("\\left|" . "|") prettify-symbols-alist)
                           (push '("\\right|" . "|") prettify-symbols-alist)
                           (add-visual-replacement "\\right|^2" "|²")
                           (add-visual-replacement "\\right|^3" "|³")

                           (push '("\\left[" .  "[") prettify-symbols-alist)
                           (push '("\\right]" . "]") prettify-symbols-alist)
                           (add-visual-replacement "\\right]^2" "]²")
                           (add-visual-replacement "\\right]^3" "]³")

                           (add-visual-replacement "\\left\\Vert" "||")
                           (add-visual-replacement "\\right\\Vert" "||")

                           (push '("\\vecb{a}" . "𝒂") prettify-symbols-alist)
                           (push '("\\vecb{b}" . "𝒃") prettify-symbols-alist)
                           (push '("\\vecb{c}" . "𝒄") prettify-symbols-alist)
                           (push '("\\vecb{u}" . "𝒖") prettify-symbols-alist)
                           (push '("\\vecb{v}" . "𝒗") prettify-symbols-alist)
                           (push '("\\vecb{r}" . "𝒓") prettify-symbols-alist)

                           (push '("\\left\\langle" .  "〈") prettify-symbols-alist)
                           (push '("\\right\\rangle" .  "〉") prettify-symbols-alist)
                           (push '("[ ]" .  "☐") prettify-symbols-alist)
                           (push '("[X]" . "☑" ) prettify-symbols-alist)
                           (push '("[-]" . "❍" ) prettify-symbols-alist)
                           (push '("#+BEGIN_SRC" . "↦" ) prettify-symbols-alist)
                           (push '("#+END_SRC" . "⇤" ) prettify-symbols-alist)
                           (push '("#+BEGIN_EXAMPLE" . "↦" ) prettify-symbols-alist)
                           (push '("#+END_EXAMPLE" . "⇤" ) prettify-symbols-alist)
                           (push '("#+BEGIN_QUOTE" . "↦" ) prettify-symbols-alist)
                           (push '("#+END_QUOTE" . "⇤" ) prettify-symbols-alist)
                           (push '("#+BEGIN_VERSE" . "↦" ) prettify-symbols-alist)
                           (push '("#+END_VERSE" . "⇤" ) prettify-symbols-alist)
                           (push '("#+begin_verse" . "↦" ) prettify-symbols-alist)
                           (push '("#+end_verse" . "⇤" ) prettify-symbols-alist)
                           (push '("#+begin_quote" . "↦" ) prettify-symbols-alist)
                           (push '("#+end_quote" . "⇤" ) prettify-symbols-alist)
                           (push '("#+begin_example" . "↦" ) prettify-symbols-alist)
                           (push '("#+end_example" . "⇤" ) prettify-symbols-alist)
                           (push '("#+begin_src" . "↦" ) prettify-symbols-alist)
                           (push '("#+end_src" . "⇤" ) prettify-symbols-alist)
                           (push '("#+title: " . "​" ) prettify-symbols-alist)
                           (prettify-symbols-mode)))

(use-package evil
  :init
  (setq-default evil-want-keybinding nil)         ; evil-collection compatibility
  (setq-default evil-search-module 'evil-search)  ; use vim-style searching
  (setq-default evil-undo-system 'undo-tree)      ; use better undo system based on vim
  (setq-default evil-vsplit-window-right t)       ; split right
  (setq-default evil-split-window-below t)        ; split down
  
  
  ;; new digraphs
  (setq-default evil-digraphs-table-user '(((?r ?a) . ?\x2192)
                                           ((?  ? ) . ?\x200b)
                                           ((?t ?h) . ?\x03b8)
                                           ((?p ?i) . ?\x03c0)
                                           ((?p ?h) . ?\x03c6)
                                           ((?l ?a) . ?\x03bb)
                                           ((?o ?m) . ?\x03c9)
                                           ((?n ?u) . ?\x03bd)
                                           ((?b ?e) . ?\x03b2)
                                           ((?g ?a) . ?\x03b3)
                                           ((?t ?a) . ?\x03c4)
                                           ((?a ?l) . ?\x03b1)
                                           ((?r ?h) . ?\x03c1)
                                           ((?m ?m) . ?\x03bc)
                                           ((?. ?.) . ?\x0307)))
  
  
  ;; change cursor colors based on mode (state)
  (setq-default evil-emacs-state-cursor '("white" box))
  (setq-default evil-normal-state-cursor '("#fdf4c1" box))
  (setq-default evil-visual-state-cursor '("orange" box))
  (setq-default evil-insert-state-cursor '("#fdf4c1" bar))
  (setq-default evil-replace-state-cursor '("orange" hollow))
  (setq-default evil-operator-state-cursor '("orange" hollow))
  
  
  ;; set state change messages
  (setq-default evil-insert-state-message "")
  (setq-default evil-visual-state-message "")

  :config
  (set-face-attribute 'evil-ex-search nil
                      :background "#81a2be"
                      :foreground "#0d0d0d"
                      :weight 'normal)
  ;; I may eventually move this out
  (defun kill-all-other-buffers ()
    "Kill all buffers except for this and *scratch*."
    (interactive)
    (mapc 'kill-buffer 
          (delete (current-buffer)
                  (delete (get-buffer "*scratch*") (buffer-list))))
    (message "Killed all buffers except this and *scratch*."))
  
  
  (use-package evil-leader
    :init
    (setq-default evil-leader/in-all-states 1)
  
    :config
    (global-evil-leader-mode)
  
  
    ;; set leader
    (evil-leader/set-leader "SPC")
  
  
    ;; convenient saving
    (evil-leader/set-key "f" #'(lambda ()
                                 (interactive)
                                 (cond
                                  ((string-prefix-p "*scratch" (buffer-name)) (message "(No changes need to be saved)"))
                                  ((equal major-mode 'wdired-mode) (wdired-finish-edit))
                                  ((equal major-mode 'org-agenda-mode)
                                   (org-save-all-org-buffers)
                                   (message "Org buffers saved.")
                                   (progenda-publish-schedule))
                                  (t
                                   (if (buffer-modified-p)
                                       (message "Wrote %s." (if (null (buffer-file-name))
                                                                "file"
                                                              (file-name-nondirectory (buffer-file-name))))
                                     (message "(No changes need to be saved)"))
                                   (with-suppressed-message
                                       (call-interactively #'save-buffer))))))
  
  
    ;; convenient exiting
    (evil-leader/set-key "e" #'(lambda ()
                                 (interactive)
                                 (if (equal major-mode 'org-agenda-mode)
                                     (org-agenda-exit)
                                   (kill-this-buffer))))
    (evil-leader/set-key "E" #'kill-all-other-buffers)
    (evil-leader/set-key "x" #'(lambda ()
                                 (interactive)
                                 (save-buffer)
                                 (kill-this-buffer)))
  
  
    ;; delete current file
    (evil-leader/set-key "D" #'(lambda ()
                                 (interactive)
                                 (if (yes-or-no-p "Really truly really delete this file? ")
                                     (delete-file (buffer-file-name)))))
  
  
    ;; display current time
    (evil-leader/set-key "j" #'(lambda ()
                                 (interactive)
                                 (message (format-time-string "%H:%M:%S ― %d %b, %Y"))))
  
  
    ;; convenient way to evaluate buffer
    (evil-leader/set-key "v" #'eval-buffer)
  
  
    ;; convenient terminal opening
    (evil-leader/set-key "t" #'(lambda ()
                                 (interactive)
                                 (vterm (format
                                         "*vterm-%s*"
                                         (make-temp-name "")))))
  
  
    (evil-leader/set-key "DEL" #'calendar)
    (evil-leader/set-key "c" #'org-capture)
    (evil-leader/set-key "d" #'osx-dictionary-search-input)
    ;; open dired at cwd or go up directory
    (evil-leader/set-key "I" #'(lambda ()
                                 (interactive)
                                 (if (equal major-mode 'dired-mode)
                                     (dired-up-directory)
                                   (dired "."))))
    
    
    ;; close current buffer and open dired at cwd or go up directory
    (evil-leader/set-key "i" #'(lambda ()
                                 (interactive)
                                 (if (not (equal dired-directory "/"))
                                     (let ((old-buffer (current-buffer)))
                                       (if (equal major-mode 'dired-mode)
                                           (progn
                                             (dired-up-directory)
                                             (kill-buffer old-buffer))
                                         (dired "."))))))
    
    
    ;; open marked files in dired
    (evil-leader/set-key "w" #'(lambda (open-all-marked)
                                 (interactive "P")
                                 (if (null open-all-marked)
                                     (shell-command (format "open '%s'" buffer-file-name))
                                   (let ((files (get-all-marked-files-and-unmark)))
                                     (mapc #'browse-url files)
                                     (message "Unmarked %d files." (length files))))))
    
    
    ;; open desktop
    (evil-leader/set-key "?" #'(lambda ()
                                 (interactive)
                                 (find-file (expand-file-name "~/Desktop/"))))
    
    
    ;; open home
    (evil-leader/set-key "/" #'(lambda ()
                                 (interactive)
                                 (find-file (expand-file-name "~/"))))
    ;; zoom in/out
    (evil-leader/set-key "=" #'text-scale-adjust)
    
    
    ;; toggle sentence split
    (evil-leader/set-key "s" #'toggle-sentence-split)
    
    
    ;; switch input method
    (evil-leader/set-key "\\" #'toggle-input-method)
    
    
    ;; count occurrences
    (evil-leader/set-key "F" #'count-matches)
    (evil-leader/set-key (kbd "m") 'magit)
    ;; open the minibuffer
    (evil-leader/set-key "k" 'helm-mini)
    
    
    ;; open command minibuffer
    (evil-leader/set-key ";" 'helm-M-x)
    
    
    ;; for helm-ag
    (evil-leader/set-key "g" 'helm-ag)
    (evil-leader/set-key "N" #'cautious-line-toggle)
    (evil-leader/set-key "R" 'reveal-in-osx-finder)
    ;; open send mail buffer
    (evil-leader/set-key "M" #'(lambda ()
                                 (interactive)
                                 (switch-to-buffer "*mail*" nil t)
                                 (mail-mode)
                                 (company-mode -1)
                                 (auto-fill-mode -1)
                                 (distraction-free t)
                                 (evil-insert-state)
                                 (yas-minor-mode)
                                 (yas-expand-snippet
                                  "From: William Aguiar Roque <william.aroque@gmail.com>
    To: $1
    Subject: $2
    --text follows this line--
    $0")))
    
    ;; open read mail buffer
    (evil-leader/set-key "1" #'(lambda (refresh-email)
                                 (interactive "P")
                                 (when refresh-email
                                   (message "Refreshing email...")
                                   (shell-command-to-string "mbsync gmail; notmuch new"))
                                 (notmuch)))
    (evil-leader/set-key "u" #'distraction-free)
    ;; open agenda without calendar; the proper way to do this is with a custom view
    (evil-leader/set-key "a" #'(lambda (dedicate-monitor)
                                 (interactive "P")
                                 (if dedicate-monitor
                                     (dedicate-monitor-to-org-agenda)
                                   (let ((org-agenda-category-filter-preset '("-Calendar")))
                                     (org-agenda-list)))))
    
    
    (evil-leader/set-key "A" #'(lambda (sync)
                                 (interactive "P")
                                 (if sync
                                     (shell-command-to-string "/Users/jetblack/cbin/sync-calendar"))
                                 (org-agenda-list)))
    
    
    (evil-leader/set-key "G" #'progenda-publish-schedule)
    ;; export org/latex file as HTML
    (evil-leader/set-key "h" #'(lambda ()
                                 (interactive)
                                 (if (equal major-mode 'org-mode)
                                     (export-and-open-html)
                                   (if (equal major-mode 'latex-mode)
                                       (call-interactively 'TeX-command-run-all)
                                     (browse-url-of-file)))))
    
    
    (defvar org-export-html-live-online nil
      "Whether the live Org HTML export server is on.")
    
    
    (defun org-export-html-toggle-live-server ()
      "Toggle the liver Org HTMl export server."
      (if org-export-html-live-online
          (progn
            (kill-process "org-export-html-live-server")
            (message "Killed live export server.")
            (setq org-export-html-live-online nil))
        (let ((html-path (format "%s.html" (file-name-base (buffer-file-name)))))
          (message html-path)
          (set-process-sentinel (start-process "org-export-html-live-server"
                                               "*Org Live Export*"
                                               "/Users/jetblack/.npm-global/bin/reload"
                                               "-b"
                                               "-s"
                                               html-path)
                                #'(lambda (process event)
                                    (when (memq (process-status process) '(exit signal))
                                      (kill-buffer "*Org Live Export*")))))
        (message "Started live export server.")
        (setq org-export-html-live-online t)))
    
    
    (evil-leader/set-key "H" #'(lambda (start-server)
                                 (interactive "P")
                                 (if start-server
                                     (org-export-html-toggle-live-server)
                                   (when (equal major-mode 'org-mode)
                                     (org-html-export-to-html)
                                     (shell-command (format "open -a Vivaldi" ))))))
    
    
    ;; export org/latex file as PDF
    (defvar org-pdf-separate-window '())
    
    (add-hook 'kill-buffer-hook #'(lambda ()
                                    (if (member (current-buffer) org-pdf-separate-window)
                                        (setq org-pdf-separate-window (delete (current-buffer) org-pdf-separate-window)))))
    
    (add-hook 'after-save-hook #'(lambda ()
                                   (if (and (equal major-mode 'org-mode)
                                            (member (current-buffer) org-pdf-separate-window))
                                       (org-export-pdf-update))))
    
    (defun org-latex-preview-pdf (separate-window)
      (interactive "P")
      (cond ((equal separate-window 1)
             (if (member (current-buffer) org-pdf-separate-window)
                 (progn
                   (setq org-pdf-separate-window (delete (current-buffer) org-pdf-separate-window))
                   (message "Stopped live compilation for this buffer."))
               (message "Started live compilation for this buffer.")
               (push (current-buffer) org-pdf-separate-window)))
            ((equal separate-window 2)
             (expand-org-latex-workspace))
            ((equal major-mode 'org-mode)
             (export-and-open-pdf separate-window))
            ((equal major-mode 'latex-mode)
             (call-interactively 'TeX-command-run-all))
            (t (browse-url-of-file))))
    
    (evil-leader/set-key "P" #'org-latex-preview-pdf)
    
    ;; fill paragraph
    (evil-leader/set-key "p" #'fill-paragraph)
    
    
    ;; open links
    (evil-leader/set-key "o" #'(lambda ()
                                 (interactive)
                                 (cond
                                  ((equal major-mode 'org-mode) (org-open-at-point))
                                  ((equal major-mode 'notmuch-show-mode) (shr-browse-url)))))
    
    
    ;; convenient way to toggle latex preview in org-mode
    (evil-leader/set-key "l" #'org-latex-preview)
    
    
    ;; convenient way to execute all code blocks at once
    (evil-leader/set-key "\"" 'org-babel-execute-buffer)
    
    
    ;; edit in separate buffer
    (evil-leader/set-key "`" (lambda ()
                               (interactive)
                               (if (not (equal major-mode 'org-mode))
                                   (org-edit-src-exit)
                                 (org-edit-special))))
    
    
    ;; org babel tangle
    (evil-leader/set-key "SPC" 'org-babel-tangle)
    ;; create scratch buffer
    (evil-leader/set-key "b" #'(lambda () (interactive) (switch-to-buffer "*scratch*")))
    
    
    ;; make the scratch buffer the only one
    (evil-leader/set-key "B" #'kill-all-buffers-except-scratch)
    ;; run scripts
    (evil-leader/set-key "," #'run-script-without-args)
    (evil-leader/set-key "'" #'run-script-without-args)
    
    
    ;; run scripts with arguments
    (evil-leader/set-key "." #'run-script-with-args)
    ;; visit snippet
    (evil-leader/set-key "V" #'yas-visit-snippet-file)
    
    
    ;; new snippet
    (evil-leader/set-key "n" #'yas-new-snippet)
    ;; start flyspell
    (evil-leader/set-key "S" #'(lambda ()
                                 (interactive)
                                 (if (and (derived-mode-p 'prog-mode) (not (equal (buffer-name) "*scratch*")))
                                     (flyspell-prog-mode)
                                   (progn
                                     (flyspell-mode)
                                     (if (equal (buffer-name) "*scratch*")
                                         (flycheck-mode -1))))))
    
    
    ;; cycle through ispell languages
    (evil-leader/set-key "C" #'cycle-ispell-languages)
    
    
    ;; save spelling
    (evil-leader/set-key "[" #'(lambda ()
                                 (interactive)
                                 (let ((current-location (point))
                                       (word (flyspell-get-word)))
                                   (when (consp word)
                                     (flyspell-do-correct 'save nil (car word) current-location (cadr word) (caddr word) current-location))
                                   (setq ispell-pdict-modified-p nil))))
    (evil-leader/set-key "r" #'edit-next-placeholder)
    ;; open non-fullscreen window
    (evil-leader/set-key (kbd "-") #'(lambda ()
                                       (interactive)
                                       (make-frame '((user-position . t) (top . 0.5) (left . 0.5)))
                                       (run-at-time .2 nil (lambda ()
                                                             (set-frame-parameter nil 'fullscreen nil)
                                                             (center-frame (selected-frame))))))
    
    
    ;; open fullscreen window
    (evil-leader/set-key (kbd "_") #'(lambda ()
                                       (interactive)
                                       (make-frame '((fullscreen . fullboth)))))
    
    
    ;; switch frames easily
    (evil-leader/set-key "O" #'other-frame)
    
    
    ;; balance out window sizes
    (evil-leader/set-key "W" #'balance-windows)
    
    
    ;; recenter frame
    (evil-leader/set-key "0" #'(lambda ()
                                 (interactive)
                                 (set-frame-parameter nil 'width default-frame-width)
                                 (set-frame-parameter nil 'height default-frame-height)
                                 (center-frame (selected-frame))))
    
    
    ;; quit emacs -- shouldn't technically be here
    (evil-leader/set-key (kbd "q") #'(lambda ()
                                       (interactive)
                                       (kill-all-buffers-except-scratch)
                                       (when (yes-or-no-p "Truly really truly quit Emacs? ")
                                         (kill-emacs))))
    
    
    ;; toggle fullscreen more easily
    (evil-leader/set-key (kbd "RET") #'toggle-frame-fullscreen)
    
    
    ;; swap window sides
    (evil-leader/set-key (kbd "y") #'window-swap-states))
  (evil-mode 1)
  
  
  ;; use vim-style paragraphs
  (defadvice forward-evil-paragraph (around default-values activate)
    (let ((paragraph-start (default-value 'paragraph-start))
          (paragraph-separate (default-value 'paragraph-separate)))
      ad-do-it))
  (use-package key-chord
    :load-path "lisp/"
    :config
    (setq-default key-chord-two-keys-delay 0.2)
  
  
    ;; redefine chord definition so that keys can't be reversed
    (defun key-chord-define (keymap keys command)
      "Define in KEYMAP, a key-chord of the two keys in KEYS starting a COMMAND.
  
  KEYS can be a string or a vector of two elements. Currently only
  elements that corresponds to ascii codes in the range 32 to 126
  can be used.
  
  COMMAND can be an interactive function, a string, or nil.
  If COMMAND is nil, the key-chord is removed."
      (if (/= 2 (length keys))
          (error "Key-chord keys must have two elements"))
      ;; Exotic chars in a string are >255 but define-key wants 128..255
      ;; for those.
      (let ((key1 (logand 255 (aref keys 0)))
            (key2 (logand 255 (aref keys 1))))
        (define-key keymap (vector 'key-chord key1 key2) command)))
  
  
    ;; bind jk to normal state
    (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
    (key-chord-mode 1)
  
  
    (add-hook 'input-method-deactivate-hook #'(lambda () (key-chord-mode 1))))
  
  
  ;; make sure C-j executes in the command window
  (define-key evil-command-window-mode-map (kbd "C-j") 'evil-command-window-execute)
  
  
  ;; remap Y to sensible function
  (define-key evil-normal-state-map (kbd "Y") #'(lambda ()
                                                  (interactive)
                                                  (evil-yank (point) (line-end-position))))
  
  
  ;; convenient macro execution
  (define-key evil-normal-state-map (kbd "RET") (kbd "@q"))
  (define-key evil-visual-state-map (kbd "RET") (kbd "@q"))
  
  
  ;; select contents of last paste
  (defun evil-select-pasted ()
    (interactive)
    (let ((start-marker (evil-get-marker ?\[))
          (end-marker (evil-get-marker ?\])))
      (evil-visual-select start-marker end-marker)))
  
  (evil-define-key 'normal global-map (kbd "g b") #'evil-select-pasted)
  
  
  ;; increment/decrement at point
  (use-package evil-numbers
    :config
    (define-key global-map (kbd "C-9") #'evil-numbers/dec-at-pt)
    (define-key global-map (kbd "C-0") #'evil-numbers/inc-at-pt))
  
  
  ;; go to beginning and end of visual line by default
  (evil-define-key 'normal global-map (kbd "0") #'evil-beginning-of-visual-line)
  (evil-define-key 'normal global-map (kbd "$") #'evil-end-of-visual-line)
  
  
  ;; convenient remap for ex state
  (evil-define-key nil evil-normal-state-map ";" 'evil-ex)
  (evil-define-key nil evil-visual-state-map ";" 'evil-ex)
  (evil-define-key 'normal 'dired-mode-map ";" 'evil-ex)
  
  
  ;; define a prefix key for perspective
  (define-key evil-normal-state-map (kbd "g p") 'perspective-map))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-args
  :config
  (define-key evil-inner-text-objects-map "g" 'evil-inner-arg)
  (define-key evil-outer-text-objects-map "g" 'evil-outer-arg)
  (define-key evil-normal-state-map "L" 'evil-forward-arg)
  (define-key evil-normal-state-map "H" 'evil-backward-arg))

(defun add-general-embrace-pairs ()
  (embrace-add-pair ?\( "( " " )")
  (embrace-add-pair ?\) "(" ")")
  (embrace-add-pair ?\[ "[ " " ]")
  (embrace-add-pair ?\] "[" "]")
  (embrace-add-pair ?\{ "{ " " }")
  (embrace-add-pair ?\} "{" "}"))


(use-package evil-embrace
  :ensure t
  :config
  (evil-embrace-enable-evil-surround-integration)

  (add-hook 'prog-mode-hook #'add-general-embrace-pairs)
  (add-hook 'text-mode-hook #'add-general-embrace-pairs)

  (defun embrace-with-latex-environment ()
    (let ((cname (or (read-string "Environment: ") "")))
      (cons (format "\\begin{%s}\n" cname) (format "\n\\end{%s}" cname))))
  
  
  (defun embrace-with-latex-command ()
    (let ((cname (read-string "Command: ")))
      (cons (format "\\%s{" (or cname "")) "}")))
  
  
  (add-hook 'LaTeX-mode-hook (lambda ()
                               (embrace-add-pair ?i "\\emph{" "}")
                               (embrace-add-pair ?m "$" "$")
                               (embrace-add-pair ?\[ "\\left[" "\\right]")
                               (embrace-add-pair ?\( "\\left(" "\\right)")
                               (embrace-add-pair ?\] "\\left[ " " \\right]")
                               (embrace-add-pair ?\) "\\left( " " \\right)")
                               (embrace-add-pair-regexp ?c "\\[^{]*?{" "}"
                                                        #'embrace-with-latex-command
                                                        (embrace-build-help "\\command{" "}"))
                               (embrace-add-pair-regexp ?e "\\begin{[^{]*?}" "\\end{[^{]*?}"
                                                        #'embrace-with-latex-environment
                                                        (embrace-build-help "\\begin{environment}" "\\end{environment}"))))
  (add-hook 'org-mode-hook #'(lambda ()
                               (delete ?b evil-embrace-evil-surround-keys)
  
                               (embrace-add-pair ?i "/" "/")
                               (embrace-add-pair ?b "*" "*")
                               (embrace-add-pair ?u "_" "_")))
  
  
  
  (use-package evil-tex
    :config
    (add-hook 'org-mode-hook #'evil-tex-mode)))

(use-package evil-owl
  :config
  (setq evil-owl-max-string-length 500)
  (add-to-list 'display-buffer-alist
               '("*evil-owl*"
                 (display-buffer-in-side-window)
                 (side . bottom)
                 (window-height . 0.3)))
  (evil-owl-mode))

(use-package evil-commentary
  :config

  (evil-commentary-mode))

(use-package evil-little-word
  :load-path "lisp/"
  :config
  (define-key evil-normal-state-map (kbd "w") 'evil-forward-little-word-begin)
  (define-key evil-normal-state-map (kbd "b") 'evil-backward-little-word-begin)
  (define-key evil-operator-state-map (kbd "w") 'evil-forward-little-word-begin)
  (define-key evil-operator-state-map (kbd "b") 'evil-backward-little-word-begin)
  (define-key evil-visual-state-map (kbd "w") 'evil-forward-little-word-begin)
  (define-key evil-visual-state-map (kbd "b") 'evil-backward-little-word-begin)
  (define-key evil-visual-state-map (kbd "i w") 'evil-inner-little-word))

(use-package evil-indent-plus
  :config
  (evil-indent-plus-default-bindings))

;; better line motion
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
(define-key evil-visual-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-visual-state-map (kbd "k") 'evil-previous-visual-line)


;; repeat find char remap
(define-key evil-normal-state-map (kbd "-") 'evil-repeat-find-char)

(evil-define-key 'normal global-map (kbd "J") #'(lambda ()
                                                  (interactive)
                                                  (scroll-up 3)))
(evil-define-key 'normal global-map (kbd "K") #'(lambda ()
                                                  (interactive)
                                                  (scroll-down 3)))

(evil-define-key 'visual global-map (kbd "J") #'(lambda ()
                                                  (interactive)
                                                  (scroll-up 3)))
(evil-define-key 'visual global-map (kbd "K") #'(lambda ()
                                                  (interactive)
                                                  (scroll-down 3)))

;; smooth scrolling
(setq-default scroll-margin 5)
(setq-default scroll-conservatively 9999)
(setq-default scroll-step 1)


;; allow horizontal scrolling with mouse
(setq-default mouse-wheel-tilt-scroll t)
(setq-default mouse-wheel-flip-direction t)

;; use backslash to end search highlighting
(define-key evil-normal-state-map (kbd "\\") #'evil-ex-nohighlight)


;; use C-j in isearch
(define-key isearch-mode-map (kbd "C-j") #'isearch-exit)


;; travel between isearch occurences
(define-key isearch-mode-map (kbd "C-n") #'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "C-p") #'isearch-repeat-backward)

;; swap ' and `
(define-key evil-normal-state-map (kbd "`") #'evil-goto-mark-line)
(define-key evil-normal-state-map (kbd "'") #'evil-goto-mark)


;; jump to word with ace-jump-mode
(use-package ace-jump-mode
  :config
  (define-key evil-normal-state-map (kbd "C-f") #'ace-jump-word-mode)

  (set-face-attribute 'ace-jump-face-foreground nil
                      :foreground "#cc6666"))

(defvar buffer-toggle-ring '())
(defvar buffer-toggle-ring-index 0)


(defun display-buffer-toggle-ring ()
  (interactive)
  (if (equal (length buffer-toggle-ring) 0)
      (message "Buffer ring empty.")
    (message "%s" (string-join
                   (mapcar (lambda (buffer)
                             (propertize (buffer-name buffer) 'face 'helm-buffer-file))
                           buffer-toggle-ring)
                   "\n"))))


(defun remove-current-from-buffer-ring ()
  "Remove current buffer from buffer ring."
  (when (member (current-buffer) buffer-toggle-ring)
    (setq buffer-toggle-ring (delete (current-buffer) buffer-toggle-ring))
    (display-buffer-toggle-ring)
    (setq buffer-toggle-ring-index (- buffer-toggle-ring-index 1))))


(defun toggle-buffer-in-buffer-ring ()
  "Add/remove current buffer from ring."
  (interactive)
  (if (member (current-buffer) buffer-toggle-ring)
      (remove-current-from-buffer-ring)
    (setq buffer-toggle-ring (insert-after buffer-toggle-ring buffer-toggle-ring-index (current-buffer)))
    (display-buffer-toggle-ring)
    (setq buffer-toggle-ring-index (+ buffer-toggle-ring-index 1))))


(defun switch-to-next-buffer-in-ring ()
  "Switch to next buffer in ring."
  (interactive)
  (when (> (length buffer-toggle-ring) 0)
    (setq buffer-toggle-ring-index (mod (+ buffer-toggle-ring-index 1) (length buffer-toggle-ring)))
    (switch-to-buffer (nth buffer-toggle-ring-index buffer-toggle-ring))))


(add-hook 'kill-buffer-hook #'remove-current-from-buffer-ring)


(evil-define-key 'normal global-map (kbd "<S-backspace>") #'toggle-buffer-in-buffer-ring)
(evil-define-key 'normal global-map (kbd "<backspace>") #'switch-to-next-buffer-in-ring)
(evil-define-key 'normal global-map (kbd "<C-backspace>") #'display-buffer-toggle-ring)

(use-package helm
  :config
  (helm-mode 1)

  (setq-default helm-completion-style 'emacs)
  (setq-default helm-left-margin-width 1)
  (setq-default helm-buffers-left-margin-width 1)
  (setq-default helm-buffer-details-flag nil)

  (define-key helm-map (kbd "M-]") 'helm-next-source)
  (define-key helm-map (kbd "M-[") 'helm-previous-source)
  
  
  (define-key helm-map (kbd "C-j") 'helm-confirm-and-exit-minibuffer)
  (define-key helm-map (kbd "C-k") 'helm-execute-persistent-action)
  
  
  (define-key helm-map (kbd "C-M-j") 'helm-cr-empty-string)
  
  
  (define-key helm-read-file-map (kbd "C-j") 'helm-ff-RET)
  
  
  (global-set-key (kbd "M-x") 'helm-M-x)
  (set-face-attribute 'helm-candidate-number nil
                      :extend t
                      :foreground "#BAA659"
                      :background "#121112")
  
  
  (set-face-attribute 'helm-header-line-left-margin nil
                      :extend t
                      :foreground "#BAB771"
                      :background "#121112")
  
  
  (set-face-attribute 'helm-selection nil
                      :extend t
                      :background "#191919"
                      :weight 'bold)
  
  
  (set-face-attribute 'helm-ff-file-extension nil
                      :extend t
                      :foreground 'unspecified)
  
  
  (set-face-attribute 'helm-buffer-directory nil
                      :extend t
                      :background 'unspecified
                      :foreground 'unspecified
                      :underline t
                      :weight 'bold)
  
  
  (set-face-attribute 'helm-source-header nil
                      :extend t
                      :background "#121112"
                      :foreground "#7d7461"
                      :slant 'italic
                      :family "Victor Mono")
  
  
  (set-face-attribute 'helm-buffer-saved-out nil
                      :extend t
                      :background 'unspecified
                      :foreground color-red)
  
  
  (set-face-attribute 'helm-ff-denied nil
                      :extend t
                      :background 'unspecified
                      :foreground color-red))

(defun helm-buffer-list ()
  "Return the current list of buffers.
The list is reordered with `helm-buffer-list-reorder-fn'."
  (helm-buffer-list-1))

(use-package helm-ag)

(use-package helm-projectile
  :ensure t
  :config
  (setq-default projectile-git-submodule-command nil)

  ;; projectile not working until I figure it out
  (setq-default helm-mini-default-sources '(helm-source-bookmarks
                                            ;; helm-source-projectile-recentf-list
                                            ;; helm-source-projectile-buffers-list
                                            ;; helm-source-projectile-projects
                                            ;; helm-source-projectile-files-list
                                            helm-source-buffers-list
                                            helm-source-recentf)))

;; tab width
(setq-default tab-width 4)
(setq-default c-basic-offset 4)
(setq-default indent-tabs-mode nil)


;; use C-j to join in visual mode
(define-key evil-visual-state-map (kbd "C-j") #'evil-join)


;; stop blinking, cursor. seriously.
(blink-cursor-mode -1)


;; show matching parenthesis
(show-paren-mode t)


;; set fill column (obviously)
(setq-default fill-column 75)


;; make sure sentences are not limited to those that are double-space-separated
(setq-default sentence-end-double-space nil)

;; disable auto-fill-mode when asked politely
(defun enable-polite-auto-fill ()
  (when (and (org-keyword-activep "AUTOFILL" nil)
             (not (org-keyword-activep "LITERARY")))
    (auto-fill-mode 1)))


(add-hook 'text-mode-hook #'enable-polite-auto-fill)


;; highlight current line
(add-hook 'prog-mode-hook #'hl-line-mode)

(defvar last-paragraph-sentence-split nil
  "Dotted list holding markers for the first and last paragraphs
after using split-paragraph-into-sentences.")


(defun split-paragraph-into-sentences ()
  (interactive)
  (save-excursion
    (let* ((sentence-count 0)
           (paragraph-bounds (if (use-region-p)
                                 `(,(region-beginning) . ,(region-end))
                               (mark-paragraph)
                               `(,(point) . ,(mark))))
           (beginning-marker (progn
                               (goto-char (car paragraph-bounds))
                               (point-marker))))
      (forward-sentence)
      (while (< (point) (cdr paragraph-bounds))
        (delete-char 1)
        (insert "\n\n")
        ;; (fill-paragraph)
        (forward-sentence)
        (setq sentence-count (+ 1 sentence-count)))
      (setq last-paragraph-sentence-split
            `(,beginning-marker . ,(point-marker))))))


(defun join-sentences-into-paragraph ()
  (interactive)
  (save-excursion
    (let ((start (marker-position (car last-paragraph-sentence-split)))
          (end (marker-position (cdr last-paragraph-sentence-split))))
      (goto-char start)
      (join-line nil (+ 1 start) end)
      ;; (fill-paragraph)
      )
    (setq last-paragraph-sentence-split nil)))


(defun toggle-sentence-split ()
  (interactive)
  (if last-paragraph-sentence-split
      (join-sentences-into-paragraph)
    (split-paragraph-into-sentences)))

(defvar rotate-text-rotations
  '(("true" "false")
    ("True" "False")
    ("yes" "no")
    ("0" "1")
    ("+" "-"))
  "List of text rotation sets.")


(defun rotate-region (beg end)
  "Rotate all matches in `rotate-text-rotations' between point and mark."
  (interactive "r")
  (let ((regexp (rotate-convert-rotations-to-regexp
		 rotate-text-rotations))
	(end-mark (copy-marker end)))
    (save-excursion
      (goto-char beg)
      (while (re-search-forward regexp (marker-position end-mark) t)
	(let* ((found (match-string 0))
	       (replace (rotate-next found)))
	  (replace-match replace))))))


(defun rotate-string (string &optional rotations)
  "Rotate all matches in STRING using associations in ROTATIONS.
If ROTATIONS are not given it defaults to `rotate-text-rotations'."
  (let ((regexp (rotate-convert-rotations-to-regexp
		 (or rotations rotate-text-rotations)))
	(start 0))
    (while (string-match regexp string start)
      (let* ((found (match-string 0 string))
	     (replace (rotate-next
		       found
		       (or rotations rotate-text-rotations))))
	(setq start (+ (match-end 0)
		       (- (length replace) (length found))))
	(setq string (replace-match replace nil t string))))
    string))


(defun rotate-next (string &optional rotations)
  "Return the next element after STRING in ROTATIONS."
  (let ((rots (rotate-get-rotations-for
	       string
	       (or rotations rotate-text-rotations))))
    (if (> (length rots) 1)
	(error (format "Ambiguous rotation for %s" string))
      (if (< (length rots) 1)
	  ;; If we get this far, this should not occur:
	  (error (format "Unknown rotation for %s" string))
	(let ((occurs-in-rots (member string (car rots))))
	  (if (null occurs-in-rots)
	      ;; If we get this far, this should *never* occur:
	      (error (format "Unknown rotation for %s" string))
	  (if (null (cdr occurs-in-rots))
	      (caar rots)
	    (cadr occurs-in-rots))))))))


(defun rotate-get-rotations-for (string &optional rotations)
  "Return the string rotations for STRING in ROTATIONS."
  (remq nil (mapcar (lambda (rot) (if (member string rot) rot))
		    (or rotations rotate-text-rotations))))


(defun rotate-convert-rotations-to-regexp (rotations)
  (regexp-opt (rotate-flatten-list rotations)))


(defun rotate-flatten-list (list-of-lists)
  "Flatten LIST-OF-LISTS to a single list.
Example:
  (rotate-flatten-list '((a b c) (1 ((2 3)))))
    => (a b c 1 2 3)"
  (if (null list-of-lists)
      list-of-lists
    (if (listp list-of-lists)
	(append (rotate-flatten-list (car list-of-lists))
		(rotate-flatten-list (cdr list-of-lists)))
      (list list-of-lists))))


(defun rotate-word-at-point ()
  "Rotate word at point based on sets in `rotate-text-rotations'."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'symbol))
        (opoint (point)))
    (when (consp bounds)
      (let ((beg (car bounds))
            (end (copy-marker (cdr bounds))))
        (rotate-region beg end)
        (goto-char (if (> opoint end) end opoint))))))


(evil-define-key 'normal global-map (kbd "g r r") #'rotate-word-at-point)

;; use C-j for command prompt
(define-key evil-ex-completion-map (kbd "C-j") #'exit-minibuffer)


;; insert spaces without leaving normal mode
(evil-define-key 'normal 'org-mode-map (kbd "z l") #'(lambda ()
                                                       (interactive)
                                                       (forward-char)
                                                       (insert " ")))
(evil-define-key 'normal 'org-mode-map (kbd "z h") #'(lambda ()
                                                       (interactive)
                                                       (insert " ")
                                                       (evil-backward-char)))


;; capitalization
(evil-define-key 'normal global-map (kbd "g \'") #'(lambda ()
                                                     (interactive)
                                                     (save-excursion
                                                       (if (not (equal (char-before) ? ))
                                                           (evil-backward-word-begin 1))
                                                       (upcase-char 1))))
(evil-define-key 'normal global-map (kbd "g \"") #'(lambda ()
                                                     (interactive)
                                                     (save-excursion
                                                       (if (not (equal (char-before) ? ))
                                                           (evil-backward-word-begin 1))
                                                       (downcase-word 1))))


;; make sure C-j doesn't actually create a new line in normal state
(define-key evil-normal-state-map (kbd "C-j") #'evil-ret)


;; delete char the fast way
(define-key key-translation-map (kbd "C-h") [127])


;; use C-j as RET
(global-set-key (kbd "C-j") #'newline)


;; set convenient line-opening
(defun open-line-down ()
  "Open and move to next line."
  (interactive)
  (call-interactively 'move-end-of-line)
  (newline))


(defun open-line-up ()
  "Open and move to previous line."
  (interactive)
  (call-interactively 'move-beginning-of-line)
  (newline)
  (forward-line -1))


(define-key evil-normal-state-map (kbd "z j") #'open-line-down)
(define-key evil-normal-state-map (kbd "z k") #'open-line-up)


(defun evil-shift-right-preserve ()
  (interactive)
  (evil-shift-right evil-visual-beginning evil-visual-end)
  (evil-normal-state)
  (evil-visual-restore))


(defun evil-shift-left-preserve ()
  (interactive)
  (evil-shift-left evil-visual-beginning evil-visual-end)
  (evil-normal-state)
  (evil-visual-restore))


(define-key evil-visual-state-map (kbd ">") #'evil-shift-right-preserve)
(define-key evil-visual-state-map (kbd "<") #'evil-shift-left-preserve)


(evil-define-key 'insert global-map (kbd "C-M-\\")
  #'(lambda (literal-string)
      (interactive "sInsert literal: ")
      (insert literal-string)))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode)


  ;; use C-, instead of tab to expand
  (define-key yas-minor-mode-map [(tab)] nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)

  (define-key yas-minor-mode-map (kbd "C-b") #'yas-prev-field)

  (define-key yas-minor-mode-map (kbd "C-,")
              #'(lambda ()
                  (interactive)
                  (let ((inhibit-message t))
                    (yas-expand)
                    (org-toggle-pretty-entities)
                    (org-toggle-pretty-entities))))
              


  ;; disable out-of-field modification warning (for laas)
  (setq-default yas-inhibit-overlay-modification-protection t) 


  ;; make sure latex snippets work in org-mode
  (add-hook 'org-mode-hook (lambda ()
                             (yas-minor-mode)
                             (yas-activate-extra-mode 'latex-mode))))

(defun insert-matrix-like (env rows cols &optional arguments)
  (interactive "sEnvironment: \nnRows: \nnColumns: ")
  (let ((beginning-marker (point-marker))
        (end-marker (save-excursion
                      (goto-char (1+ (point)))))
        (matrix-string ""))
    (insert (format "\\begin{%s}%s\n" env (if arguments arguments "")))
    (dotimes (i rows)
      (dotimes (j cols)
        (setq matrix-string (concat matrix-string
                                    (format
                                     (if (equal j (1- cols)) "$%s" "$%s & ")
                                     (+ 1 (* i cols) j)))))
      (setq matrix-string (concat matrix-string (if (equal i (1- rows)) (format "\n\\end{%s}" env) "\\\\\\\n"))))
    (setq my/unhiding-current-line nil)
    (yas-expand-snippet matrix-string)
    (add-hook 'yas-after-exit-snippet-hook #'align-yasnippet-matrix)))


(defun align-yasnippet-matrix ()
  (align-regexp yas-snippet-beg yas-snippet-end "\\(\\s-*\\) &" 1 1 t)
  (setq my/unhiding-current-line t)
  (remove-hook 'yas-after-exit-snippet-hook #'align-yasnippet-matrix))

(defun snippet-create-n-cases (n)
  (let ((snippet-string "")
        (yas-indent-line 'fixed)
        (yas-wrap-around-region 'nil))
    (dotimes (i n)
      (setq snippet-string (format "%s$%d & \\text{if } $%d\\\\\\\n"
                                   snippet-string
                                   (+ (* 2 i) 1)
                                   (+ (* 2 i) 2))))
    (yas-expand-snippet (format "\\begin{cases}\n%s\\end{cases}" snippet-string))))

(defun snippet-create-cases ()
  (interactive)
  (let ((n (save-excursion
             (goto-char (- (point) 1))
             (number-at-point))))
    (if n
        (progn
          (save-excursion
            (goto-char (- (point) 1))
            (let ((bounds (bounds-of-thing-at-point 'number)))
              (delete-region (car bounds) (cdr bounds))))
          (snippet-create-n-cases n))
      (snippet-create-n-cases 2))))

(defun snippet-convert-fraction (start end)
  (interactive
   (if (region-active-p)
       (list (region-beginning) (region-end))
     '(nil nil)))

  (if (null start)
      (if (not (texmathp))
          (insert "/")
        (pcase (char-before)
          (?/
           (delete-backward-char 1)
           (yas-expand-snippet "\\dfrac{$1}{$2}$0"))
          (?.
           (delete-backward-char 1)
           (insert "/"))
          (?,
           (delete-backward-char 1)
           (insert "\\big/"))
          (-
           (insert "/")

           (let ((found-brace 0)
                 (limit (point))
                 (match nil)
                 (match-start nil))
             (save-excursion
               (goto-char (1- (point)))
               (while (and
                       (texmathp)
                       (> (point) (line-beginning-position))
                       (not (equal (char-after) ? ))
                       (not (and (memq (char-after) '(?{ ?\()) (equal found-brace 0))))
                 (pcase (char-after)
                   ((or ?} ?\)) (setq found-brace (1+ found-brace)))
                   ((or ?{ ?\() (setq found-brace (1- found-brace))))
                 (goto-char (1- (point))))
               (when (re-search-forward "\\(?:^\\(?1:.+\\)/\\)\\|\\(?:[ {(]\\(?1:.+\\)/\\)" limit t)
                 (setq match (match-string 1))
                 (setq match-start (match-beginning 1))))

             (when match-start
               (delete-region match-start limit)
               (yas-expand-snippet (format "\\dfrac{%s}{$1}$0" match)))))))
    (let ((numerator (buffer-substring start end)))
      (evil-insert-state)
      (delete-region start end)
      (yas-expand-snippet (format "\\dfrac{%s}{$1}$0" numerator)))))


(evil-define-key 'insert org-mode-map (kbd "/") #'snippet-convert-fraction)
(evil-define-key 'visual org-mode-map (kbd "/") #'snippet-convert-fraction)

(defun swap-equation-sides (start end)
  (interactive
   (if (region-active-p)
       (list (region-beginning) (region-end))
     `(,(line-beginning-position) ,(line-end-position))))
  (save-excursion
    (goto-char start)
    (re-search-forward "\\(.*?\\)\\([[:blank:]]*&?=[[:blank:]]*\\)\\(.*\\)" end)

    (let ((LHS (match-string 1))
          (equals (match-string 2))
          (RHS (match-string 3)))
      (delete-region start end)
      (insert (format "%s%s%s" RHS equals LHS)))))

(evil-define-key 'visual org-mode-map (kbd "=") #'swap-equation-sides)
(evil-define-key 'normal org-mode-map (kbd "g =") #'swap-equation-sides)

;; TODO Create stronger criteria for variable subsitution (maybe incorporating thing at point, with exceptions for numbers and other variables)

(defun string-substitute-math-variables (contents vars)
  (with-temp-buffer
    (insert contents)
    (dolist (pair (split-string vars ","))
      (-let (((var value) (split-string pair "=")))
        (condition-case nil
            (while (replace-regexp-in-region
                    var
                    (format "(%s)" value)
                    (point-min) (point-max)))
          (error nil))))
    (buffer-string)))


(defun substitute-math-variables (vars start end)
  (interactive
   (append (list (read-string "Enter variable-value pairs (e.g., \"x=4,y=7\"): "))
           (if (use-region-p)
               (list (region-beginning) (region-end))
             (list (line-beginning-position) (line-end-position)))))

  (save-excursion
    (let ((contents (buffer-substring start end)))
      (goto-char end)
      (insert (string-substitute-math-variables contents vars)))
    (delete-region start end)))


(evil-define-key 'normal global-map (kbd "g r s") #'substitute-math-variables)

(defvar anonymous-snippet ""
  "Stores a temporary-use snippet.")


(defun create-anonymous-snippet (snippet)
  (interactive "sSnippet: ")
  (setq anonymous-snippet snippet))


(define-key global-map (kbd "C-s") #'create-anonymous-snippet)


(defun expand-anonymous-snippet ()
  (interactive)
  (yas-expand-snippet anonymous-snippet))

  ;; It may be interesting to note that the following combinations don't appear in
  ;; English: bx, cj, cv, cx, dx, fq, fx, gq, gx, hx, jc, jf, jg, jq, js, jv, jw,
  ;; jx, jz, kq, kx, mx, px, pz, qb, qc, qd, qf, qg, qh, qj, qk, ql, qm, qn, qp,
  ;; qs, qt, qv, qw, qx, qy, qz, sx, vb, vf, vh, vj, vm, vp, vq, vt, vw, vx, wx,
  ;; xj, xx, zj, zq, zx


  (use-package aas
    :hook (LaTeX-mode . aas-activate-for-major-mode)
    :hook (org-mode . aas-activate-for-major-mode)
    :config
    (aas-set-snippets 'text-mode
      ";o-" "ō"
      ";i-" "ī"
      ";a-" "ā"
      ";u-" "ū"
      ";e-" "ē"
      "a-0" #'expand-anonymous-snippet)

    (aas-set-snippets 'org-mode
      "js" (lambda () (interactive)
             (yas-expand-snippet "\\\\( $1 \\\\)$0"))
      "jf" (lambda () (interactive)
             (yas-expand-snippet "\\begin{alignat*}{3}\n$0\n\\end{alignat*}")))

    (aas-set-snippets 'org-mode
      :cond #'texmathp
      "cas" #'snippet-create-cases
      "=-0" #'swap-equation-sides
      "3det" (lambda () (interactive)
               (insert-matrix-like "vmatrix" 3 3))
      "2det" (lambda () (interactive)
               (insert-matrix-like "vmatrix" 2 2))
      "3mat" (lambda () (interactive)
               (insert-matrix-like "bmatrix" 3 3))
      "2mat" (lambda () (interactive)
               (insert-matrix-like "bmatrix" 2 2))
      "2amat" (lambda () (interactive)
               (insert-matrix-like "amatrix" 2 3 "{2}"))
      "3amat" (lambda () (interactive)
               (insert-matrix-like "amatrix" 3 4 "{3}"))
      "4amat" (lambda () (interactive)
               (insert-matrix-like "amatrix" 4 5 "{4}"))
      "matr" (lambda (rows cols) (interactive "nRows: \nnColumns: ")
               (insert-matrix-like "bmatrix" rows cols))
      "amatr" (lambda (rows cols sep)
                (interactive "nRows: \nnColumns: \nnSeparate at: ")
                (insert-matrix-like "aamatrix" rows cols
                                    (format "{%s}{%s}" sep (- cols sep))))
      "matl" #'insert-matrix-like)

    (let ((snippets '(("sup" . "^{$1$0")
                      ("ud" . "_{$1$0")
                      ("jg" . "^2$0")
                      ("jc" . "^3$0")

                      ("gal" . "\\alpha$0")
                      ("gbe" . "\\beta$0")
                      ("gga" . "\\gamma$0")
                      ("gde" . "\\delta$0")
                      ("gep" . "\\epsilon$0")
                      ("gze" . "\\zeta$0")
                      ("geta" . "\\eta$0")
                      ("gth" . "\\theta$0")
                      ("gio" . "\\iota$0")
                      ("gka" . "\\kappa$0")
                      ("gla" . "\\lambda$0")
                      ("gmu" . "\\mu$0")
                      ("gnu" . "\\nu$0")
                      ("gxi" . "\\xi$0")
                      ("gmi" . "\\omicron$0")
                      ("gpi" . "\\pi$0")
                      ("grh" . "\\rho$0")
                      ("gsi" . "\\sigma$0")
                      ("gta" . "\\tau$0")
                      ("gup" . "\\upsilon$0")
                      ("gph" . "\\phi$0")
                      ("gch" . "\\chi$0")
                      ("gpsi" . "\\psi$0")
                      ("gme" . "\\omega$0")

                      ("Alpha" . "\\Alpha$0")
                      ("Beta" . "\\Beta$0")
                      ("Gamma" . "\\Gamma$0")
                      ("Delta" . "\\Delta$0")
                      ("Epsilon" . "\\Epsilon$0")
                      ("Zeta" . "\\Zeta$0")
                      ("Eta" . "\\Eta$0")
                      ("Theta" . "\\Theta$0")
                      ("Iota" . "\\Iota$0")
                      ("Kappa" . "\\Kappa$0")
                      ("Lambda" . "\\Lambda$0")
                      ("Mu" . "\\Mu$0")
                      ("Nu" . "\\Nu$0")
                      ("Xi" . "\\Xi$0")
                      ("Omicron" . "\\Omicron$0")
                      ("Pi" . "\\Pi$0")
                      ("Rho" . "\\Rho$0")
                      ("Sigma" . "\\Sigma$0")
                      ("Tau" . "\\Tau$0")
                      ("Upsilon" . "\\Upsilon$0")
                      ("Phi" . "\\Phi$0")
                      ("Chi" . "\\Chi$0")
                      ("Psi" . "\\Psi$0")
                      ("Omega" . "\\Omega$0")

                      ("ln" . "\\ln $0")

                      ("cos" . "\\cos{$1$0")
                      ("sec" . "\\sec{$1$0")
                      ("sin" . "\\sin{$1$0")
                      ("csc" . "\\csc{$1$0")
                      ("tan" . "\\tan{$1$0")
                      ("cot" . "\\cot{$1$0")
                      ("acos" . "\\arccos{$1$0")
                      ("asin" . "\\arcsin{$1$0")
                      ("atan" . "\\arctan{$1$0")

                      ("ncos" . "\\cos^{$1{$2}$0")
                      ("nsec" . "\\sec^{$1{$2}$0")
                      ("nsin" . "\\sin^{$1{$2}$0")
                      ("ncsc" . "\\csc^{$1{$2}$0")
                      ("ntan" . "\\tan^{$1{$2}$0")
                      ("ncot" . "\\cot^{$1{$2}$0")
                      ("nacos" . "\\arccos^{$1{$2}$0")
                      ("nasin" . "\\arcsin^{$1{$2}$0")
                      ("natan" . "\\arctan^{$1{$2}$0")

                      ("2cos" . "\\cos^2{$2$0")
                      ("2sec" . "\\sec^2{$2$0")
                      ("2sin" . "\\sin^2{$2$0")
                      ("2csc" . "\\csc^2{$2$0")
                      ("2tan" . "\\tan^2{$2$0")
                      ("2cot" . "\\cot^2{$2$0")
                      ("2acos" . "\\arccos^2{$2$0")
                      ("2asin" . "\\arcsin^2{$2$0")
                      ("2atan" . "\\arctan^2{$2$0")

                      ("exp" . "\\exp$0")
                      ("log" . "\\log$0")
                      ("ein" . " \\in $0")

                      ("ooo" . "\\infty$0")

                      ("par" . "\\left( $1 \\right)$0")
                      ("bra" . "\\left[ $1 \\right]$0")

                      ("cro" . " \\times $0")
                      ("dot" . " \\cdot $0")

                      ("3po" . "\\left( $1, $2, $3 \\right)$0")
                      ("3ve" . "\\left\\langle $1, $2, $3 \\right\\rangle$0")

                      ("2po" . "\\left( $1, $2 \\right)$0")
                      ("2ve" . "\\left\\langle $1, $2 \\right\\rangle$0")

                      ("kq" . "\\sqrt{$1$0")

                      ("equ" . " = $0")
                      (">>" . " \\> $0")
                      ("nqu" . " \\neq $0")
                      ("vm" . " - $0")
                      ("vp" . " + $0")
                      ("seq" . " &= $0")
                      ("amp" . " & $0")
                      ("gtn" . " > $0")
                      ("sgt" . " &> $0")
                      ("lst" . " < $0")
                      ("slt" . " &< $0")
                      ("leq" . " \\leq $0")
                      ("lseq" . " &\\leq $0")
                      ("geq" . " \\geq $0")
                      ("rk" . "\\\\\\\\")
                      ("gseq" . " &\\geq $0")
                      ("trip" . " \\equiv $0")
                      ("strip" . " &\\equiv $0"))))

      (dolist (snippet snippets)
        (aas-set-snippets 'org-mode
          :cond #'texmathp
          (car snippet) `(lambda () (interactive)
                           (yas-expand-snippet ',(cdr snippet)))))))

(setq-default ispell-program-name "/usr/local/bin/aspell")

(let ((langs '("american" "brasileiro")))
  (setq lang-ring (make-ring (length langs)))
  (dolist (elem langs) (ring-insert lang-ring elem)))


(defun cycle-ispell-languages ()
  (interactive)
  (let ((lang (ring-ref lang-ring -1)))
    (ring-insert lang-ring lang)
    (ispell-change-dictionary lang)))

(setq-default flyspell-prog-text-faces '(font-lock-comment-face font-lock-doc-face))

(use-package flyspell
  :config
  (set-face-attribute 'flyspell-duplicate nil
                      :underline color-blue)
  
  
  (set-face-attribute 'flyspell-incorrect nil
                      :underline color-red))

(use-package smartparens
  :config

  (smartparens-global-mode)

  ;; disable pairing for single quotes in emacs lisp
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil))

(defvar template-placeholder-text "<++>"
  "Placeholder text to be replaced by `edit-next-placeholder'.")

(defun edit-next-placeholder ()
  "Jump to next `template-placeholder-text' and edit."
  (interactive)
  (if (search-forward template-placeholder-text nil t)
      (progn
        (delete-backward-char (length template-placeholder-text))
        (evil-insert-state))))

(global-set-key (kbd "C-.") #'edit-next-placeholder)

(fset 'org-list-to-row
      (kmacro-lambda-form [?v ?i ?p ?\; ?s ?/ ?\\ ?n ?  ?  ?/ ?  ?\C-j ?g ?v ?\; ?s ?/ ?- ?  ?\\ ?\( ?. ?* ?\\ ?\) ?/ ?| ?  ?\\ ?1 ?  return ?g ?v ?\C-j ?A ?| ?j ?k] 0 "%d"))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c C-r") 'org-list-to-row))

(use-package undo-tree
  :config
  ;; stop creating automatic backups
  (setq-default undo-tree-auto-save-history nil)


  (global-undo-tree-mode))

(defun vterm-send-escape ()
  (interactive)
  (vterm-send-key "<escape>"))


(use-package vterm
  :ensure t
  :config
  (define-key vterm-mode-map (kbd "M-h") 'evil-window-left)
  (define-key vterm-mode-map (kbd "M-j") 'evil-window-down)
  (define-key vterm-mode-map (kbd "M-k") 'evil-window-up)
  (define-key vterm-mode-map (kbd "M-l") 'evil-window-right)
  (setq-local evil-insert-state-cursor '("#aaa" box))


  (define-key vterm-mode-map (kbd "C-<return>") #'evil-normal-state)


  ;; use C-return to escape external insert state
  (evil-define-key 'insert vterm-mode-map (kbd "<escape>") #'vterm-send-escape))

(global-set-key (kbd "M-§") 'shell-command)

(define-key minibuffer-local-map (kbd "C-o")
  (lambda ()
    (interactive) 
    (insert (buffer-name (window-buffer (minibuffer-selected-window))))))

(define-key minibuffer-local-map (kbd "C-p") 'previous-line-or-history-element)
(define-key minibuffer-local-map (kbd "C-n") 'next-line-or-history-element)

(use-package gitignore-mode
  :load-path "lisp/")

(use-package magit)

(setq-default magit-display-buffer-function
              (lambda (buffer)
                (display-buffer
                 buffer (if (and (derived-mode-p 'magit-mode)
                                 (memq (with-current-buffer buffer major-mode)
                                       '(magit-process-mode
                                         magit-revision-mode
                                         magit-diff-mode
                                         magit-stash-mode
                                         magit-status-mode)))
                            nil
                          '(display-buffer-same-window)))))

(use-package flycheck
  :config
  (set-face-attribute 'flycheck-error nil
                      :underline color-red)
  
  
  (set-face-attribute 'flycheck-info nil
                      :underline color-light-green)
  
  
  (set-face-attribute 'flycheck-warning nil
                      :underline color-light-orange)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  
  (global-flycheck-mode))

(use-package flymake
  :config
  (set-face-attribute 'flymake-error nil
                      :underline color-red)
  
  
  (set-face-attribute 'flymake-note nil
                      :underline color-light-green)
  
  
  (set-face-attribute 'flymake-warning nil
                      :underline color-light-orange))

(use-package company
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq-default company-tooltip-align-annotations t)


  ;; use icons in company mode
  (setq-default company-format-margin-function 'company-vscode-dark-icons-margin)


  ;; use C-j to complete
  (define-key company-active-map (kbd "C-j") #'company-complete-selection)


  ;; change company faces
  (set-face-attribute 'company-tooltip nil
                      :inherit 'default
                      :background color-background)


  (set-face-attribute 'company-scrollbar-fg nil
                      :background color-dark-subdued)


  (set-face-attribute 'company-scrollbar-bg nil
                      :background color-background)


  (set-face-attribute 'company-tooltip-mouse nil
                      :background color-dark-subdued)


  (set-face-attribute 'company-tooltip-selection nil
                      :background color-dark-subdued)


  (set-face-attribute 'company-tooltip-annotation nil
                      :foreground color-blue)


  (setq-default company-idle-delay              0.1
                company-minimum-prefix-length   2
                company-show-numbers            t
                company-tooltip-limit           20
                company-dabbrev-downcase        nil))

(use-package elpy
  :config
  (elpy-enable)


  ;; stop weird indentation guides from elpy
  (add-hook 'elpy-mode-hook (lambda () (highlight-indentation-mode -1)))


  (setq-default elpy-rpc-backend "jedi")


  ;; use global environment
  (setq-default elpy-rpc-virtualenv-path 'current))

(use-package company-jedi
  :config

  (add-hook 'python-mode-hook #'(lambda ()
                                  (interactive)
                                  (add-to-list 'company-backends 'company-jedi))))

;; setting python executable
(setq-default flycheck-python-flake8-executable "python")
(setq-default flycheck-python-pycompile-executable "python")
(setq-default flycheck-python-pylint-executable "python")

;; set python path
(setenv "PYTHONPATH" (shell-command-to-string "$SHELL --login -c 'echo -n $PYTHONPATH'"))

(defface method-call-lock
  '((t :foreground "#b5bd68"))
  "Face for method calls."
  :group 'python-mode)

(defface operator-lock
  '((t :foreground "#cc6666"))
  "Face for operators."
  :group 'python-mode)

(font-lock-add-keywords 'python-mode
                        `((,(concat
                             "\\."
                             "\\<\\([_a-zA-Z][_a-zA-Z0-9]*\\)\\>"
                             "(") 1 'method-call-lock keep t)
                          ("\\([=+-/*><]\\)" . 'operator-lock)))

(add-hook 'python-mode-hook (lambda ()
                              (embrace-add-pair ?d "\"\"\"" "\"\"\"")))

(setq-default lsp-rust-server 'rust-analyzer)

(use-package rust-mode
  :config
  (add-hook 'rust-mode-hook #'racer-mode)


  (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)


  (setq-default racer-rust-src-path
                (concat (string-trim
                         (shell-command-to-string "rustc --print sysroot"))
                        "/lib/rustlib/src/rust/library")))

(global-set-key (kbd "C-;") 'eval-expression)

(define-key lisp-interaction-mode-map (kbd "C-j") #'newline)

(setq-default sgml-basic-offset 4)
(setq-default sgml-specials nil)

(use-package emmet-mode
  :config
  (add-hook 'html-mode-hook 'emmet-mode)

  (define-key emmet-mode-keymap (kbd "C-j") #'newline)
  (define-key emmet-mode-keymap (kbd "C-,") #'emmet-expand-line))

(use-package latex
  :ensure auctex
  :config
  (setq-default TeX-auto-save t)
  (setq-default TeX-parse-self t)


  ;; refresh PDF constantly
  (setq-default auto-revert-interval 0.5)


  ;; stop showing revert messages
  (setq-default auto-revert-verbose nil)


  ;; disable inline sub-/superscript --- gets annoying
  (setq-default font-latex-fontify-script nil)


  ;; add math pairing
  (sp-local-pair '(latex-mode org-mode) "$" "$")


  ;; add pairings for \left \right delimiters
  (sp-local-pair '(latex-mode org-mode) "\\left(" "\\right)")
  (sp-local-pair '(latex-mode org-mode) "\\left[" "\\right]")
  (sp-local-pair '(latex-mode org-mode) "\\left\\langle" "\\right\\rangle")


  ;; Use pdf-tools to open PDF files
  (setq-default TeX-view-program-selection '((output-pdf "PDF Tools"))
                TeX-source-correlate-start-server t)


  ;; Update PDF buffers after successful LaTeX runs
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer))

(defun run-current-python (arguments)
  "Run current Python file."
  (shell-command-to-string
   (format
    "export PYTHONPATH='${PYTHONPATH}:/Users/jetblack/maxwell/src/:/Users/jetblack/pyimports/' && python '%s' %s"
    (buffer-file-name) arguments)))

(defun run-current-javascript (arguments)
  "Run current JavaScript file."
  (shell-command-to-string
   (format
    "node '%s' %s"
    (buffer-file-name) arguments)))

(defun run-current-julia (arguments)
  "Run current Python file."
  (shell-command-to-string
   (format
    "export PYTHONPATH='${PYTHONPATH}:/Users/jetblack/maxwell/src/' && /Applications/Julia-1.6.app/Contents/Resources/julia/bin/julia '%s' %s"
    (buffer-file-name) arguments)))

(defun run-current-shell (arguments)
  "Run current shell script."
  (shell-command-to-string (format "zsh '%s' %s" (buffer-file-name) arguments)))

(defun run-script-without-args ()
  (interactive)
  (cond ((equal major-mode 'dired-mode) (dired-find-file) (run-script-without-args))
        ((equal major-mode 'python-mode) (princ (run-current-python "")))
        ((equal major-mode 'js-mode) (princ (run-current-javascript "")))
        ((equal major-mode 'julia-mode) (princ (run-current-julia "")))
        ((equal major-mode 'sh-mode) (princ (run-current-shell "")))
        ((equal major-mode 'org-mode) (org-babel-execute-src-block))
        (t (message "File type not supported."))))


(defun run-script-with-args (arguments)
  (interactive "MArguments: ")
  (cond ((equal major-mode 'dired-mode) (dired-find-file) (run-script-with-args arguments))
        ((equal major-mode 'python-mode) (princ (run-current-python arguments)))
        ((equal major-mode 'js-mode) (princ (run-current-javascript arguments)))
        ((equal major-mode 'julia-mode) (princ (run-current-julia arguments)))
        ((equal major-mode 'sh-mode) (princ (run-current-shell arguments)))
        ((equal major-mode 'org-mode) (org-babel-execute-src-block))
        (t (message "File type not supported."))))

(use-package notmuch
  :config
  ;; bind C-j properly
  (evil-define-key 'normal notmuch-search-mode-map (kbd "C-j") #'notmuch-search-show-thread)
  
  
  ;; search by tag in notmuch
  (evil-define-key 'normal notmuch-hello-mode-map (kbd "?") #'notmuch-search-by-tag))

;; hide ugly notmuch logo
(setq-default notmuch-show-logo nil)

(setq-default send-mail-function 'sendmail-query-once
              sendmail-program "/usr/sbin/sendmail"
              mail-specify-envelope-from t
              message-sendmail-envelope-from 'header
              mail-envelope-from 'header)

(defun get-all-marked-files-and-unmark ()
  "Return a list of marked files from all Dired buffers."
  (let ((files  ())
        (here   ()))
    (dolist (buf  (mapcar #'cdr dired-buffers))
      (with-current-buffer buf
        (setq here  (dired-get-marked-files nil nil nil t))
        (when (or (null (cdr here))  (eq t (car here)))
          (setq here  (cdr here)))
        (setq files  (nconc here files))
        (let ((inhibit-message t))
          (dired-unmark-all-marks))))
    (setq files  (delete-dups files))))

(add-hook 'dired-mode-hook #'auto-revert-mode)

(setq-default dired-kill-when-opening-new-dired-buffer t)

(add-hook 'dired-after-readin-hook #'(lambda () (set-buffer-modified-p nil)))

(custom-set-faces
 '(bookmark-face ((t nil))))

(add-hook 'dired-mode-hook 'dired-hide-details-mode)

(setq-default dired-use-ls-dired t)
(setq-default insert-directory-program "/usr/local/bin/gls")
(setq-default dired-listing-switches "-lGh1v --group-directories-first")


(defvar dired-show-hidden-files nil
  "Whether to show hidden files in dired.")


(defun toggle-dired-show-hidden-files ()
  "Toggle showing hidden dired files."
  (interactive)
  (if dired-show-hidden-files
      (progn
        (setq dired-listing-switches "-lGh1v --group-directories-first")
        (setq dired-show-hidden-files nil)
        (message "Hiding hidden files."))
    (setq dired-listing-switches "-lAGh1v --group-directories-first")
    (setq dired-show-hidden-files t)
    (message "Showing hidden files."))
  (let ((current-directory dired-directory))
    (kill-buffer)
    (dired current-directory)))

(defun dired-open-in-default ()
  "Opens current file in default application.  `browse-url-of-dired-file` wasn't working."
  (interactive)
  (let ((marked-files (dired-get-marked-files)))
    (if (null marked-files)
        (shell-command (format "open '%s'" (dired-current-directory)))
      (dired-do-shell-command "open" 1 marked-files))))

(evil-define-key 'normal dired-mode-map "W" #'dired-open-in-default)

(evil-define-key 'normal dired-mode-map "K" #'(lambda ()
                                                (interactive)
                                                (scroll-down 3)))
(evil-define-key 'normal dired-mode-map "J" #'(lambda ()
                                                (interactive)
                                                (scroll-up 3)))
(evil-define-key 'normal dired-mode-map (kbd "C-j") 'dired-find-file)

(evil-define-key 'normal 'dired-mode-map "q" 'evil-owl-record-macro)

(define-key dired-mode-map (kbd "<f5>") #'toggle-dired-show-hidden-files)

(setq-default revert-without-query '(".pdf"))
(add-hook 'pdf-view-mode-hook #'auto-revert-mode)

(defvar pdf-hiding-cursor nil
  "Last buffer was a PDF and was hiding cursor.")


(use-package pdf-tools
  :config
  (add-hook 'pdf-view-mode-hook
            #'(lambda ()
                (add-to-list 'pdf-annot-default-annotation-properties '(text (icon . "Note")
                                                                             (color . "#fff")))
                (set (make-local-variable 'evil-normal-state-cursor) (list nil))))

  (pdf-tools-install))

(define-key image-mode-map (kbd "g =") 'image-increase-size)
(define-key image-mode-map (kbd "g -") 'image-decrease-size)

(defvar pdf-has-set-light-mode nil
  "Whether PDf view has turned on light mode.")

(add-hook 'post-command-hook #'(lambda ()
                                 (when (and (not (equal major-mode 'pdf-view-mode)) pdf-has-set-light-mode)
                                   (reset-temporary-face-attributes)
                                   (set-background-color color-background)
                                   (set-foreground-color color-foreground)
                                   (setq evil-normal-state-cursor '("#fdf4c1" box))
                                   (setq pdf-has-set-light-mode nil))))

(add-hook 'post-command-hook #'(lambda ()
                                 (when (and (equal major-mode 'pdf-view-mode)
                                            (equal (count-windows) 1))
                                   (setq pdf-has-set-light-mode t)
                                   (set-background-color "#FFF")
                                   (set-foreground-color color-background)
                                   (setq evil-normal-state-cursor '("#FFF" box))

                                   (set-temporary-face-attributes
                                    :background "#FFF"
                                    '(header-line
                                      mode-line))

                                   (set-temporary-face-attributes
                                    :foreground color-background
                                    '(mode-line
                                      mood-line-buffer-name
                                      mood-line-major-mode
                                      mood-line-modified
                                      mood-line-status-error
                                      mood-line-status-info
                                      mood-line-status-neutral
                                      mood-line-status-success
                                      mood-line-status-warning
                                      mood-line-unimportant)))))

(evil-define-key 'visual pdf-view-mode-map (kbd "g h") #'pdf-annot-add-highlight-markup-annotation)

(evil-define-key 'visual pdf-view-mode-map (kbd "g t") #'pdf-annot-add-text-annotation)
(evil-define-key 'normal pdf-view-mode-map (kbd "g t") #'pdf-annot-add-text-annotation)

(use-package org
  :hook ((org-mode . flyspell-mode)
         (text-mode . flyspell-mode))
  :init
  (setq-default org-emphasis-regexp-components
                '("   ('\"{\x200B" "-     .,:!?;'\")}\\[\x200B" " ,\"'" "." 20))
  
  :config
  (require 'ox)

  (setq-default org-return-follows-link nil)
  (setq-default org-src-preserve-indentation t)
  (setq-default org-src-window-setup 'current-window)
  (setq-default org-src-tab-acts-natively t)
  (setq-default org-html-htmlize-output-type 'css)
  
  
  ;; prevent org from splitting line on meta return       
  (setq-default org-M-RET-may-split-line nil)
  
  
  ;; pretty LaTeX entities for org mode
  (setq-default org-pretty-entities t)
  
  
  ;; prevent confirmation prompt when executing code blocks
  (setq-default org-confirm-babel-evaluate nil)
  
  
  ;; prevent section indentation
  (setq-default org-adapt-indentation nil)
  
  
  ;; use all agenda files as refile targets
  (setq-default org-refile-targets '((org-agenda-files :maxlevel . 1)))
  
  
  ;; prevent section numbering in exports
  (setq-default org-export-with-section-numbers nil)
  
  
  ;; show markdown for org-mode
  (setq-default org-hide-emphasis-markers nil)
  
  
  ;; use different ellipsis for folded content
  (setq-default org-ellipsis " [...]")
  
  
  ;; make sure to color latex
  (setq-default org-highlight-latex-and-related '(latex entities))
  
  
  ;; latex formatting
  (setq-default org-format-latex-options '(:foreground default
                                                       :background default
                                                       :scale 1.0
                                                       :html-foreground "Black"
                                                       :html-background "Transparent"
                                                       :html-scale 0.4
                                                       :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))
  
  
  ;; prevent html export from having postamble
  (setq-default org-html-postamble nil)
  
  
  ;; remove table of contents from export
  (setq-default org-export-with-toc nil)
  
  
  ;; use smart quotation marks
  (setq-default org-export-with-smart-quotes t)
  
  
  ;; stop displaying inline images in org-mode
  (setq-default org-startup-with-inline-images nil)
  ;; (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
  
  
  ;; default image width 
  (setq-default org-image-actual-width 600)
  
  
  ;; change MathJax formatting
  (setf org-html-mathjax-options
        '((path "https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML")
          (scale "90") 
          (align "center") 
          (indent "2em")
          (mathml nil)
          (font "Neo-Euler")))
  
  
  ;; change custom date format
  (setq-default org-time-stamp-custom-formats '("<%A, %m/%d/%y>" . "<%A, %m/%d/%y, %H:%M>"))
  ;; disable company mode for org-mode
  (add-hook 'org-mode-hook #'(lambda () (company-mode -1)))
  (add-hook 'text-mode-hook #'(lambda () (company-mode -1)))
  
  
  ;; make sure M-h binding works
  (add-hook 'org-mode-hook
            (lambda ()
              (define-key org-mode-map "\M-h" nil)
              (global-set-key (kbd "M-h") 'evil-window-left)))
  
  
  ;; open link in the same window
  (add-to-list 'org-link-frame-setup '(file . find-file))
  (setq-default help-window-keep-selected t)
  
  
  ;; recalculate table
  (define-key org-mode-map (kbd "C-M-'") #'org-table-recalculate)
  
  
  ;; recalculate table
  (define-key org-mode-map (kbd "C-M-;") #'(lambda ()
                                             (interactive)
                                             (org-table-eval-formula '(4))))
  
  
  ;; clear field
  (define-key org-mode-map (kbd "C-c SPC") #'org-table-blank-field)
  
  
  ;; start timer
  (evil-define-key 'normal org-mode-map (kbd "g t s") #'org-timer-start)
  (evil-define-key 'normal org-mode-map (kbd "g t m")
    #'(lambda ()
        (interactive)
        (message (org-timer-value-string))))
  (evil-define-key 'normal org-mode-map (kbd "g t d") #'org-timer-set-timer)
  (evil-define-key 'normal org-mode-map (kbd "g t i") #'org-timer)
  (evil-define-key 'normal org-mode-map (kbd "g t .")
    #'(lambda ()
        (interactive)
        (let ((final-timer-value (string-trim (org-timer-value-string))))
          (let ((inhibit-message t))
            (org-timer-stop))
          (message "Timer stopped at %s." final-timer-value))))
  (evil-define-key 'normal org-mode-map (kbd "g t ,") #'org-timer-pause-or-continue)
  
  
  ;; disable pairing for single quotes
  (sp-local-pair 'org-mode "'" nil :actions nil)
  
  
  ;; create a definition for a term in notes
  (defun org-note-create-definition (start end)
    (interactive "r")
    (let ((old-point (point))
          (inhibit-message t))
      (if (use-region-p)
          (let* ((text (buffer-substring start end))
                 (title (concat (upcase (substring text 0 1)) (substring text 1))))
            (replace-region-contents start end
                                     (lambda ()
                                       (format "[[*%s][%s]]" title text)))
            (goto-char (point-max))
            (yas-expand-snippet
             (format
              "\n%s ${1:%s}\n\n$2/${1:$(if (or (not (yas-field-value 2)) (equal (yas-field-value 2) \"\")) yas-text (downcase yas-text))}/ $0"
              (org-heading-asterisks) title)
             (point-max))
            (evil-insert-state))
        (goto-char (point-max))
        (yas-expand-snippet
         "\n`(org-heading-asterisks)` $1\n\n$2/${1:$(if (equal (yas-field-value 2) \"\") yas-text (downcase yas-text))}/ $0"
         (point-max))
        (evil-insert-state))
      (org-mark-ring-push old-point)))
  
  
  ;; function for how many asterisks required to create heading
  (defun org-heading-asterisks ()
    (make-string (if (> (org-outline-level) 0)
                     (max (org-outline-level) 2)
                   1) ?*))
  
  
  ;; stop creating bookmarks after refiling
  (plist-put org-bookmark-names-plist :last-capture nil)
  (plist-put org-bookmark-names-plist :last-refile nil)
  (plist-put org-bookmark-names-plist :last-capture-marker nil)
  (defun replace-all (regexp string)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (replace-match string))))
  
  (defun org-compile-files (dir regexp title)
    (interactive "DWhich directory? \nsWhich files? \nsWhat title? ")
    (let ((files (directory-files-recursively dir regexp))
          (output-path (concat dir "compilation.org")))
      (write-region (format "#+title: %s\n\n" title) nil output-path)
      (mapc (lambda (file)
              (with-temp-buffer
                (insert-file-contents file)
                (replace-all "^\\*\\(\\** \\)" "**\\1")
                (replace-all "#\\+title: \\(.+\\)" "* \\1")
                (goto-char (point-max))
                (newline)
                (append-to-file (point-min) (point-max) output-path)))
            files)
      (find-file output-path)))
  ;; change face for emphasis markers
  (defface org-emphasis-marker '((t (:inherit shadow)))
    "Face for Org emphasis markers"
    :group 'org-faces)
  
  
  (defun org-do-emphasis-faces (limit)
    "Run through the buffer and emphasize strings."
    (require 'org-macs)
    (require 'org-compat)
    (let ((quick-re (format "\\([%s]\\|^\\)\\([~=*/_+]\\)"
                            (car org-emphasis-regexp-components))))
      (catch :exit
        (while (re-search-forward quick-re limit t)
          (let* ((marker (match-string 2))
                 (verbatim? (member marker '("~" "="))))
            (when (save-excursion
                    (goto-char (match-beginning 0))
                    (and
                     ;; Do not match table hlines.
                     (not (and (equal marker "+")
                               (org-match-line
                                "[ \t]*\\(|[-+]+|?\\|\\+[-+]+\\+\\)[ \t]*$")))
                     ;; Do not match headline stars.  Do not consider
                     ;; stars of a headline as closing marker for bold
                     ;; markup either.
                     (not (and (equal marker "*")
                               (save-excursion
                                 (forward-char)
                                 (skip-chars-backward "*")
                                 (looking-at-p org-outline-regexp-bol))))
                     ;; Match full emphasis markup regexp.
                     (looking-at (if verbatim? org-verbatim-re org-emph-re))
                     ;; Do not span over paragraph boundaries.
                     (not (string-match-p org-element-paragraph-separate
                                          (match-string 2)))
                     ;; Do not span over cells in table rows.
                     (not (and (save-match-data (org-match-line "[ \t]*|"))
                               (string-match-p "|" (match-string 4))))))
              (pcase-let ((`(,_ ,face ,_) (assoc marker org-emphasis-alist)))
                (font-lock-prepend-text-property
                 (match-beginning 2) (match-end 2) 'face face)
                (when verbatim?
                  (org-remove-flyspell-overlays-in
                   (match-beginning 0) (match-end 0))
                  (remove-text-properties (match-beginning 2) (match-end 2)
                                          '(display t invisible t intangible t)))
                (add-text-properties (match-beginning 2) (match-end 2)
                                     '(font-lock-multiline t org-emphasis t)))
  
              ;; Begin new code
              (font-lock-prepend-text-property
               (match-beginning 3) (match-end 3) 'face 'org-emphasis-marker)
              (font-lock-prepend-text-property
               (match-end 4) (match-beginning 5) 'face 'org-emphasis-marker)
              ;; End new code
  
              (when org-hide-emphasis-markers
                (add-text-properties (match-end 4) (match-beginning 5)
                                     '(invisible org-link))
                (add-text-properties (match-beginning 3) (match-end 3)
                                     '(invisible org-link))))
            (throw :exit t))))))
  
  ;; thanks, https://www.reddit.com/r/emacs/comments/eipbvk/org_emphasis_marker_face/
  ;; not necessarily the best approach, but it works
  (add-hook 'org-mode-hook #'(lambda ()
                               (set-fontset-font t 'greek (font-spec :family "Helvetica" :weight 'normal :height 0.75))))
  
  ;; make sure C-; works in org-mode
  (define-key org-mode-map (kbd "C-;") #'eval-expression)
  (define-key flyspell-mode-map (kbd "C-;") #'eval-expression)
  
  
  ;; use RET for newline and to fill paragraph
  ;; (define-key org-mode-map (kbd "<RET>")
  ;;   #'(lambda ()
  ;;       (interactive)
  ;;       (newline)
  ;;       (when (equal (char-before (- (point) 1)) ?\C-j)
  ;;         (fill-paragraph))))
  
  
  ;; toggle link display
  (define-key org-mode-map (kbd "<f8>") #'org-toggle-link-display)
  
  
  ;; compile org files
  (define-key org-mode-map (kbd "<f4>") #'org-compile-files)
  (define-key dired-mode-map (kbd "<f4>") #'org-compile-files)
  
  
  ;; define term in notes
  (evil-define-key 'visual org-mode-map (kbd "C-d") #'org-note-create-definition)
  (evil-define-key 'normal org-mode-map (kbd "C-d") #'(lambda ()
                                                        (interactive)
                                                        (org-note-create-definition nil nil)))
  
  
  ;; make sure C-i works like TAB
  (defun org-c-i ()
    (interactive)
    (if (texmathp)
        (yas-expand)
      (org-cycle)))
  
  (define-key org-mode-map (kbd "C-i") #'org-c-i)
  (evil-define-key 'normal org-mode-map (kbd "C-i") #'org-c-i)
  (evil-define-key 'visual org-mode-map (kbd "C-i") #'org-c-i)
  
  
  ;; schedule todo items
  (evil-define-key 'normal org-mode-map (kbd "g s") #'org-schedule)
  
  
  ;; set todo item deadlines
  (evil-define-key 'normal org-mode-map (kbd "g d") #'org-deadline)
  
  
  ;; schedule item without being a task
  (evil-define-key 'normal org-mode-map (kbd "g .")
    #'(lambda ()
        (interactive)
        (save-excursion
          (if (equal (line-end-position) (point-max))
              (open-line-down))
          (save-excursion
            (org-previous-visible-heading 1)
            (goto-char (line-end-position))
            (insert (format "\n%s" (with-temp-buffer
                                     (org-time-stamp nil)))))
          (delete-region (1- (line-beginning-position)) (line-end-position)))))
  
  
  ;; set tag
  (evil-define-key 'normal org-mode-map (kbd "g T") #'org-set-tags-command)
  
  
  ;; save and close org capture
  (evil-define-key 'normal org-capture-mode-map (kbd "g f") #'org-capture-finalize)
  
  
  ;; save and close org capture
  (evil-define-key 'normal org-capture-mode-map (kbd "g q") #'org-capture-kill)
  
  
  ;; use org-specific jump back
  (evil-define-key 'normal org-mode-map (kbd "C-o") #'org-mark-ring-goto)
  
  
  ;; fix paragraph navigation
  (evil-define-key 'normal org-mode-map (kbd "] ]" ) #'evil-forward-section-begin)
  (evil-define-key 'normal org-mode-map (kbd "[ [" ) #'evil-backward-section-begin)
  
  
  ;; move headings
  (evil-define-key 'normal org-mode-map (kbd "C-M-k") #'org-metaup)
  (evil-define-key 'normal org-mode-map (kbd "C-M-j") #'org-metadown)
  
  
  ;; meta return in normal mode
  (evil-define-key 'normal org-mode-map (kbd "g o") #'(lambda ()
                                                        (interactive)
                                                        (end-of-line)
                                                        (org-meta-return)
                                                        (evil-insert-state)))
  
  
  ;; create headings
  (evil-define-key 'normal org-mode-map (kbd "C-n")
    #'(lambda ()
        (interactive)
        (org-insert-heading-respect-content)
        (evil-insert-state)))
  
  
  ;; create new todo haeding
  (evil-define-key 'normal org-mode-map (kbd "C-t")
    #'(lambda ()
        (interactive)
        (org-insert-todo-heading-respect-content)
        (evil-insert-state)))
  
  
  ;; copy cell contents
  (evil-define-key 'normal org-mode-map (kbd "C-c C-y")
    #'(lambda ()
        (interactive)
        (when (org-at-table-p)
          (kill-new
           (string-trim
            (substring-no-properties (org-table-get-field))))
          (message "Copied cell contents."))))
  
  
  ;; toggle source block visibility 
  (define-key org-mode-map (kbd "<f12>") #'org-toggle-blocks)
  
  
  ;; create org-mode links across files
  (global-set-key (kbd "C-c l") #'org-store-link)
  (global-set-key (kbd "C-c C-l") #'org-insert-link)
  
  
  ;; use `g j' and `g k' for navigating lists
  (add-hook 'org-mode-hook (lambda ()
                             (define-key evil-normal-state-local-map (kbd "g j") #'org-shiftdown)
                             (define-key evil-normal-state-local-map (kbd "g k") #'org-shiftup)))
  
  
  ;; export English text
  (define-key org-mode-map (kbd "<f5>") #'export-colored)
  
  
  ;; use C-S-j as M-RET
  (define-key org-mode-map (kbd "C-M-j") 'org-meta-return)
  
  
  ;; make sure `z j` and `z k` are properly bound
  (evil-define-key 'normal org-mode-map (kbd "z j") #'open-line-down)
  (evil-define-key 'normal org-mode-map (kbd "z k") #'open-line-up)
  
  
  ;; make sure that window switching works
  (evil-define-key 'normal org-mode-map (kbd "M-h") #'evil-window-left)
  (evil-define-key 'normal org-mode-map (kbd "M-j") #'evil-window-down)
  (evil-define-key 'normal org-mode-map (kbd "M-k") #'evil-window-up)
  (evil-define-key 'normal org-mode-map (kbd "M-l") #'evil-window-right)
  
  
  ;; insert today's date
  (evil-define-key 'normal org-mode-map (kbd "g D") #'(lambda ()
                                                        (interactive)
                                                        (forward-char)
                                                        (insert (format-time-string "%d/%m/%Y --- %A"))))
  (set-face-attribute 'org-block-begin-line nil
                      :extend t
                      :background color-background
                      :foreground color-dark-subdued)
  
  
  (set-face-attribute 'org-block nil
                      :foreground "#fdf4c1"
                      :background "#121112")
  
  
  (set-face-attribute 'org-code nil
                      :inherit 'shadow)
  
  
  (set-face-attribute 'org-document-info-keyword nil
                      :inherit 'shadow)
  
  
  (set-face-attribute 'org-document-title nil
                      :weight 'bold
                      :family "Optima"
                      :height 190
                      :foreground color-foreground)
  
  
  (set-face-attribute 'org-link nil
                      :inherit 'custom-link)
  
  
  (set-face-attribute 'org-meta-line nil
                      :foreground color-dark-subdued)
  
  
  (set-face-attribute 'org-property-value nil
                      :inherit nil
                      :foreground color-medium-subdued)
  
  
  (set-face-attribute 'org-special-keyword nil
                      :inherit 'font-lock-comment-face
                      :foreground color-medium-subdued)
  
  
  (set-face-attribute 'org-todo nil
                      :weight 'bold
                      :foreground color-dark-brown)
  
  
  (set-face-attribute 'org-verbatim nil
                      :inherit 'shadow
                      :foreground color-light-green)
  
  
  (set-face-attribute 'outline-1 nil
                      :extend t
                      :height 180
                      :weight 'bold
                      :foreground color-light-subdued
                      :family "Optima")
  
  
  (set-face-attribute 'outline-2 nil
                      :extend t
                      :height 160
                      :weight 'bold
                      :foreground color-light-subdued
                      :family "Optima")
  
  
  (set-face-attribute 'outline-3 nil
                      :extend t
                      :height 160
                      :weight 'bold
                      :foreground color-light-subdued
                      :family "Optima")
  
  
  (set-face-attribute 'outline-4 nil
                      :extend t
                      :height 160
                      :weight 'bold
                      :foreground color-light-subdued
                      :family "Optima")
  
  
  (set-face-attribute 'outline-5 nil
                      :extend t
                      :height 160
                      :weight 'bold
                      :foreground color-light-subdued
                      :family "Optima")
  
  
  (set-face-attribute 'outline-6 nil
                      :extend t
                      :height 160
                      :weight 'bold
                      :foreground color-light-subdued
                      :family "Optima")
  
  
  (set-face-attribute 'outline-7 nil
                      :extend t
                      :height 160
                      :weight 'bold
                      :foreground color-light-subdued
                      :family "Optima")
  
  
  (set-face-attribute 'outline-8 nil
                      :extend t
                      :height 160
                      :weight 'bold
                      :foreground color-light-subdued
                      :family "Optima")
  
  
  (set-face-attribute 'org-emphasis-marker nil
                      :foreground color-medium-subdued)
  
  
  (set-face-attribute 'org-table nil
                      :foreground color-brown)
  
  
  (set-face-attribute 'org-ellipsis nil
                      :foreground color-dark-subdued
                      :family "Victor Mono"
                      :height 120)
  
  
  (set-face-attribute 'org-tag nil
                      :foreground color-dark-subdued
                      :height 140)
  
  
  (set-face-attribute 'org-checkbox nil
                      :foreground color-brown
                      :height 140)
  
  
  (set-face-attribute 'org-latex-and-related nil
                      :foreground color-green
                      :weight 'bold)
  (use-package org-ref
    :config
  
    (require 'bibtex)
    (require 'doi-utils)
    (require 'org-ref-pdf)
    (require 'org-ref-url-utils)
    (require 'org-ref-arxiv)
    (require 'org-ref-bibtex)
    (require 'org-ref-isbn)
  
    (setq-default bibtex-autokey-year-length 4
                  bibtex-autokey-name-year-separator "-"
                  bibtex-autokey-year-title-separator "-"
                  bibtex-autokey-titleword-separator "-"
                  bibtex-autokey-titlewords 2
                  bibtex-autokey-titlewords-stretch 1
                  bibtex-autokey-titleword-length 5)
  
    (defun bibtex-generate-autokey ()
      (let* ((names (bibtex-autokey-get-names))
             (year (bibtex-autokey-get-year))
             (title (bibtex-autokey-get-title))
             (autokey (concat bibtex-autokey-prefix-string
                              title
                              (unless (or (equal names "")
                                          (equal year ""))
                                bibtex-autokey-name-year-separator)
                              names
                              (unless (or (and (equal names "")
                                               (equal year ""))
                                          (equal title ""))
                                bibtex-autokey-year-title-separator)
                              year)))
        (if bibtex-autokey-before-presentation-function
            (funcall bibtex-autokey-before-presentation-function autokey)
          autokey)))
  
    (define-key org-mode-map (kbd "C-]") #'org-ref-insert-link))
  (defvar autocorrect-words-on-type t
    "Whether to automatically correct a word after pressing space.")
  
  
  (defun correct-word-before-space ()
    (if (and
         autocorrect-words-on-type
         (memq major-mode '(org-mode mail-mode text-mode))
         (not (texmathp))
         (not (yas-active-snippets))
         (not (equal (TeX-current-macro) "text"))
         (not (org-in-src-block-p)))
        (when (and
               (equal (char-before) ? )
               (save-excursion
                 (goto-char (- (point) 1))
                 (thing-at-point 'word)))
          (save-excursion
            (flyspell-auto-correct-word)))))
  
  
  (add-hook 'post-self-insert-hook #'correct-word-before-space)
  
  
  ;; avoid triggering autocorrect
  (define-key org-mode-map (kbd "S-SPC") #'(lambda ()
                                             (interactive)
                                             (insert ? )))
  
  
  (define-key org-mode-map (kbd "<f3>") #'(lambda ()
                                            (interactive)
                                            (if autocorrect-words-on-type
                                                (progn
                                                  (setq autocorrect-words-on-type nil)
                                                  (message "Disabled autocorrect."))
                                              (setq autocorrect-words-on-type t)
                                              (message "Enabled autocorrect."))))
  (defun remove-ipython-matplotlib-line (text backend info)
    (replace-regexp-in-string "<pre class=\"example\"[[:ascii:][:nonascii:]]*?\\[&lt;matplotlib.lines.Line2D at 0x[0-9a-f]*&gt;\\][[:ascii:][:nonascii:]]*?</pre>" "" text))
  
  
  (add-to-list 'org-export-filter-drawer-functions
               'remove-ipython-matplotlib-line)
  
  
  ;; load python in org-mode
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)))
  (defun org-capture-select-template (&optional keys)
    "Select a capture template.
  Lisp programs can force the template by setting KEYS to a string."
    (let ((org-capture-templates
           (or (org-contextualize-keys
                (org-capture-upgrade-templates org-capture-templates)
                org-capture-templates-contexts)
               '(("t" "Task" entry (file+headline "" "Tasks")
                  "* TODO %?\n  %u\n  %a")))))
      (if keys
          (or (assoc keys org-capture-templates)
              (error "No capture template referred to by \"%s\" keys" keys))
        (org-mks org-capture-templates
                 (concat
                  "Select a capture template\n"
                  (make-string 25 9472))
                 "Template key: "
                 '(("C" "Customize org-capture-templates")
                   ("q" "Abort"))))))
  
  
  (defun org-mks (table title &optional prompt specials)
    "Select a member of an alist with multiple keys.
  
  TABLE is the alist which should contain entries where the car is a string.
  There should be two types of entries.
  
  1. prefix descriptions like (\"a\" \"Description\")
     This indicates that `a' is a prefix key for multi-letter selection, and
     that there are entries following with keys like \"ab\", \"ax\"...
  
  2. Select-able members must have more than two elements, with the first
     being the string of keys that lead to selecting it, and the second a
     short description string of the item.
  
  The command will then make a temporary buffer listing all entries
  that can be selected with a single key, and all the single key
  prefixes.  When you press the key for a single-letter entry, it is selected.
  When you press a prefix key, the commands (and maybe further prefixes)
  under this key will be shown and offered for selection.
  
  TITLE will be placed over the selection in the temporary buffer,
  PROMPT will be used when prompting for a key.  SPECIALS is an
  alist with (\"key\" \"description\") entries.  When one of these
  is selected, only the bare key is returned."
    (save-window-excursion
      (let ((inhibit-quit t)
            (buffer (org-switch-to-buffer-other-window "*Org Select*"))
            (prompt (or prompt "Select: "))
            case-fold-search
            current)
        (setq-local olivetti-body-width 50)
        (olivetti-mode 1)
        (unwind-protect
            (catch 'exit
              (while t
                (erase-buffer)
                (insert title "\n\n")
                (let ((des-keys nil)
                      (allowed-keys '("\C-g"))
                      (tab-alternatives '("\s" "\t" "\r"))
                      (cursor-type nil))
                  ;; Populate allowed keys and descriptions keys
                  ;; available with CURRENT selector.
                  (let ((re (format "\\`%s\\(.\\)\\'"
                                    (if current (regexp-quote current) "")))
                        (prefix (if current (concat current " ") "")))
                    (dolist (entry table)
                      (pcase entry
                        ;; Description.
                        (`(,(and key (pred (string-match re))) ,desc)
                         (let ((k (match-string 1 key)))
                           (push k des-keys)
                           ;; Keys ending in tab, space or RET are equivalent.
                           (if (member k tab-alternatives)
                               (push "\t" allowed-keys)
                             (push k allowed-keys))
                           (insert prefix "[" k "]" "..." "  " desc "..." "\n")))
                        ;; Usable entry.
                        (`(,(and key (pred (string-match re))) ,desc . ,_)
                         (let ((k (match-string 1 key)))
                           (insert prefix "[" k "]" "     " desc "\n")
                           (push k allowed-keys)))
                        (_ nil))))
                  ;; Insert special entries, if any.
                  (when specials
                    (insert (concat "\n" (make-string (- (window-width) 1) 9472) "\n" "\n"))
                    (pcase-dolist (`(,key ,description) specials)
                      (insert (format "[%s]     %s\n" key description))
                      (push key allowed-keys)))
                  ;; Display UI and let user select an entry or
                  ;; a sub-level prefix.
                  (goto-char (point-min))
                  (org-fit-window-to-buffer)
                  (message "") ; With this line the prompt appears in
                                          ; the minibuffer. Else keystrokes may
                                          ; appear, which is spurious.
                  (let ((pressed (org--mks-read-key
                                  allowed-keys prompt
                                  (not (pos-visible-in-window-p (1- (point-max)))))))
                    (setq current (concat current pressed))
                    (cond
                     ((equal pressed "\C-g") (user-error "Abort"))
                     ;; Selection is a prefix: open a new menu.
                     ((member pressed des-keys))
                     ;; Selection matches an association: return it.
                     ((let ((entry (assoc current table)))
                        (and entry (throw 'exit entry))))
                     ;; Selection matches a special entry: return the
                     ;; selection prefix.
                     ((assoc current specials) (throw 'exit current))
                     (t (error "No entry available")))))))
          (when buffer (kill-buffer buffer))))))
  
  
  ;; stop opening new window
  (defun org-switch-to-buffer-other-window (&rest args)
    "Switch to buffer in a second window on the current frame.
  In particular, do not allow pop-up frames.
  Returns the newly created buffer."
    (org-no-popups (apply #'switch-to-buffer args)))
  )

(defun org-section-num-format (numbering)
  (if (= (length numbering) 1)
      (propertize (concat (mapconcat
                           #'number-to-string
                           numbering ".") " | " )
                  'face `(:family "Victor Mono"
                                  :height 140
                                  :foreground ,color-darkish-subdued))
    (propertize (concat (mapconcat
                         #'number-to-string
                         numbering ".") " — " )
                'face `(:family "Victor Mono"
                                :height 140
                                :foreground ,color-dark-subdued))))


(font-lock-add-keywords 'org-mode
                        `(("^*+ " . 'org-meta-line)))


(setq-default org-num-format-function #'org-section-num-format)


(add-hook 'org-mode-hook #'org-num-mode)

(setq-default org-capture-bookmark nil)

(add-hook 'org-capture-mode-hook #'(lambda ()
                                     (delete-other-windows)
                                     (evil-insert-state)
                                     (setq-local header-line-format nil)))

(setq-default org-capture-templates
              '(("g" "General" entry (file+headline "~/Documents/Education/schedule.org" "General")
                 "** TODO %?")
                ("m" "Meeting" entry (file+headline "~/Documents/Education/schedule.org" "General")
                 "** %?  :meeting:")
                ("i" "BYU IS" entry (file+headline "~/Documents/Education/schedule.org" "Work")
                 "** TODO %?")
                ("r" "Research" entry (file+headline "~/Documents/Education/schedule.org" "Research")
                 "** TODO %?")
                ("v" "Event" entry (file+headline "~/Documents/Education/schedule.org" "General")
                 "** %?  :event:")
                ("c" "MATH 215" entry (file+headline "~/Documents/Education/schedule.org" "MATH 215")
                 "** TODO %?")
                ("l" "MATH 213" entry (file+headline "~/Documents/Education/schedule.org" "MATH 213")
                 "** TODO %?")
                ("a" "PHSCS 127" entry (file+headline "~/Documents/Education/schedule.org" "PHSCS 127")
                 "** TODO %?")
                ("p" "CS 111" entry (file+headline "~/Documents/Education/schedule.org" "CS 111")
                 "** TODO %?")
                ("f" "REL C 200" entry (file+headline "~/Documents/Education/schedule.org" "REL C 200")
                 "** TODO %?")
                ("w" "PHSCS 123" entry (file+headline "~/Documents/Education/schedule.org" "PHSCS 123")
                 "** TODO %?")
                ("s" "PHSCS 191" entry (file+headline "~/Documents/Education/schedule.org" "PHSCS 191")
                 "** TODO %?")
                ("j" "Major" entry (file+headline "~/Documents/Education/schedule.org" "Major")
                 "** TODO %?")
                ("e" "Emacs" entry (file+headline "~/Documents/Education/schedule.org" "Emacs")
                 "** TODO %?")))

(use-package mixed-pitch
  :config
  (setq-default mixed-pitch-set-height t)
  (set-face-attribute 'variable-pitch nil :height 1.15))

(defvar org-blocks-hidden nil)

(defun org-toggle-blocks ()
  (interactive)
  (if org-blocks-hidden
      (org-show-block-all)
    (org-hide-block-all))
  (setq-local org-blocks-hidden (not org-blocks-hidden)))

(add-hook 'org-mode-hook #'org-toggle-blocks)

(add-hook 'org-src-mode-hook #'(lambda ()
                                 (setq header-line-format "")))

(defvar my/current-line '(0 . 0)
  "(start . end) of current line in current buffer")
(make-variable-buffer-local 'my/current-line)


(defvar my/unhiding-current-line t
  "Whether to unhide current line.")


(defun my/unhide-current-line (limit)
  "Font-lock function"
  (when my/unhiding-current-line
    (let ((start (max (point) (car my/current-line)))
          (end (min limit (cdr my/current-line))))
      (when (< start end)
        (remove-text-properties start end '(invisible t display "" composition ""))
        (goto-char limit)
        t))))


(defun my/refontify-on-linemove ()
  "Post-command-hook"
  (let* ((start (line-beginning-position))
         (end (line-beginning-position 2))
         (needs-update (not (equal start (car my/current-line)))))
    (setq my/current-line (cons start end))
    (when needs-update
      (font-lock-fontify-block 2))))


(defun my/entity-unhighlight ()
  "Install"
  (font-lock-add-keywords nil '((my/unhide-current-line)) t)
  (add-hook 'post-command-hook #'my/refontify-on-linemove nil t))

(add-hook 'org-mode-hook #'my/entity-unhighlight)

(evil-define-operator evil-yank-no-newlines (beg end type register yank-handler)
  "Yank text from BEG to END with TYPE while stripping newlines."
  :move-point nil
  :repeat nil
  (interactive "<R><x><y>")
  (let ((evil-was-yanked-without-register
         (and evil-was-yanked-without-register (not register))))
    (let ((old-buffer (current-buffer)))
      (with-temp-buffer
        (insert-buffer-substring old-buffer beg end)
        (let ((fill-column (point-max)))
          (fill-region (point-min) (point-max)))
      (cond
       ((eq type 'block)
        (evil-yank-rectangle (point-min) (point-max) register yank-handler))
       ((memq type '(line screen-line))
        (evil-yank-lines (point-min) (point-max) register yank-handler))
       (t
        (evil-yank-characters (point-min) (point-max) register yank-handler)))))))


(evil-define-key 'visual org-mode-map "Y" 'evil-yank-no-newlines)

(defun org-table-blank-selected-fields ()
  (interactive)
  (let* ((start (region-beginning))
         (end (region-end))
         (start-line (line-number-at-pos start))
         (end-line (line-number-at-pos end))
         (line-count (- end-line start-line)))
    (save-excursion
      (goto-char start)
      (dotimes (_ line-count)
        (org-table-blank-field)
        (next-line))
      (org-table-blank-field))))


(evil-define-key 'visual org-mode-map (kbd "RET") #'org-table-blank-selected-fields)

(use-package auto-capitalize
  :load-path "lisp/"
  :hook (org-mode . auto-capitalize-mode)

  :config
  (setq-default auto-capitalize-predicate
                #'(lambda ()
                    (and (not (texmathp))
                         (not (org-in-src-block-p))))))

(use-package org-evil)

(use-package org-download
  :config
  (add-hook 'dired-mode-hook 'org-download-enable))

(use-package helm-org
  :config
  (evil-define-key 'normal org-mode-map "?" #'helm-org-in-buffer-headings))

(require 'appt)

(setq-default appt-time-msg-list nil)
(setq-default appt-display-interval '5)
(setq-default appt-message-warning-time '15)
(setq-default appt-display-mode-line nil)
(setq-default appt-display-format 'window)

(setq-default appt-disp-window-function (function appt-display-notification))

(appt-activate 1)


(defun appt-display-notification (min-to-app new-time msg)
  (shell-command-to-string
   (format
    "terminal-notifier -title \"Appointment in %s\" -message \"%s\" -sender org.gnu.Emacs"
    min-to-app
    msg))
  (let ((org-agenda-category-filter-preset '("-Calendar")))
    (org-agenda-list)))


(org-agenda-to-appt)
(run-at-time "24:01" 3600 'org-agenda-to-appt)
(add-hook 'org-agenda-finalize-hook #'org-agenda-to-appt)

(add-hook 'org-agenda-mode-hook #'(lambda ()
                                    (olivetti-set-width 70)
                                    (olivetti-mode 1)
                                    (setq default-mode-line-format mode-line-format)
                                    (setq mode-line-format nil)))

(defun dedicate-monitor-to-org-agenda ()
  (interactive)
  (modify-frame-parameters nil '((fullscreen . fullboth)))
  (let ((org-agenda-category-filter-preset '("-Calendar")))
    (org-agenda-list))
  (olivetti-set-width 72)
  (text-scale-set 1.5))

(defvar progenda-schedule-path "/Users/jetblack/Documents/Education/schedule.org")
(defvar progenda-publish-path "/Users/jetblack/Documents/Education/Progenda/")

(defun progenda-publish-schedule ()
  (interactive)
  (save-window-excursion
    (let* ((json-data (with-current-buffer (find-file-noselect progenda-schedule-path)
                        (ox-json-export-to-buffer)
                        (let ((contents (buffer-string)))
                          (kill-buffer)
                          contents)))
           (js-data (concat "const data = " json-data)))
      (write-region js-data nil (concat progenda-publish-path "data.js"))))
  (shell-command-to-string (format "cd %s && git add . && git commit -m 'Updated.' && git push" progenda-publish-path))
  (message "Published schedule."))

(with-eval-after-load 'org-agenda
  ;; set directory for global org files
  (setq-default org-agenda-files '("~/Documents/Education/schedule.org"
                                   "~/Documents/general.org"
                                   "~/Documents/calendar.org"))
  
  
  ;; open org-agenda in the same window
  (setq-default org-agenda-window-setup 'current-window)
  
  
  ;; stop showing warnings for deadlines
  (setq-default org-agenda-skip-deadline-prewarning-if-scheduled t)
  
  
  ;; make sure the week starts on Sunday
  (setq-default org-agenda-start-on-weekday nil)
  
  
  ;; hide deadlines by default
  (setq-default org-agenda-include-deadlines nil)
  
  
  ;; customize "now" string for agenda view; "King of the moment"
  (setq-default org-agenda-current-time-string "───*─*─  ♚  ─*─*───")
  (setq-default org-agenda-time-grid
                '((daily today require-timed)
                  (800 1000 1200 1400 1600 1800 2000)
                  "......" "--------------------"))
  
  
  ;; hide tags from agenda items
  (setq-default org-agenda-remove-tags t)
  
  
  ;; set org agenda item display
  (setq-default org-agenda-prefix-format '((agenda . "  %-12:c%?-12t")
                                           (timeline . "  % s")
                                           (todo . " %i %-12:c")
                                           (tags . " %i %-12:c")
                                           (search . " %i %-12:c")))
  
  
  ;; add line dividers between dates
  (setq-default org-agenda-format-date (lambda (date) (concat "\n"
                                                              (make-string 68 9472)
                                                              "\n"
                                                              (org-agenda-format-date-aligned date))))
  
  
  ;; set heading
  (setq-default org-agenda-overriding-header "")
  
  
  ;; set tag alignment column
  (setq-default org-tags-column 60)
  (setq-default org-agenda-tags-column 60)
  (set-face-attribute 'org-agenda-date nil
                      :foreground color-medium-subdued
                      :weight 'normal)
  
  
  (set-face-attribute 'org-agenda-date-today nil
                      :foreground color-foreground
                      :weight 'normal)
  
  
  (set-face-attribute 'org-agenda-date-weekend nil
                      :foreground color-darkish-subdued
                      :weight 'normal)
  
  
  (set-face-attribute 'org-scheduled nil
                      :foreground color-medium-subdued)
  
  
  (set-face-attribute 'org-scheduled-today nil
                      :foreground "#fdf4c1")
  
  
  (set-face-attribute 'org-scheduled-previously nil
                      :foreground "#fdf4c1"
                      :weight 'normal)
  
  
  (set-face-attribute 'org-agenda-structure nil
                      :foreground "#bdb491"
                      :weight 'ultra-bold)
  
  
  (setq-default org-priority-faces `((?A . (:foreground ,color-red :weight bold))
                                     (?B . (:foreground ,color-light-orange :weight medium))
                                     (?C . (:foreground ,color-light-green :weight normal))))
  (add-hook 'org-agenda-mode-hook
            #'(lambda ()
                (hl-line-mode 1)
                (setq-local evil-normal-state-cursor '("#151719" bar))))
  
  
  (advice-add #'org-agenda-redo :after #'(lambda (&rest args)
                                           (evil-refresh-cursor)))
  
  
  ;; move cursor to first item
  (advice-add 'org-agenda-list :after #'(lambda (&rest args)
                                          (org-agenda-next-item 1)))
  
  
  (add-hook 'org-agenda-finalize-hook #'(lambda ()
                                          (interactive)
                                          (if (> text-scale-mode-amount 0)
                                              (olivetti-set-width 72))))
  ;; make sure normal state is enabled by default in org agenda
  (evil-set-initial-state 'org-agenda-mode 'normal)
  
  
  ;; open agenda from calendar
  (evil-define-key 'normal calendar-mode-map (kbd "g l") #'org-calendar-goto-agenda)
  
  
  ;; use j k to navigate items
  (evil-define-key 'normal org-agenda-mode-map (kbd "j")
    #'(lambda ()
        (interactive)
        (org-agenda-next-item 1)
        (beginning-of-line)))
  (evil-define-key 'normal org-agenda-mode-map (kbd "k")
    #'(lambda ()
        (interactive)
        (org-agenda-previous-item 1)
        (beginning-of-line)))
  
  
  ;; save all org buffers from agenda
  (evil-define-key 'normal org-agenda-mode-map (kbd "g s") #'org-save-all-org-buffers)
  
  
  ;; open agenda item with return in normal state
  (evil-define-key 'normal org-agenda-mode-map (kbd "C-j") #'org-agenda-switch-to)
  (evil-define-key 'normal org-agenda-mode-map (kbd "RET") #'org-agenda-switch-to)
  
  
  ;; toggle todo
  (evil-define-key 'normal org-agenda-mode-map (kbd "t") #'org-agenda-todo)
  
  
  ;; reschedule task
  (evil-define-key 'normal org-agenda-mode-map (kbd "s") #'org-agenda-schedule)
  
  
  ;; refile task
  (evil-define-key 'normal org-agenda-mode-map (kbd "R") #'org-agenda-refile)
  
  
  ;; search for task
  (evil-define-key 'normal org-agenda-mode-map (kbd "S") #'org-search-view)
  
  
  ;; reload
  (evil-define-key 'normal org-agenda-mode-map (kbd "r") #'org-agenda-redo)
  
  
  ;; filter by current category
  (evil-define-key 'normal org-agenda-mode-map (kbd "c") #'org-agenda-filter-by-category)
  
  
  ;; inverse filter by current category
  (evil-define-key 'normal org-agenda-mode-map (kbd "-")
    #'(lambda ()
        (interactive)
        (org-agenda-filter-by-category t)))
  
  
  ;; set priority
  (evil-define-key 'normal org-agenda-mode-map (kbd ",") #'org-agenda-priority)
  
  
  ;; set day view
  (evil-define-key 'normal org-agenda-mode-map (kbd ".") #'org-agenda-day-view)
  
  
  ;; set week view
  (evil-define-key 'normal org-agenda-mode-map (kbd "w") #'org-agenda-week-view)
  
  
  ;; set fortnight view
  (evil-define-key 'normal org-agenda-mode-map (kbd "F") #'org-agenda-fortnight-view)
  
  
  ;; goto date
  (evil-define-key 'normal org-agenda-mode-map (kbd "f") #'org-agenda-goto-date)
  
  
  ;; delete item
  (evil-define-key 'normal org-agenda-mode-map (kbd "D") #'org-agenda-kill)
  
  
  ;; set deadline
  (evil-define-key 'normal org-agenda-mode-map (kbd "d") #'org-agenda-deadline)
  
  
  ;; toggle deadlines
  (evil-define-key 'normal org-agenda-mode-map (kbd "g d") #'org-agenda-toggle-deadlines)
  
  
  ;; filter
  (evil-define-key 'normal org-agenda-mode-map (kbd "/") #'org-agenda-filter)
  
  
  ;; add tag
  (evil-define-key 'normal org-agenda-mode-map (kbd "'") #'org-agenda-set-tags)
  
  
  ;; add and display text related to item
  (evil-define-key 'normal org-agenda-mode-map (kbd "E") #'org-agenda-add-note)
  (evil-define-key 'normal org-agenda-mode-map (kbd "e") #'org-agenda-entry-text-mode)
  
  
  ;; exit agenda
  (evil-define-key 'normal org-agenda-mode-map (kbd "x") #'org-agenda-exit))

(use-package olivetti)

(defun message-filter-center (args)
  "Center message string.
This is a :filter-args advice for `message`."
  (if (car args)
      (with-current-buffer (window-buffer (minibuffer-window))
        (let* ((str (apply #'format-message args))
               (filler (make-string (max 0 (/ (- (window-width (minibuffer-window)) (string-width str)) 2)) ? )))
          (list "%s%s" filler str)))
    args))


(defun org-detect-distraction-free-keyword ()
  (if (and (org-keyword-activep "DFREE" t) (not (string-prefix-p "*scratch" (buffer-name))))
      (distraction-free (org-keyword-activep "LITERARY"))))


(add-hook 'org-mode-hook #'org-detect-distraction-free-keyword)


(defvar distraction-free-background-set nil)


(defvar stored-face-attributes nil)


(defun set-temporary-face-attributes (attribute value faces)
  (dolist (face faces)
    (add-to-list 'stored-face-attributes
                 (list face attribute (face-attribute face attribute)))
    (set-face-attribute face nil attribute value)))


(defun reset-temporary-face-attributes ()
  (dolist (item stored-face-attributes)
    (set-face-attribute (car item) nil (nth 1 item) (nth 2 item)))
  (setq stored-face-attributes nil))


(defun set-distraction-free-background ()
  (if (org-keyword-activep "LITERARY")
      (progn
        (when (not distraction-free-background-set)
          (setq evil-emacs-state-cursor `(,color-background box))
          (setq evil-normal-state-cursor `(,color-background box))
          (setq evil-insert-state-cursor `(,color-background bar))

          (set-temporary-face-attributes
           :foreground color-background
           '(org-document-title
             header-line
             outline-1
             outline-2
             outline-3
             outline-4
             outline-5
             outline-6
             outline-7
             outline-8))

          (set-temporary-face-attributes
           :background "#FFF"
           '(org-block-begin-line
             org-block-end-line
             sp-pair-overlay-face
             header-line
             yas-field-highlight-face))

          (set-temporary-face-attributes
           :foreground color-green
           '(yas-field-highlight-face))

          (set-background-color "#FFF")
          (set-foreground-color color-background))
        (setq distraction-free-background-set t))
    (when (and distraction-free-background-set
               (not pdf-has-set-light-mode)
               (or helm-alive-p
                   (not (equal major-mode 'minibuffer-mode))))
      (setq evil-emacs-state-cursor `("white" box))
      (setq evil-normal-state-cursor `("#fdf4c1" box))
      (setq evil-insert-state-cursor `("#fdf4c1" bar))
      (reset-temporary-face-attributes)
      (set-background-color color-background)
      (set-foreground-color color-foreground))
    (setq distraction-free-background-set nil)))


(add-hook 'post-command-hook #'set-distraction-free-background)


(defun distraction-free (use-mixed-pitch &optional no-spell)
  (interactive "P")
  (if (equal olivetti-mode nil)
      (progn
        (when use-mixed-pitch
          (olivetti-set-width 75)
          (mixed-pitch-mode 1)
          (setq-local line-spacing 4))
        (setq-local default-mode-line-format mode-line-format)
        (setq-local mode-line-format nil)
        (olivetti-mode 1)
        (if (not no-spell)
            (flyspell-mode)))
    (mixed-pitch-mode 0)
    (olivetti-mode 0)
    (set-window-margins (get-buffer-window) 2)
    (setq-local mode-line-format default-mode-line-format)
    (setq-local line-spacing nil)))


(add-hook 'evil-insert-state-entry-hook
          #'(lambda ()
              (when line-spacing
                (save-excursion
                  (end-of-line)
                  (insert ? )))))


(add-hook 'evil-insert-state-exit-hook
          #'(lambda ()
              (when line-spacing
                (save-excursion
                  (end-of-line)
                  (when (equal (char-before) ? )
                    (delete-backward-char 1))))))

(use-package osx-dictionary
  :config
  (defun osx-dictionary--goto-dictionary (word)
    "Switch to osx-dictionary buffer in other window."
    (setq osx-dictionary-previous-window-configuration
          (current-window-configuration))
    (let* ((buffer (osx-dictionary--get-buffer word))
           (window (get-buffer-window buffer)))
      (if (null window)
          (switch-to-buffer buffer)
        (select-window window))))
  (defun osx-dictionary--view-result (word)
    "Make buffer for the searching result of WORD."
    (if word
        (with-current-buffer (get-buffer-create
                              (funcall osx-dictionary-generate-buffer-name-function word))
          (let ((inhibit-read-only t))
            (erase-buffer)
            (let ((progress-reporter
                   (make-progress-reporter (format "Searching (%s)..." word)
                                           nil nil)))
              (insert (osx-dictionary--search word))
              (progress-reporter-done progress-reporter))
            (osx-dictionary--goto-dictionary word)
            (goto-char (point-min))
  
            (olivetti-set-width 70)
            (olivetti-mode 1)
            (mixed-pitch-mode 1)
            (setq default-mode-line-format mode-line-format)
            (setq mode-line-format nil)
            (setq line-spacing 4)
  
            (let ((buffer-read-only nil))
              (whitespace-cleanup))))
      (message "Nothing to look up")))
  
  
  (defun osx-dictionary-quit ()
    "Quit osx-dictionary: reselect previously selected buffer."
    (interactive)
    (mixed-pitch-mode 0)
    (olivetti-mode 0)
    (setq mode-line-format default-mode-line-format)
    (set-window-margins (get-buffer-window) 2)
    (setq line-spacing nil)
    (if (window-configuration-p osx-dictionary-previous-window-configuration)
        (progn
          (set-window-configuration osx-dictionary-previous-window-configuration)
          (setq osx-dictionary-previous-window-configuration nil)
          (kill-buffer))
      (kill-buffer)))
  )

(defface phonetics-lock
  '((t :family "Gentium Plus"))
  "Face for dictionary phonetics."
  :group 'osx-dictionary-mode)


(defvar phonetics-lock 'phonetics-lock
  "Face for dictionary phonetics.")


(setq-default osx-dictionary-mode-font-lock-keywords
              '(
                ;; Word class
                ("\\b\\(noun\\|adjective\\|det\\|verb\\|adverb\\|abbreviation\\|preposition\\|suffix\\|prefix\\|conjunction\\|symb\\)\\b" . font-lock-type-face)
                ;; Serial number
                ("^[0-9]+" . font-lock-builtin-face)
                ;; Phonetics
                ("|\\(.*\\)|" . phonetics-lock)
                ;; Dictionary comment
                ("^\\(DERIVATIVES\\|ORIGIN\\|PHRASES\\)" . font-lock-comment-face)))


;; center header
(defun center-header (header)
  "Center HEADER string."
  (let ((header-length 70))
    (append
     `(,(make-string (- (/ (window-total-width) 2)
                        (/ header-length 2))
                     (string-to-char " ")))
     header)))


(define-derived-mode osx-dictionary-mode fundamental-mode "osx-dictionary"
  "Major mode to look up word through dictionary.
\\{osx-dictionary-mode-map}.
Turning on Text mode runs the normal hook `osx-dictionary-mode-hook'."

  (setq header-line-format (center-header osx-dictionary-mode-header-line))
  (setq font-lock-defaults '(osx-dictionary-mode-font-lock-keywords)))

(add-hook 'osx-dictionary-mode-hook
          #'(lambda ()
              (evil-local-set-key 'normal (kbd "q") #'osx-dictionary-quit)))

(defun clean-quick-calc (start end)
  (interactive
   (if (region-active-p)
       (list (region-beginning) (region-end))
     '(nil nil)))

  (if (null start)
      (with-temp-buffer
        (quick-calc t)
        (kill-ring-save (point-min) (point-max)))
    (progn
      (let* ((expression-start start)
             (prefix (save-excursion
                       (goto-char start)
                       (condition-case nil
                           (progn
                             (re-search-forward "\\([[:space:]]*&= \\)\\|\\([[:space:]]*= \\)" end)
                             (setq expression-start (match-end 0))
                             (match-string 0))
                         (error ""))))
             (expression (replace-regexp-in-string
                          "\\([abcxyz]\\|_[[:digit:]]\\)[[:space:]]*("
                          "\\1*("
                          (buffer-substring expression-start end))))
        (delete-region start (if (and (equal (evil-visual-type) 'line)
                                      (not (equal end (point-max))))
                                 (- end 1) end))
        (save-excursion
          (goto-char start)
          (insert prefix)
          (insert (calc-eval expression)))))))

(defun evaluate-math-expression-at-point (substition start end)
  (interactive
   (append (list (read-string "Evaluate at (e.g., \"x=1,y=2\"): "))
           (if (use-region-p)
               (list (region-beginning) (region-end))
             (list (line-beginning-position) (line-end-position)))))

  (save-excursion
    (goto-char end)
    (insert (let ((contents (buffer-substring start end)))
              (with-temp-buffer
                (insert (string-substitute-math-variables contents substition))
                (clean-quick-calc (point-min) (point-max))
                (buffer-string))))
    (delete-region start end)))

(defun evaluate-math-expression-at-limits (var a b start end)
  (interactive
   (append (split-string (read-string "Evaluate at (e.g., \"x,1,2\"): ") ",")
           (if (use-region-p)
               (list (region-beginning) (region-end))
             (list (line-beginning-position) (line-end-position)))))

  (save-excursion
    (goto-char end)
    (insert (let ((contents (buffer-substring start end)))
              (with-temp-buffer
                (insert (format "(%s) - (%s)"
                                (string-substitute-math-variables contents (format "%s=%s" var b))
                                (string-substitute-math-variables contents (format "%s=%s" var a))))
                (clean-quick-calc (point-min) (point-max))
                (buffer-string))))
    (delete-region start end)))


(evil-define-key 'normal global-map (kbd "g r l") #'evaluate-math-expression-at-limits)
(evil-define-key 'normal global-map (kbd "g r p") #'evaluate-math-expression-at-point)

(define-key global-map (kbd "C-'") #'clean-quick-calc)
(define-key org-mode-map (kbd "C-'") #'clean-quick-calc)

(add-hook 'calc-mode-hook #'(lambda ()
                              (calc-latex-language nil)
                              (calc-symbolic-mode 1)
                              (calc-frac-mode 1)
                              (calc-radians-mode)))

(defun follow-zoom-link (link-url)
  "Open zoommtg:// links."
  (shell-command (format "open 'zoommtg:%s'" link-url)))


(org-link-set-parameters "zoommtg" :follow #'follow-zoom-link)

(defun maxwell-create-link ()
  (interactive)
  (if (not (file-exists-p "mxl"))
      (make-directory "mxl"))
  (let* ((cwd (file-name-directory buffer-file-name))
         (name (make-temp-name "diagram_"))
         (image-path (concat cwd (file-name-as-directory "mxl") name ".png")))
    (shell-command
     (format
      "python -c 'from maxwell import capture_area; capture_area(\"%s\")'"
      image-path))
    (select-frame-set-input-focus (selected-frame))
    (insert (format "#+attr_latex: :placement [H] :scale .4\n[[%s]]\n\n" image-path))
    (org-remove-inline-images)))


(define-key org-mode-map (kbd "<f7>") #'maxwell-create-link)

(define-minor-mode maxwell-get-latex-prompt-mode
  "Minor mode to simulate buffer local keybindings."
  :init-value nil
  :keymap (make-keymap))


(define-key maxwell-get-latex-prompt-mode-map (kbd "C-j")
            #'(lambda ()
                (interactive)
                (write-file (expand-file-name "~/temp.org"))
                (kill-buffer)
                (delete-frame)
                (shell-command-to-string "osascript -e 'activate application \"Electron\"'")))


(defun maxwell-get-latex-prompt ()
  (let ((should-center-frame nil))
    (make-frame `((width . 50)
                  (height . 13)
                  (left . ,(- (round (/ (display-pixel-width) 2)) 202))
                  (top . -200)))
    (set-frame-parameter nil 'height 5)
    (set-frame-parameter nil 'top -100))
  (select-frame-set-input-focus (selected-frame))
  (switch-to-buffer "*Prompt*")
  (erase-buffer)
  (org-mode)
  (setq-local olivetti-body-width 40)
  (maxwell-get-latex-prompt-mode)
  (yas-expand-snippet "\\\\( $0 \\\\)")
  (evil-insert-state))

(defun arxiv-local-add-bibtex-entry (arxiv-number)
  "Add bibtex entry for ARXIV-NUMBER to current buffer."
  (interactive
   (list (read-string
          "arxiv: "
          (arxiv-maybe-arxiv-id-from-current-kill))))
  (save-excursion
    (goto-char (point-max))
    (when (not (looking-at "^")) (insert "\n"))
    (insert (arxiv-get-bibtex-entry-via-arxiv-api arxiv-number))
    (org-ref-clean-bibtex-entry)
    (goto-char (point-max))
    (when (not (looking-at "^")) (insert "\n"))))


(defun arxiv-local-get-pdf-add-bibtex-entry (arxiv-number pdfdir)
  "Add bibtex entry for ARXIV-NUMBER to current buffer.
Remove troublesome chars from the bibtex key, retrieve a pdf
for ARXIV-NUMBER and save it to PDFDIR with the same name of the
key."
  (interactive
   (list (read-string
          "arxiv: "
          (arxiv-maybe-arxiv-id-from-current-kill))
         default-directory))

  (arxiv-local-add-bibtex-entry arxiv-number)

  (save-excursion
    (let ((key ""))
      (goto-char (point-max))
      (bibtex-beginning-of-entry)
      (re-search-forward bibtex-entry-maybe-empty-head)
      (if (match-beginning bibtex-key-in-head)
          (progn
            (setq key (delete-and-extract-region
                       (match-beginning bibtex-key-in-head)
                       (match-end bibtex-key-in-head)))
            ;; remove potentially troublesome characters from key
            ;; as it will be used as  a filename
            (setq key (replace-regexp-in-string   "\"\\|\\*\\|/\\|:\\|<\\|>\\|\\?\\|\\\\\\||\\|\\+\\|,\\|\\.\\|;\\|=\\|\\[\\|]\\|!\\|@"
                                                  "" key))
            ;; check if the key is in the buffer
            (when (save-excursion
                    (bibtex-search-entry key))
              (save-excursion
                (bibtex-search-entry key)
                (bibtex-copy-entry-as-kill)
                (switch-to-buffer-other-window "*duplicate entry*")
                (bibtex-yank))
              (setq key (bibtex-read-key "Duplicate Key found, edit: " key))))
        (setq key (bibtex-read-key "Key not found, insert: ")))
      (insert key)
      (arxiv-get-pdf arxiv-number (concat pdfdir key ".pdf"))
      ;; Check that it worked, and insert a field for it.
      (when (file-exists-p (concat pdfdir key ".pdf"))
        (bibtex-end-of-entry)
        (backward-char)
        (insert (format "  file = {%s}\n  " (concat pdfdir key ".pdf")))))))


(defun doi-utils-local-async-download-pdf ()
  "Download the PDF for bibtex entry at point asynchronously.
It is not fully async, only the download is. Fully async is
harder because you need to run `doi-utils-get-pdf-url' async
too. "
  (interactive)
  (require 'async)
  (save-excursion
    (bibtex-beginning-of-entry)
    (let (;; get doi, removing http://dx.doi.org/ if it is there.
          (doi (replace-regexp-in-string
                "https?://\\(dx.\\)?.doi.org/" ""
                (bibtex-autokey-get-field "doi")))
          (key (cdr (assoc "=key=" (bibtex-parse-entry))))
          (pdf-url)
          (pdf-file))

      (setq pdf-file (concat default-directory key ".pdf"))

      (unless doi (error "No DOI found to get a pdf for"))

      (when (file-exists-p pdf-file)
        (error "%s already exists. Delete to re-download" pdf-file))

      ;; (doi-utils-get-pdf-url "10.1063/1.5019667")
      ;; If you get here, try getting the pdf file
      (async-start
       `(lambda ()
          (setq package-user-dir ,package-user-dir)
          (require 'package)
          (package-initialize)
          (setq load-path (list ,@load-path))
          (require 'doi-utils)

          (setq pdf-url (doi-utils-get-pdf-url ,doi))
          (when pdf-url
            (url-copy-file pdf-url ,pdf-file t)

            (let* ((header (with-temp-buffer
                             (set-buffer-multibyte nil)
                             (insert-file-contents-literally ,pdf-file nil 0 5)
                             (buffer-string)))
                   (valid (and (stringp header)
                               (string-equal (encode-coding-string header 'utf-8) "%PDF-"))))
              (if valid
                  (format "%s downloaded" ,pdf-file)
                (delete-file ,pdf-file)
                (require 'browse-url)
                (browse-url pdf-url)
                (message "Invalid pdf (file deleted). Header = %s" header)))))
       `(lambda (result)
          (message "doi-utils-async-download-pdf: %s"  result))))))


(defun doi-utils-local-get-bibtex-entry-pdf (&optional arg)
  "Download pdf for entry at point if the pdf does not already exist locally.
The entry must have a doi. The pdf will be saved, by the name
%s.pdf where %s is the bibtex label. Files will not be
overwritten. The pdf will be checked to make sure it is a pdf,
and not some html failure page. You must have permission to
access the pdf. We open the pdf at the end if
`doi-utils-open-pdf-after-download' is non-nil.

With one prefix ARG, directly get the pdf from a file (through
`read-file-name') instead of looking up a DOI. With a double
prefix ARG, directly get the pdf from an open buffer (through
`read-buffer-to-switch') instead. These two alternative methods
work even if the entry has no DOI, and the pdf file is not
checked."
  (interactive "P")
  (save-excursion
    (bibtex-beginning-of-entry)
    (let (;; get doi, removing http://dx.doi.org/ if it is there.
          (doi (replace-regexp-in-string
                "https?://\\(dx.\\)?.doi.org/" ""
                (bibtex-autokey-get-field "doi")))
          (key (cdr (assoc "=key=" (bibtex-parse-entry))))
          (pdf-url)
          (pdf-file))

      (setq pdf-file (concat default-directory key ".pdf"))
      ;; now get file if needed.
      (unless (file-exists-p pdf-file)
	(cond
	 ((and (not arg)
	       doi
	       (setq pdf-url (doi-utils-get-pdf-url doi)))
	  (url-copy-file pdf-url pdf-file)
	  ;; now check if we got a pdf
          (if (org-ref-pdf-p pdf-file)
              (message "%s saved" pdf-file)
            (delete-file pdf-file)
            (message "No pdf was downloaded.")
            (browse-url pdf-url)))
	 ((equal arg '(4))
	  (copy-file (expand-file-name (read-file-name "Pdf file: " nil nil t))
		     pdf-file))
	 ((equal arg '(16))
	  (with-current-buffer (read-buffer-to-switch "Pdf buffer: ")
	    (write-file pdf-file)))
	 (t
	  (message "We don't have a recipe for this journal.")))

	(when (file-exists-p pdf-file)
	  (bibtex-set-field "file" pdf-file))

	(when (and doi-utils-open-pdf-after-download (file-exists-p pdf-file))
	  (org-open-file pdf-file))))))


(defun doi-utils-add-bibtex-entry-from-doi (doi)
  "Add DOI entry to end of a file in the current directory.
Pick the file ending with .bib or in .  If you have an active region that
starts like a DOI, that will be the initial prompt.  If no region
is selected and the first entry of the ‘kill-ring’ starts like a
DOI, then that is the intial prompt.  Otherwise, you have to type
or paste in a DOI.
Argument BIBFILE the bibliography to use."
  (interactive
   (list (read-string
          "DOI: "
          ;; now set initial input
          (doi-utils-maybe-doi-from-region-or-current-kill))))

  
  ;; Wrap in save-window-excursion to restore your window arrangement after this
  ;; is done.
  (save-excursion
      ;; Check if the doi already exists
      (goto-char (point-min))
      (if (re-search-forward (concat doi "\\_>") nil t)
          (message "%s is already in this file" doi)
        (goto-char (point-max))

	(when (not (looking-back "\n\n" (min 3 (point))))
	  (insert "\n\n"))

        (doi-utils-insert-bibtex-entry-from-doi doi))))


(defun doi-utils-local-add-bibtex-entry-from-doi (doi)
  "Add DOI entry to end of a file in the current directory.
Pick the file ending with .bib or in .  If you have an active region that
starts like a DOI, that will be the initial prompt.  If no region
is selected and the first entry of the ‘kill-ring’ starts like a
DOI, then that is the intial prompt.  Otherwise, you have to type
or paste in a DOI.
Argument BIBFILE the bibliography to use."
  (interactive
   (list (read-string
          "DOI: "
          ;; now set initial input
          (doi-utils-maybe-doi-from-region-or-current-kill))))

  ;; Wrap in save-window-excursion to restore your window arrangement after this
  ;; is done.
  (save-excursion
      ;; Check if the doi already exists
      (goto-char (point-min))
      (if (re-search-forward (concat doi "\\_>") nil t)
          (message "%s is already in this file" doi)
        (goto-char (point-max))

	(when (not (looking-back "\n\n" (min 3 (point))))
	  (insert "\n\n"))

        (doi-utils-insert-bibtex-entry-from-doi doi))))


(defun orcb-local-download-pdf ()
  "Try to get the pdf in an entry."
  ;; try to get pdf
  (when doi-utils-download-pdf
    (if doi-utils-async-download
        (doi-utils-local-async-download-pdf)
      (doi-utils-local-get-bibtex-entry-pdf))))


(setq-default org-ref-clean-bibtex-entry-hook '(org-ref-bibtex-format-url-if-doi
                                                orcb-key-comma
                                                org-ref-replace-nonascii
                                                orcb-&
                                                orcb-%
                                                org-ref-title-case-article
                                                orcb-clean-year
                                                orcb-key
                                                orcb-clean-doi
                                                orcb-clean-pages
                                                orcb-check-journal
                                                org-ref-sort-bibtex-entry
                                                orcb-fix-spacing
                                                orcb-local-download-pdf))

(defun org-ref-read-key ()
  "Read a key with completion."
  (unless bibtex-completion-display-formats-internal
    (bibtex-completion-init))
  (let* ((bibtex-completion-bibliography (org-ref-find-bibliography))
	 (candidates (mapcar (lambda (entry)
			       (cons (bibtex-completion-format-entry entry (- (frame-width) 4))
				     (cdr entry)))
			     (bibtex-completion-candidates)))
	 (choice (completing-read "org-ref BibTeX entries: " candidates)))
    (cdr (assoc "=key=" (assoc choice candidates)))))

(evil-define-key 'insert bibtex-mode-map (kbd "C-]") #'doi-utils-add-bibtex-entry-from-doi)
(evil-define-key 'normal bibtex-mode-map (kbd "C-]") #'doi-utils-add-bibtex-entry-from-doi)

(evil-define-key 'insert bibtex-mode-map (kbd "C-M-]") #'arxiv-local-get-pdf-add-bibtex-entry)
(evil-define-key 'normal bibtex-mode-map (kbd "C-M-]") #'arxiv-local-get-pdf-add-bibtex-entry)

(setq org-latex-default-packages-alist '())


(add-to-list 'org-latex-classes
             `("assignment"
               "
\\documentclass[a4paper,12pt]{article}

\\usepackage[doublespacing]{setspace}
\\usepackage[margin=1in]{geometry}
\\usepackage{csquotes}
\\usepackage{amsmath}
\\usepackage{graphicx}
\\usepackage[hidelinks]{hyperref}
\\usepackage[style=apa,backend=biber]{biblatex}
\\usepackage[small]{titlesec}

[NO-DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


(add-to-list 'org-latex-classes
             '("apa6"
               "
\\documentclass[a4paper,man]{apa6}

\\usepackage{csquotes}
\\usepackage{graphicx}
\\usepackage[hidelinks]{hyperref}
\\usepackage[style=apa,backend=biber]{biblatex}

\\renewcommand*{\\finalnamedelim}{ \\ifnumgreater{\\value{liststop}}{2}{\\finalandomma}{} \\addspace\\&\\space}

\\usepackage[skip=0pt]{parskip}

[NO-DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


(add-to-list 'org-latex-classes
             '("math-document"
               "
\\documentclass[a4paper,12pt,oneside]{book}
\\usepackage[margin=1in]{geometry}
\\usepackage{csquotes}
\\usepackage{amsmath}
\\usepackage{amssymb}
\\usepackage{gensymb}
\\usepackage{siunitx}
\\usepackage{float}
\\usepackage{graphicx}
\\usepackage[hidelinks]{hyperref}
\\usepackage{fancyhdr}
\\usepackage{xhfill}
\\usepackage[svgnames]{xcolor}
\\usepackage[explicit]{titlesec}
\\usepackage[defaultsans,scale=1]{opensans}
\\usepackage[T1]{fontenc}
\\usepackage{anyfontsize}
\\usepackage[many]{tcolorbox}
\\usepackage{amsthm}
\\usepackage{tikz}
\\usepackage{microtype}
\\usepackage{adjustbox}

\\definecolor{title-foreground}{RGB}{230,230,230}
\\definecolor{subtitle-foreground}{RGB}{120,120,120}
\\definecolor{chapter-foreground}{RGB}{80,80,80}

\\definecolor{theorem-background}{RGB}{0,53,110}
\\definecolor{definition-background}{RGB}{0,87,153}
\\definecolor{example-background}{RGB}{86,152,179}
\\definecolor{note-background}{RGB}{0,53,110}

\\colorlet{rulecolor}{Gainsboro!40!Lavender}
\\titleformat{\\chapter}[display]
{\\filcenter}{\\mbox{}\\xrfill[0.4ex]{3pt}[rulecolor]\\textsc{\\large\\enspace\\chaptername~\\thechapter}\\enspace\\xrfill[0.4ex]{3pt}[rulecolor]\\mbox{}}{0.3ex}
{{\\color{rulecolor}\\titlerule[1pt]}\\vskip3ex\\huge\\sffamily\\color{chapter-foreground}#1}[\\vskip1.2ex{\\color{rulecolor}\\titlerule[1pt]}]

\\titleformat{\\section}{\\sffamily\\large}{\\thesection.}{.5em}{#1}
\\titleformat{\\subsection}{\\sffamily\\large}{\\thesubsection.}{.5em}{#1}

\\tcbuselibrary{theorems}

\\newtcbtheorem[number within=chapter]{old-theorem}{Theorem}{
  breakable,
  colback=theorem-background!10,
  colframe=theorem-background!55!black,
  fonttitle=\\bfseries\\sffamily}{th}

\\newtcbtheorem[number within=chapter]{old-exmp}{Example}{
  breakable,
  colback=example-background!10,
  colframe=example-background!55!black,
  fonttitle=\\bfseries\\sffamily}{ex}

\\newtcbtheorem[number within=chapter]{old-definition}{Definition}{
  breakable,
  colback=definition-background!10,
  colframe=definition-background!65!black,
  fonttitle=\\bfseries\\sffamily}{def}

\\newtcolorbox{note}[1][]{
  enhanced jigsaw,
  borderline west={2pt}{0pt}{note-background},
  sharp corners,
  boxrule=0pt,
  fonttitle={\\bfseries\\sffamily},
  coltitle={black},
  title={Note:\\ },
  attach title to upper,
  #1
}

\\renewenvironment{theorem}[1][]{%
  \\begin{old-theorem}{#1}{}%
    
}{%
  \\end{old-theorem}%
}

\\renewenvironment{exmp}{%
  \\begin{old-exmp}{}{}%
    
}{%
  \\end{old-exmp}%
}

\\renewenvironment{definition}{%
  \\begin{old-definition}{}{}%
    
}{%
  \\end{old-definition}%
}

\\newtheorem{exercise}{Exercise}
\\numberwithin{exercise}{chapter}
\\newtheorem{lemma}{Lemma}[theorem]
\\numberwithin{lemma}{chapter}
\\newtheorem{corollary}{Corollary}[theorem]
\\numberwithin{corollary}{chapter}

\\setlength{\\parskip}{.5em}
\\setlength{\\parindent}{0pt}

\\pagestyle{fancy}
\\fancyhf{}
\\renewcommand{\\chaptermark}[1]{\\markboth{#1}{}}
\\renewcommand{\\sectionmark}[1]{\\markright{SECTION \\thesection. \\uppercase{#1}}}
\\let\\oldtitle\\title
\\renewcommand{\\title}[1]{\\oldtitle{#1}\\def\\titletext{#1}}
\\fancyhead[L]{\\leftmark}
\\fancyhead[R]{\\titletext}
\\fancyfoot[C]{\\rightmark}
\\fancyfoot[R]{\\thepage}

\\newcommand*{\\textinrule}[3][]{%
  \\makebox[#2]{#1%
    \\leaders\\hrule height \\dimexpr.5ex+.2pt\\relax depth \\dimexpr -.5ex+.2pt\\relax \\hfill% Left rule
    \\enskip{#3}\\enskip% Text (and surrounding spaces)
    \\leaders\\hrule height \\dimexpr.5ex+.2pt\\relax depth \\dimexpr -.5ex+.2pt\\relax \\hfill\\kern0pt}% Right rule
}

[NO-DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]"
               ("\\chapter{%s}" . "\\chapter{%s}")
               ("\\section{%s}" . "\\section{%s}")
               ("\\subsection{%s}" . "\\subsection{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection{%s}")
               ("\\paragraph{%s}" . "\\paragraph{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph{%s}")))


(add-to-list 'org-latex-classes
             '("math-homework"
               "
\\documentclass[a4paper,12pt,oneside]{book}
\\usepackage[margin=1in]{geometry}
\\usepackage{csquotes}
\\usepackage{amsmath}
\\usepackage{amssymb}
\\usepackage{gensymb}
\\usepackage{siunitx}

\\usepackage{float}

\\usepackage{graphicx}
\\usepackage[hidelinks]{hyperref}
\\usepackage{fancyhdr}
\\usepackage[explicit]{titlesec}

\\titleformat{\\chapter}{\\normalfont\\huge}{#1}{}{}
\\titleformat{\\section}{\\normalfont\\large}{}{.5em}{#1}
\\titleformat{\\subsection}{\\normalfont\\large}{}{.5em}{\\textit{#1}}

\\titlespacing{\\chapter}{0pt}{0pt}{1em}

\\usepackage{amsthm}
\\setlength{\\parskip}{.5em}
\\setlength{\\parindent}{0pt}
\\newtheoremstyle{boxed}{8pt}{8pt}{}{}{\\upshape\\bfseries}{.}{.5em}{}
\\theoremstyle{boxed}
\\newtheorem{theorem}{Theorem}
\\numberwithin{theorem}{chapter}
\\newtheorem{note}{Note}
\\numberwithin{note}{chapter}
\\newtheorem{exmp}{Example}
\\numberwithin{exmp}{chapter}
\\newtheorem{corollary}{Corollary}
\\numberwithin{corollary}{chapter}
\\newtheorem{exercise}{Exercise}
\\numberwithin{exercise}{chapter}
\\newtheorem{definition}{Definition}
\\numberwithin{definition}{chapter}
\\newtheorem{lemma}{Lemma}[theorem]
\\numberwithin{lemma}{chapter}
\\usepackage{mdframed}
\\BeforeBeginEnvironment{theorem}{\\begin{mdframed}}
\\AfterEndEnvironment{theorem}{\\end{mdframed}}
\\BeforeBeginEnvironment{corollary}{\\begin{mdframed}}
\\AfterEndEnvironment{corollary}{\\end{mdframed}}
\\BeforeBeginEnvironment{exmp}{\\begin{mdframed}}
\\AfterEndEnvironment{exmp}{\\end{mdframed}}
\\BeforeBeginEnvironment{exercise}{\\begin{mdframed}}
\\AfterEndEnvironment{exercise}{\\end{mdframed}}
\\BeforeBeginEnvironment{definition}{\\begin{mdframed}}
\\AfterEndEnvironment{definition}{\\end{mdframed}}
\\pagestyle{fancy}
\\fancyhf{}

\\renewcommand{\\chaptermark}[1]{\\markboth{#1}{}}

\\let\\oldtitle\\title
\\renewcommand{\\title}[1]{\\oldtitle{#1}\\def\\titletext{#1}}
\\fancyhead[L]{\\leftmark}
\\fancyhead[R]{\\titletext}
\\fancyfoot[R]{\\thepage}

[NO-DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]"
               ("\\chapter{%s}" . "\\chapter{%s}")
               ("\\section*{%s}" . "\\section*{%s}")
               ("\\subsection*{%s}" . "\\subsection*{%s}")
               ("\\subsubsection*{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph{%s}")))

(defun export-and-open-html ()
  "Export \"org-mode\" file to HTML and open it."
  (interactive)
  (shell-command (format "open -a Vivaldi '%s'" (org-html-export-to-html))))


(defvar org-pdf-through-latex t
  "Export \"org-mode\" file to latex before converting to PDF. Otherwise, convert to HTML first.")


(defun export-and-open-pdf (separate-window)
  "Export \"org-mode\" file to PDF, then preview."
  (interactive)
  (let ((output-path (format "%s.pdf" (file-name-sans-extension (buffer-file-name)))))
    (if org-pdf-through-latex
        (shell-command-to-string
         (format
          "latexmk -pdf -jobname=temp -f %s; rm *.aux *.fls *.log *.out *.fdb_latexmk; mv temp.pdf '%s'"
          (org-latex-export-to-latex)
          output-path))
      (shell-command-to-string (format
                                "wkhtmltopdf --disable-smart-shrinking %s %s"
                                (org-html-export-to-html) output-path)))
    (if separate-window
        (find-file-other-window output-path)
      (find-file output-path))))


(defvar org-pdf-export-running nil)
(defvar org-pdf-export-has-queue nil)


(defun org-export-pdf-update ()
  "Export \"org-mode\" file to PDF in background."
  (interactive)

  (if org-pdf-export-running
      (setq org-pdf-export-has-queue t)
    (setq org-pdf-export-running t)
    (let* ((output-path (format "%s.pdf" (file-name-sans-extension (buffer-file-name))))
           (output-buffer (generate-new-buffer "*Async shell command*"))
           (inhibit-message t)
           (proc (save-window-excursion
                   (async-shell-command
                    (format
                     "latexmk -pdf -jobname=temp -f %s; rm *.aux *.fls *.log *.out *.fdb_latexmk; mv temp.pdf '%s'"
                     (org-latex-export-to-latex)
                     output-path)
                    output-buffer)
                   (get-buffer-process output-buffer))))
      (if (process-live-p proc)
          (set-process-sentinel proc #'(lambda (process signal)
                                         (when (memq (process-status process) '(exit signal))
                                           (setq org-pdf-export-running nil)
                                           (kill-buffer "*Async shell command*")
                                           (shell-command-sentinel process signal)

                                           (when org-pdf-export-has-queue
                                             (setq org-pdf-export-has-queue nil)
                                             (org-export-pdf-update)))))))))

(defvar org-html-custom-css-path (concat emacs-configuration-directory "org_style.css")
  "Path to default custom css.")


;; org-mode custom HTML head export
(defun org-html-export-head-hook (exporter)
  "Insert custom inline head."
  (when (eq exporter 'html)
    (let* ((dir (ignore-errors (file-name-directory (buffer-file-name))))
           (path (concat dir "style.css"))
           (homestyle (or (null dir) (null (file-exists-p path))))
           (final (if homestyle org-html-custom-css-path path)))
      (setq org-html-head-include-default-style nil)
      (setq org-html-head (concat
                           "<style type=\"text/css\">\n"
                           "<!--/*--><![CDATA[/*><!--*/\n"
                           (with-temp-buffer
                             (insert-file-contents final)
                             (buffer-string))
                           "/*]]>*/-->\n"
                           "</style>\n"
                           "\n"
                           "<script>
document.addEventListener('DOMContentLoaded', () => {
    document.querySelectorAll('table.currency td.org-right').forEach(elem => {
        const text = elem.innerText;
        if (!isNaN(text)) {
            let num = (Math.round(parseFloat(text) * 100) / 100).toFixed(2);
            elem.innerText = '$' + num.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');
        }
    });
});
</script>")))))


(add-hook 'org-export-before-processing-hook #'org-html-export-head-hook)


(setq-default org-html-mathjax-template
              "<script type=\"text/x-mathjax-config\">
    MathJax.Ajax.config.path[\"mhchem\"] = \"https://cdnjs.cloudflare.com/ajax/libs/mathjax-mhchem/3.3.2\";
    MathJax.Hub.Config({
        displayAlign: \"%ALIGN\",
        displayIndent: \"%INDENT\",

        \"HTML-CSS\": { scale: %SCALE,
                        linebreaks: { automatic: \"%LINEBREAKS\" },
                        webFont: \"%FONT\"
                       },
        SVG: {scale: %SCALE,
              linebreaks: { automatic: \"%LINEBREAKS\" },
              font: \"%FONT\"},
        NativeMML: {scale: %SCALE},
        TeX: { equationNumbers: {autoNumber: \"%AUTONUMBER\"},
               MultLineWidth: \"%MULTLINEWIDTH\",
               TagSide: \"%TAGSIDE\",
               TagIndent: \"%TAGINDENT\",
               extensions: [\"[mhchem]/mhchem.js\"]
             },
    });
</script>
<script src=\"%PATH\"></script>")

(defun export-colored (num simple-separatorp)
  (interactive
   (list
    (read-number "How many people? ")
    (y-or-n-p "Use simple separators? ")))
  (if (equal major-mode 'org-mode)
      (progn
        (shell-command (format "~/tcolor %s %s %s" (buffer-file-name) num simple-separatorp))
        (let ((old-buffer-path (buffer-file-name)))
          (with-current-buffer (find-file-noselect (with-temp-buffer
                                                     (insert old-buffer-path)
                                                     (goto-char (point-min))
                                                     (replace-regexp "\\(.+\\)\\.org" "\\1_color.org")
                                                     (buffer-string)))
            (shell-command (format "open %s" (org-html-export-to-html))))))
    (message "Please make sure this a `.org` file.")))

(defun expand-org-latex-workspace ()
  (interactive)
  (toggle-buffer-in-buffer-ring)
  (evil-goto-line)
  (org-latex-preview-pdf 1)
  (make-frame '((fullscreen . fullboth)))
  (export-and-open-pdf t)
  (evil-collection-pdf-view-goto-page)
  (toggle-buffer-in-buffer-ring)
  (evil-window-left 1))

(use-package ichthys-mode
  :load-path "lisp/"
  :config
  ;; binding to insert link in org-mode
  (define-key org-mode-map (kbd "C-c i") #'ichthys/insert-link)

  ;; create a ring for Portuguese and English scriptures
  (ichthys/create-lang-ring
   "~/Documents/Church/Ichthys/Emacs/Portuguese"
   "~/Documents/Church/Ichthys/Emacs/English")

  ;; use <f9> to cycle between languages
  (define-key global-map (kbd "<f9>") #'ichthys/lang-ring-next))

(use-package gomoku
  :config
  (set-face-attribute 'gomoku-O nil
                      :weight 'bold
                      :foreground "#cc6666")

  (set-face-attribute 'gomoku-X nil
                      :weight 'bold
                      :foreground "#6ca17a"))

(use-package pong
  :config
  (define-key pong-mode-map (kbd "s") #'pong-move-left)
  (define-key pong-mode-map (kbd "d") #'pong-move-right)
  (define-key pong-mode-map (kbd "k") #'pong-move-up)
  (define-key pong-mode-map (kbd "j") #'pong-move-down))

;; configure .curve for json
(add-to-list 'auto-mode-alist '("\\.curve\\'" . json-mode))


;; remove bell sound
(setq-default ring-bell-function 'ignore)


;; delete a file by moving it to trash
(setq-default delete-by-moving-to-trash t)
(setq-default trash-directory "~/.Trash")


;; stop making backup files
(setq-default make-backup-files nil)

(require 'calendar)
(require 'holidays)


(defvar custom-holidays
  '((holiday-fixed 1 1 "New Year's Day")
    (holiday-float 1 1 3 "Martin Luther King Day")
    (holiday-fixed 2 2 "Groundhog Day")
    (holiday-fixed 2 14 "Valentine's Day")
    (holiday-float 2 1 3 "President's Day")
    (holiday-fixed 3 17 "St. Patrick's Day")
    (holiday-fixed 4 1 "April Fools' Day")
    (holiday-float 5 0 2 "Mother's Day")
    (holiday-float 5 1 -1 "Memorial Day")
    (holiday-fixed 6 14 "Flag Day")
    (holiday-fixed 6 19 "Juneteenth")
    (holiday-float 6 0 3 "Father's Day")
    (holiday-fixed 7 4 "Independence Day")
    (holiday-fixed 7 25 "Pioneer Day")
    (holiday-float 9 1 1 "Labor Day")
    (holiday-float 10 1 2 "Columbus Day")
    (holiday-fixed 10 31 "Halloween")
    (holiday-fixed 11 11 "Veteran's Day")
    (holiday-float 11 4 4 "Thanksgiving")
    (holiday-easter-etc)
    (holiday-fixed 12 25 "Christmas")
    (solar-equinoxes-solstices)
    (holiday-sexp calendar-daylight-savings-starts
                  (format "Daylight Saving Time Begins %s"
                          (solar-time-string
                           (/ calendar-daylight-savings-starts-time . #1=((float 60)))
                           calendar-standard-time-zone-name)))
    (holiday-sexp calendar-daylight-savings-ends
                  (format "Daylight Saving Time Ends %s"
                          (solar-time-string
                           (/ calendar-daylight-savings-ends-time . #1#)
                           calendar-daylight-time-zone-name)))
    (holiday-easter-etc -47 "Carnaval")
    (holiday-easter-etc -2 "Paixão de Cristo")
    (holiday-fixed 4 21 "Tiradentes")
    (holiday-fixed 5 1 "Dia do Trabalhador")
    (holiday-fixed 7 25 "Pioneer Day")
    (holiday-easter-etc 60 "Corpus Christi")
    (holiday-fixed 9 7 "Dia da Independência do Brasil")
    (holiday-fixed 10 12 "Nossa Senhora Aparecida")
    (holiday-fixed 11 2 "Finados")
    (holiday-fixed 11 15 "Proclamação da República")
    (holiday-fixed 12 25 "Natal"))
  "Only the ones I want (though frankly, I'd like to observe more).")


(setq-default org-agenda-include-diary t)
(setq-default calendar-holidays custom-holidays)

(add-hook 'calendar-mode-hook #'(lambda ()
                                  (evil-local-set-key 'normal (kbd "q") #'(lambda ()
                                                                            (interactive)
                                                                            (calendar-exit t)))))

(setq-default mac-command-modifier 'meta)

(use-package reveal-in-osx-finder)

(defun before-evil-quit (orig-fun &rest args)
  "Work around macOS fullscreen quit crash."
  (if (one-window-p)
      (set-frame-parameter nil 'fullscreen nil)))
(advice-add 'evil-quit :before #'before-evil-quit)

(use-package atomic-chrome
  :config
  (setq-default atomic-chrome-buffer-open-style 'full)


  (atomic-chrome-start-server))
