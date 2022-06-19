;;; ichthys-mode.el --- Navigate LDS scriptures in org mode

;; Author: William Aguiar Roque <william.aroque@gmail.com>
;; Version: 1.0

;;; Commentary:

;; This is a minor mode for navigating and reading LDS scriptures in
;; org mode.

;; Use `ichthys/get-scripture' to find a chapter and `goto-verse'
;; to find a verse.

;;; Code:

(require 'json)


(defun ichthys/get-json (path)
  "Load JSON from PATH into hashmap."
  (let* ((json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string)
         (json (json-read-file path)))
    json))


(defun ichthys/get-book (scripture-hash &optional book)
  "Choose book from SCRIPTURE-HASH.  Might be BOOK if it exists."
  (let*
      ((name-order (gethash "nameOrder" scripture-hash))
       (abbr-order (gethash "bookOrder" scripture-hash))
       (book (if book
                 book
               (completing-read "Choose book: " name-order nil t)))
       (book-index (cl-position book name-order :test 'equal))
       (abbr (nth book-index abbr-order))
       (chapters (gethash abbr scripture-hash)))
    (list chapters book)))


(defun ichthys/get-chapter (chapters &optional chapter)
  "Choose chapter from CHAPTERS.  Might be CHAPTER if it exists."
  (let*
      ((chapter-numbers (mapcar #'number-to-string (number-sequence 1 (length chapters))))
       (chapter-number (if chapter
                           chapter
                         (completing-read "Choose chapter: " chapter-numbers nil t)))
       (chapter-index (- (string-to-number chapter-number) 1))
       (chapter (nth chapter-index chapters)))
    (list chapter chapter-number)))


(defun ichthys/goto-verse (verse)
  "Go to VERSE."
  (interactive "nGoto verse: ")
  (goto-char (point-min))
  (dotimes (_ (- verse 1))
    (forward-paragraph))
  (forward-line)
  (evil-scroll-line-to-top (line-number-at-pos)))


(defface highlighted-verse
  '((t :foreground "#a19e91"))
  "Face for highlighted verses."
  :group 'ichthys)


(defun ichthys/highlight-verse (verse)
  "Highlight VERSE."
  (interactive "nHighlight verse: ")
  (save-excursion
    (goto-char (point-min))
    (dotimes (_ (- verse 1))
      (forward-paragraph))
    (let ((start (point)))
      (forward-paragraph)
      (put-text-property start (point)
       'font-lock-face 'highlighted-verse))))


(defun ichthys/open-chapter (chapter buffer-name &optional verse-numbers)
  "Open CHAPTER in BUFFER-NAME.  Might also go to VERSE if it exists."

  (switch-to-buffer buffer-name)
  (erase-buffer)

  (org-mode)
  (ichthys-mode)

  (mixed-pitch-mode 1)
  (olivetti-mode 1)

  (let ((inhibit-message t))
    (olivetti-set-width 64))

  (setq default-mode-line-format mode-line-format)
  (setq mode-line-format nil)
  (setq line-spacing 4)

  (dolist (verse chapter)
    (insert (replace-regexp-in-string "^\\([0-9]+\\)" "*\\1*" (string-trim verse)))

    (setq fill-column 70)
    (insert "\n\n"))

  (fill-region 0 (point-max))

  (if verse-numbers
      (progn
        (ichthys/goto-verse (car verse-numbers))
        (when (> (length verse-numbers) 1)
          (dolist (verse-number (apply #'number-sequence verse-numbers))
            (ichthys/highlight-verse verse-number))))
    (goto-char (point-min))))


(defconst scripture-names '("Book of Mormon" "Old Testament" "New Testament" "Pearl of Great Price" "D&C")
  "Names of the scriptures.")


(defconst scripture-abbreviations '("bofm" "ot" "nt" "nt" "dc")
  "Abbreviations of the scriptures.")


(defconst scripture-paths '("bofm.json" "ot.json" "nt.json" "pgp.json" "dc.json" )
  "Paths of scripture data files.")


(defconst scripture-index-paths '("bofm_index.json" "ot_index.json" "nt_index.json" "pgp_index.json")
  "Paths of scripture index files.")


(defvar ichthys-base-path)


(defvar ichthys/lang-ring)
(defvar ichthys/lang-index)


(defun ichthys/lang-ring-next ()
  "Go to next element in language ring."
  (interactive)
  (setq ichthys/lang-index (1+ ichthys/lang-index))

  (let ((new-path (ring-ref ichthys/lang-ring ichthys/lang-index)))
    (setq ichthys-base-path new-path)
    (message "Set language to %s." (file-name-nondirectory new-path))))


(defun ichthys/create-lang-ring (&rest lang-paths)
  "Create a ring for paths to scriptures in different languages from LANG-PATHS."
  (setq ichthys/lang-ring (make-ring (length lang-paths)))
  (dolist (path (reverse lang-paths))
    (ring-insert ichthys/lang-ring (expand-file-name path)))

  (setq ichthys/lang-index -1)
  (ichthys/lang-ring-next))


;; Defaults to data path
(ichthys/create-lang-ring "~/Documents/Ichthys/data")


(defun ichthys/get-scripture (&optional scripture book passage)
  "Choose scripture.  Use SCRIPTURE, BOOK, and PASSAGE if they exist."
  (interactive)
  (let*
      ((scripture (if scripture
                      scripture
                    (completing-read "Choose scripture: " scripture-names nil t)))
       (scripture-index (cl-position scripture scripture-names :test 'equal))
       (scripture-hashmap (ichthys/get-relative (nth scripture-index scripture-paths)))

       (verse-numbers (nth 1 passage))
       (verse-numbers (when verse-numbers (split-string verse-numbers "-")))
       (verse-numbers (mapcar #'string-to-number verse-numbers)))
    
    (if (equal scripture "D&C")
        (let*
            ((chapter (ichthys/get-chapter scripture-hashmap (car passage)))
             (chapter-contents (car chapter))
             (chapter-number (nth 1 chapter)))
          (ichthys/open-chapter
           chapter-contents
           (format "*D&C %s*" chapter-number)
           verse-numbers))

      (let*
          ((book (ichthys/get-book scripture-hashmap book))
           (chapters (car book))
           (book-name (nth 1 book))
           (chapter (ichthys/get-chapter chapters (car passage)))
           (chapter-contents (car chapter))
           (chapter-number (nth 1 chapter)))

        (ichthys/open-chapter
         chapter-contents
         (format "*%s ― %s %s*" scripture book-name chapter-number)
         verse-numbers)))))


(defun ichthys/get-relative (path)
  "Get JSON from PATH relative to base path."
  (ichthys/get-json (concat
                     (file-name-as-directory ichthys-base-path)
                     path)))


(defun ichthys/get-indices ()
  "Get scripture indices."
  (mapcar #'ichthys/get-relative scripture-index-paths))


(defun ichthys/find-book-in-indices (book indices)
  "Find BOOK in scripture INDICES and return scripture name or nil."
  (let ((scripture-index (cl-some (lambda (index)
                                    (when (gethash book index)
                                      (cl-position index indices :test 'equal)))
                                  indices)))

    (when scripture-index
      (nth scripture-index scripture-names))))


(defun ichthys/parse-scripture (scripture)
  "Parse SCRIPTURE string and return formatted list."
  (let*
      ((scripture (split-string scripture))
       (book-element (string-join (butlast scripture) " "))
       (passage-element (car (last scripture))))

    (when (> (length scripture) 1)
      (if (equal "D&C" book-element)
          (list "D&C" nil (split-string passage-element ":"))

        (let*
            ((scripture-indices (ichthys/get-indices))
             (scripture-name (ichthys/find-book-in-indices book-element scripture-indices))
             (passage (split-string passage-element ":")))

          (when scripture-name
            (list scripture-name book-element passage)))))))


(defun ichthys/org-follow (scripture)
  "Parse and follow SCRIPTURE string."
  (apply 'ichthys/get-scripture (ichthys/parse-scripture scripture)))


(defun ichthys/org-export (scripture description export-format)
  "Link SCRIPTURE to church website when EXPORT-FORMAT is set to html.  If available, use DESCRIPTION."
  (when (equal export-format 'html)
    (let*
        ((scripture (ichthys/parse-scripture scripture))
         (scripture-name (car scripture))
         (passage (nth 2 scripture)))

      (if (equal scripture-name "D&C")
          (let*
              ((path (concat
                      (format "https://www.churchofjesuschrist.org/study/scriptures/dc-testament/dc/%s" (car passage))
                      (if (> (length passage) 1)
                          (format ".%s" (nth 1 passage))
                        "")))
               (desc (or description path)))

            (format "<a href=\"%s\">%s</a>" path desc))
        
        (let*
            ((scripture-index (cl-position scripture-name scripture-names))
             (scripture-abbreviation (nth scripture-index scripture-abbreviations))
             (index (ichthys/get-relative (nth scripture-index scripture-index-paths)))
             (book-abbreviation (gethash (nth 1 scripture) index))
             (path (concat
                    (format "https://www.churchofjesuschrist.org/study/scriptures/%s/%s/%s" scripture-abbreviation book-abbreviation (car passage))
                    (if (> (length passage) 1)
                        (format ".%s" (nth 1 passage))
                      "")))
             (desc (or description path)))

          (format "<a href=\"%s\">%s</a>" path desc))))))


(defun ichthys/insert-link (scripture)
  "Insert `org-mode' link for SCRIPTURE."
  (interactive "MWhich scripture? ")
  (insert (format "[[ichthys:%s][%s]]" scripture scripture)))


(org-link-set-parameters "ichthys"
                         :follow #'ichthys/org-follow
                         :export #'ichthys/org-export)


;;;###autoload
(define-minor-mode ichthys-mode
  "Navigate scriptures in Portuguese within Emacs."
  :lighter " ichthys"
  :keymap (let ((map (make-sparse-keymap)))
            (use-package evil
              :config
              (evil-define-minor-mode-key 'normal 'ichthys-mode "gl" #'ichthys/goto-verse))
            map))


(provide 'ichthys-mode)
;;; ichthys-mode.el ends here
