;;; consult-omni-rga.el --- Consulting Ripgrep-all Command -*- lexical-binding: t -*-

;; Copyright (C) 2024 Armin Darvish

;; Author: Armin Darvish
;; Maintainer: Armin Darvish
;; Created: 2024
;; Version: 0.1
;; Package-Requires: ((emacs "28.1") (consult "1.4") (consult-omni "0.1"))
;; Homepage: https://github.com/armindarvish/consult-omni
;; Keywords: convenience

;;; Commentary:

;;; Code:

(require 'consult-omni)
(require 'consult-omni-grep)

(defconst consult-omni-rga-match-regexp "\\`\\(?:\\./\\)?\\([^\n\0]+\\)\0\\([0-9]+\\)\\([-:]\\)\\([pP]age\s\\)?\\([0-9]+\\)?\\(.*\\)[-:\0]"
  "Regexp used to match file and line of grep output.")

(defcustom consult-omni-rga-args
'("rga" "--null" "--line-buffered" "--color=never" "--max-columns=1000" "--path-separator" "/" "--smart-case" "--no-heading" "--with-filename" "--line-number")
  "Command line arguments for rga (ripgrep-all), see `consult-omni-rga'.
The dynamically computed arguments are appended.
Can be either a string, or a list of strings or expressions."
  :type '(choice string (repeat (choice string sexp))))

(cl-defun consult-omni--rga-format (candidates &rest args &key source query regexp-pattern)
  "Formats candidates for grep based commands.

Adopted from `consult--grep-format'."
(let* ((frame-width-percent (floor (* (frame-width) 0.1)))
         (file "")
         (file-len 0)
         (file-str)
         (cand)
         result)
    (save-match-data
      (dolist (str candidates)
        (when (and (not (string-prefix-p "Error" str))
                   (not (string-prefix-p "Syntax Error" str))
                   (string-match regexp-pattern str)
                   ;; Filter out empty context lines
                   (or (/= (aref str (match-beginning 3)) ?-)
                       (/= (match-end 0) (length str))))
          ;; We share the file name across candidates to reduce
          ;; the amount of allocated memory.
          (unless (and (= file-len (- (match-end 1) (match-beginning 1)))
                       (eq t (compare-strings
                              file 0 file-len
                              str (match-beginning 1) (match-end 1) nil)))
            (setq file (file-truename (match-string 1 str)))
            (if (and file (stringp file) (> file-len (* frame-width-percent 2)))
              (setq file-str (consult-omni--set-string-width (string-remove-prefix (file-truename default-directory) file) (* frame-width-percent 2) (* frame-width-percent 1)))
              (setq file-str (string-remove-prefix (file-truename default-directory) file)))
            (setq file-len (length file-str)))
          (let* ((line (match-string 2 str))
                 (ctx (and (numberp (match-beginning 3)) (= (aref str (match-beginning 3)) ?-)))
                 (sep (if ctx "-" ":"))
                 (page (match-string 5 str))
                 (page-str (and page (concat "Page " page)))
                 (content (substring str (match-end 0)))
                 (line-len (length line)))
            (when (length> content consult-grep-max-columns)
              (setq content  (consult-omni--set-string-width content consult-grep-max-columns)))
             (setq cand (concat file-str sep line sep page-str sep content))
            ;; Store file name in order to avoid allocations in `consult--prefix-group'
            (add-text-properties 0 file-len `(face consult-file consult--prefix-group ,file) cand)
            (put-text-property (1+ file-len) (+ 1 file-len line-len) 'face 'consult-line-number cand)
            (when ctx
              (add-face-text-property (+ 2 file-len line-len) (length str) 'consult-grep-context 'append cand))
            (push (propertize cand :source source :title cand :query query :file file :pos line :page page :content content) result)))))
    result))

(defun consult-omni--rga-filter (candidates &optional query)
  "Filters candidates for `consult-omni-rga."
  (seq-filter (lambda (candidate)
                (not (string-match "^Error:.*$" candidate nil nil)))
              candidates))

(defun consult-omni--rga-transform (candidates &optional query)
  "Formats candidates of `consult-omni-rga'."
(consult-omni--rga-format candidates :source "rga" :query query :regexp-pattern consult-omni-rga-match-regexp))

(defun consult-omni--rga-preview (cand)
  "Preview function for `consult-omni-rga'."
  (let ((file (get-text-property 0 :file cand))
        (pos (get-text-property 0 :pos cand))
        (page (get-text-property 0 :page cand))
        (content (get-text-property 0 :content cand))
        (query (get-text-property 0 :query cand)))
    (with-current-buffer (funcall #'consult--file-action file)
      (cond
       ((string-suffix-p ".pdf" file)
        (pcase major-mode
          ('doc-view-mode
           (and (stringp page) (doc-view-goto-page (string-to-number page))))
          ('pdf-view-mode
           (and (stringp page) (pdf-view-goto-page (string-to-number page)))
           (when consult-omni-highlight-matches
             (add-to-history 'search-ring (isearch-string-propertize query))
             (when-let ((matches (pdf-isearch-search-page query)))
               (setq pdf-isearch-current-matches matches)
               (setq pdf-isearch-current-match (car-safe matches))
               (pdf-isearch-hl-matches pdf-isearch-current-match pdf-isearch-current-matches t)
               (pdf-isearch-focus-match pdf-isearch-current-match))))
          (_ nil)))
       (t
        (if (buffer-narrowed-p) (widen))
        (and (stringp pos) (goto-line (string-to-number pos)))
        (when (derived-mode-p 'org-mode)
          (org-fold-show-entry))
        (recenter nil t)
        (when consult-omni-highlight-matches
          (add-to-history 'search-ring (isearch-string-propertize query))
          (consult-omni--overlay-match query nil consult-omni-highlight-match-ignore-case))
        (consult-omni--pulse-line))))
    nil))

(defun consult-omni--rga-callback (cand)
  "Callback function for `consult-omni-rga'."
  (let ((file (get-text-property 0 :file cand))
        (pos (get-text-property 0 :pos cand))
        (page (get-text-property 0 :page cand))
        (content (get-text-property 0 :content cand))
        (query (get-text-property 0 :query cand)))
    (funcall  #'consult--file-action file)
    (cond
     ((string-suffix-p ".pdf" file)
      (pcase major-mode
        ('doc-view-mode
         (and (stringp page) (doc-view-goto-page (string-to-number page))))
        ('pdf-view-mode
         (and (stringp page) (pdf-view-goto-page (string-to-number page)))
         (when consult-omni-highlight-matches
             (add-to-history 'search-ring (isearch-string-propertize query))
             (when-let ((matches (pdf-isearch-search-page query)))
               (setq pdf-isearch-current-matches matches)
               (setq pdf-isearch-current-match (car-safe matches))
               (pdf-isearch-hl-matches pdf-isearch-current-match pdf-isearch-current-matches t)
               (pdf-isearch-focus-match pdf-isearch-current-match))))
        (_ nil)))
     (t
      (if (buffer-narrowed-p) (widen))
      (and (stringp pos) (goto-line (string-to-number pos)))
      (when (derived-mode-p 'org-mode)
        (org-fold-show-entry))
      (recenter nil t)
      (when consult-omni-highlight-matches
        (add-to-history 'search-ring (isearch-string-propertize query))
        (consult-omni--overlay-match query nil consult-omni-highlight-match-ignore-case)
        (consult-omni-overlays-toggle))
      (consult-omni--pulse-line)))))

(cl-defun consult-omni--rga-builder (input &rest args &key callback &allow-other-keys)
  "Makes builder command line args for “ripgrep”."
  (pcase-let* ((`(,query . ,opts) (consult-omni--split-command input (seq-difference args (list :callback callback))))
               (opts (car-safe opts))
               (count (plist-get opts :count))
               (dir (plist-get opts :dir))
               (dir (if dir (file-truename (format "%s" dir))))
               (count (or (and count (integerp (read count)) (string-to-number count))
                          consult-omni-default-count))
               (default-directory (or dir default-directory))
               (consult-ripgrep-args consult-omni-rga-args))
    (funcall (consult-omni--grep-make-builder #'consult--ripgrep-make-builder dir) query)))

;; Define the Ripgrep Source
(consult-omni-define-source "rga"
                            :narrow-char ?r
                            :type 'async
                            :require-match t
                            :face 'consult-omni-engine-title-face
                            :request #'consult-omni--rga-builder
                            ;; :filter #'consult-omni--rga-filter
                            :transform #'consult-omni--rga-transform
                            :on-preview #'consult-omni--rga-preview
                            :on-return #'identity
                            :on-callback #'consult-omni--rga-callback
                            :preview-key consult-omni-preview-key
                            :search-hist 'consult-omni--search-history
                            :select-hist 'consult-omni--selection-history
                            :group #'consult-omni--group-function
                            :enabled (lambda () (if (and (executable-find "rga")
                                                        (fboundp 'consult--ripgrep-make-builder))
                                                    t nil))
                            :sort t
                            :static 'both
                            :annotate nil)

;;; provide `consult-omni-rga' module

(provide 'consult-omni-rga)

(add-to-list 'consult-omni-sources-modules-to-load 'consult-omni-rga)
;;; consult-omni-rga.el ends here
