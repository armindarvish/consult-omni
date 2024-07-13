;;; consult-omni-ripgrep-all.el --- Consulting Ripgrep-all Command -*- lexical-binding: t -*-

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

(defconst consult-omni-ripgrep-all-match-regexp "\\`\\(?:\\./\\)?\\([^\n\0]+\\)\0\\([0-9]+\\)\\([-:]\\)\\([pP]age\s\\)?\\([0-9]+\\)?\\(.*\\)[-:\0]"
  "Regexp used to match file and line of grep output.")

(defcustom consult-omni-ripgrep-all-args
'("rga" "--null" "--line-buffered" "--color=never" "--max-columns=1000" "--path-separator" "/" "--smart-case" "--no-heading" "--with-filename" "--line-number")
  "Command line arguments for rga (ripgrep-all), see `consult-omni-ripgrep-all'.
The dynamically computed arguments are appended.
Can be either a string, or a list of strings or expressions."
  :type '(choice string (repeat (choice string sexp))))

(cl-defun consult-omni--ripgrep-all-format (candidates &rest args &key source query regexp-pattern)
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
            (let* ((file (file-truename (match-string 1 str)))
                   (line (match-string 2 str))
                   (ctx (and (numberp (match-beginning 3)) (= (aref str (match-beginning 3)) ?-)))
                   (sep (if ctx "-" ":"))
                   (content (substring str (or (match-end 5) (match-end 4) (match-end 3) (match-end 2) (match-end 1) (match-end 0))))
                   (content (if (length> content (* frame-width-percent 6)) (consult-omni--set-string-width content (* frame-width-percent 6))
                              content))
                   (page (match-string 5 str))
                   (page-str (and page (concat "Page " page)))
                   (line-len (length line))
                   (file-str (string-remove-prefix (file-truename default-directory) file))
                   (file-str (if (and (stringp file-str) (> (length file-str) (* frame-width-percent 5)))
                                 (consult-omni--set-string-width file-str (* frame-width-percent 5) (* frame-width-percent 1))
                               file-str))
                   (file-len (length file-str))
                   (cand (concat file-str sep line sep page-str sep content)))

            ;; Store file name in order to avoid allocations in `consult--prefix-group'
            (add-text-properties 0 file-len `(face consult-file consult--prefix-group ,file) cand)
            (put-text-property (1+ file-len) (+ 1 file-len line-len) 'face 'consult-line-number cand)
            (when ctx
              (add-face-text-property (+ 2 file-len line-len) (length cand) 'consult-grep-context 'append cand))
            (push (propertize cand :source source :title cand :query query :file file :pos line :page page :content content) result)))))
    result))

(defun consult-omni--ripgrep-all-transform (candidates &optional query)
  "Formats candidates of `consult-omni-ripgrep-all'."
(consult-omni--ripgrep-all-format candidates :source "ripgrep-all" :query query :regexp-pattern consult-omni-ripgrep-all-match-regexp))

(defun consult-omni--ripgrep-all-preview (cand)
  "Preview function for `consult-omni-ripgrep-all'."
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

(defun consult-omni--ripgrep-all-callback (cand)
  "Callback function for `consult-omni-ripgrep-all'."
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

(cl-defun consult-omni--ripgrep-all-builder (input &rest args &key callback &allow-other-keys)
  "Makes builder command line args for “ripgrep”."
  (pcase-let* ((`(,query . ,opts) (consult-omni--split-command input (seq-difference args (list :callback callback))))
               (opts (car-safe opts))
               (count (plist-get opts :count))
               (dir (plist-get opts :dir))
               (dir (if dir (file-truename (format "%s" dir))))
               (count (or (and count (integerp (read count)) (string-to-number count))
                          consult-omni-default-count))
               (default-directory (or dir default-directory))
               (consult-ripgrep-args consult-omni-ripgrep-all-args))
    (funcall (consult-omni--grep-make-builder #'consult--ripgrep-make-builder default-directory) query)))

;; Define the Ripgrep Source
(consult-omni-define-source "ripgrep-all"
                            :narrow-char ?r
                            :type 'async
                            :require-match t
                            :face 'consult-omni-engine-title-face
                            :request #'consult-omni--ripgrep-all-builder
                            :transform #'consult-omni--ripgrep-all-transform
                            :on-preview #'consult-omni--ripgrep-all-preview
                            :on-return #'identity
                            :on-callback #'consult-omni--ripgrep-all-callback
                            :preview-key consult-omni-preview-key
                            :search-hist 'consult-omni--search-history
                            :select-hist 'consult-omni--selection-history
                            :group #'consult-omni--group-function
                            :enabled (lambda () (if (and (executable-find "rga")
                                                        (fboundp 'consult--ripgrep-make-builder))
                                                    t nil))
                            :sort nil
                            :static 'both
                            :annotate nil)

;;; provide `consult-omni-ripgrep-all' module

(provide 'consult-omni-ripgrep-all)

(add-to-list 'consult-omni-sources-modules-to-load 'consult-omni-ripgrep-all)
;;; consult-omni-ripgrep-all.el ends here
