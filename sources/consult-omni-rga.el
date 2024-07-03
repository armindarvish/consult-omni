;;; consult-omni-rga.el --- Consulting Rga Command -*- lexical-binding: t -*-

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
"rga --null --line-buffered --color=never --max-columns=1000 --path-separator /\
   --smart-case --no-heading --line-number --with-filename"
  "Command line arguments for rga (ripgrep-all), see `consult-omni-rga'.
The dynamically computed arguments are appended.
Can be either a string, or a list of strings or expressions."
  :type '(choice string (repeat (choice string sexp))))

(defun consult-omni--rga-transform (candidates &optional query)
  "Formats candidates of `consult-omni-rga'."
(consult-omni--grep-format candidates :source "rga" :query query :regexp-pattern consult-omni-rga-match-regexp))

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
         (let ((matches (pdf-isearch-search-page query)))
           (add-to-history 'search-ring (isearch-string-propertize query))
           (when (listp matches)
             (pdf-isearch-hl-matches (car-safe matches) matches t)
             (pdf-isearch-focus-match (car-safe matches)))))
        (_ nil)))
     (t
      (if (buffer-narrowed-p) (widen))
      (and (stringp pos) (goto-line (string-to-number pos)))
      (when (derived-mode-p 'org-mode)
        (org-fold-show-entry))
      (recenter nil t)
      (when consult-omni-highlight-matches
        (consult-omni--overlay-match query nil consult-omni-highlight-match-ignore-case)
        (add-to-history 'search-ring (isearch-string-propertize query)))
      (consult-omni--pulse-line)
      )))
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
         (let ((matches (pdf-isearch-search-page query)))
           (add-to-history 'search-ring (isearch-string-propertize query))
           (when (listp matches)
             (pdf-isearch-hl-matches (car-safe matches) matches t)
             (pdf-isearch-focus-match (car-safe matches)))))
        (_ nil)))
     (t
      (if (buffer-narrowed-p) (widen))
      (and (stringp pos) (goto-line (string-to-number pos)))
      (when (derived-mode-p 'org-mode)
        (org-fold-show-entry))
      (recenter nil t)
      (when consult-omni-highlight-matches
        (consult-omni--overlay-match query nil consult-omni-highlight-match-ignore-case)
        (add-to-history 'search-ring (isearch-string-propertize query))
        (consult-omni-overlays-toggle))
      (consult-omni--pulse-line)
      ))))

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
               (consult-ripgrep-args "rga --null --line-buffered --color=never --max-columns=1000 --path-separator /\ --smart-case --no-heading --line-number --with-filename"))
    (funcall (consult-omni--grep-make-builder #'consult--ripgrep-make-builder dir) query)))

;; Define the Ripgrep Source
(consult-omni-define-source "rga"
                            :narrow-char ?r
                            :type 'async
                            :require-match t
                            :face 'consult-omni-engine-title-face
                            :request #'consult-omni--rga-builder
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
