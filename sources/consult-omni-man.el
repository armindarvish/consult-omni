;;; consult-omni-man.el --- Consulting Man Command -*- lexical-binding: t -*-

;; Copyright (C) 2024 Armin Darvish

;; Author: Armin Darvish
;; Maintainer: Armin Darvish
;; Created: 2024
;; Version: 0.3
;; Package-Requires: (
;;         (emacs "29.4")
;;         (consult "2.0")
;;         (consult-omni "0.3"))
;;
;; Homepage: https://github.com/armindarvish/consult-omni
;; Keywords: convenience

;;; Commentary:
;; consult-omni-man provides commands for running “man” shell commands
;; similar to consult-man but using consult-omni.


;;; Code:

(require 'consult-omni)

;;; User Options (a.k.a. Custom Variables)

(defcustom consult-omni-man-args "man -k"
  "Command line arguments for man.

Similar to `consult-man-args' bur for consult-omni."
  :group 'consult-omni
  :type '(choice string (repeat (choice string sexp))))

(defun consult-omni--man-preview (cand)
  "Preview function for CAND from `consult-omni-man'."
  (when-let* ((Man-notify-method 'quiet)
         (path (get-text-property 0 :path cand)))
    (funcall (consult--buffer-preview) 'preview (man path))))

(defun consult-omni--man-callback (cand)
  "Callback for CAND from `consult-omni-man'."
  (when-let ((path (get-text-property 0 :path cand)))
    (man path)))

(defun consult-omni--man-transform (candidates &optional query)
  "Format CANDIDATES for QUERY from `consult-omni-man'."
  (let* ((frame-width-percent (floor (* (frame-width) 0.1)))
         (match-str (if (and (stringp query) (not (equal query ".*"))) (consult--split-escaped query) nil))
         (source (propertize "man" 'face 'consult-omni-source-type-face))
         (results))
    (save-match-data
      (dolist (cand candidates)
        (when (string-match "\\`\\(.*?\\([^ ]+\\) *(\\([^,)]+\\)[^)]*).*?\\) +- +\\(.*\\)\\'" cand)
          (let* ((names (match-string 1 cand))
                 (name (match-string 2 cand))
                 (section (match-string 3 cand))
                 (desc (match-string 4 cand))
                 (path (concat section " " name))
                 (snippet (and (stringp desc) (propertize desc 'face 'consult-omni-snippet-face)))
                 (face (or (consult-omni--get-source-prop source :face) 'consult-omni-default-face))
                 (title (propertize names 'face face))
                 (title-str (and (stringp title) (consult-omni--set-string-width title (* 6 frame-width-percent))))
                 (str (concat title-str
                              (and desc "\t") desc
                              (and snippet "\s\s") snippet
                              (and source "\t") source)))
            (if consult-omni-highlight-matches-in-minibuffer
                (cond
                 ((listp match-str)
                  (mapc (lambda (match) (setq str (consult-omni--highlight-match match str t))) match-str))
                 ((stringp match-str)
                  (setq str (consult-omni--highlight-match match-str str t)))))
            (push (propertize str :source "man" :query query :title title :path path :url nil :search-url nil :snippet desc) results)))))
    (nreverse results)))

(cl-defun consult-omni--man-builder (input &rest args &key callback &allow-other-keys)
  "Make builder command line args for “man” with INPUT and ARGS.

CALLBACK is a function used internally to update the list of candidates in
the minibuffer asynchronously.  It is called with a list of strings, which
are new annotated candidates \(e.g. as they arrive from an asynchronous
process\) to be added to the minibuffer completion cnadidates.  See the
section on REQUEST in documentation for `consult-omni-define-source' as
well as the function
`consult-omni--multi-update-dynamic-candidates' for how CALLBACK is used."
  (pcase-let* ((`(,query . _) (consult-omni--split-command input (seq-difference args (list :callback callback))))
               (consult-man-args consult-omni-man-args))
    (funcall #'consult--man-builder query)))

;; Define the man source
(consult-omni-define-source "man"
                            :narrow-char ?m
                            :category 'consult-man
                            :type 'async
                            :require-match t
                            :face 'consult-omni-engine-title-face
                            :request #'consult-omni--man-builder
                            :transform #'consult-omni--man-transform
                            :on-preview #'consult-omni--man-preview
                            :on-return #'identity
                            :on-callback #'consult-omni--man-callback
                            :preview-key consult-omni-preview-key
                            :search-hist 'consult-omni--search-history
                            :select-hist 'consult-omni--selection-history
                            :group #'consult-omni--group-function
                            :enabled (lambda () (executable-find "man"))
                            :sort t
                            :interactive consult-omni-intereactive-commands-type
                            :annotate nil)

;;; provide `consult-omni-man' module

(provide 'consult-omni-man)

(add-to-list 'consult-omni-sources-modules-to-load 'consult-omni-man)
;;; consult-omni-man.el ends here
