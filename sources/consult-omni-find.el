;;; consult-omni-find.el --- Consulting Find Command -*- lexical-binding: t -*-

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
;; consult-omni-find provides commands for running “find” shell commands
;; similar to consult-find but using consult-omni.

;;; Code:

(require 'consult-omni)

;;; User Options (a.k.a. Custom Variables)

(defcustom consult-omni-find-show-hidden-files t
  "Whether to show hidden files in `consult-omni-find'."
  :group 'consult-omni
  :type 'boolean)

(defcustom consult-omni-find-args  "find ."
  "Command line arguments for find.

Similar to `consult-find-args' bur for consult-omni."
  :group 'consult-omni
  :type '(choice string (repeat (choice string sexp))))

(defun consult-omni--find-transform (candidates &optional _query)
  "Format CANDIDATES of `consult-omni-find'."
  (mapcar (lambda (candidate)
            (string-trim (string-remove-prefix (file-truename default-directory) candidate)))
          candidates))

(defun consult-omni--find-filter (candidates &optional _query)
  "Filters for CANDIDATES of `consult-omni-find'."
  (seq-filter (lambda (candidate) (not (string-match "^find:.*$" candidate nil nil))) candidates))

(defun consult-omni--find-preview (cand)
  "Preview function for CAND from `consult-omni-find'."
  (funcall (consult--file-preview) 'preview cand))

(defun consult-omni--find-callback (cand)
  "Callback for CAND from `consult-omni-find'."
  (consult--file-action cand))

(cl-defun consult-omni--find-builder (input &rest args &key callback &allow-other-keys)
  "Make builder command line args for “find” from INPUT from ARGS.

CALLBACK is a function used internally to update the list of candidates in
the minibuffer asynchronously.  It is called with a list of strings, which
are new annotated candidates \(e.g. as they arrive from an asynchronous
process\) to be added to the minibuffer completion cnadidates.  See the
section on REQUEST in documentation for `consult-omni-define-source' as
well as the function
`consult-omni--multi-update-dynamic-candidates' for how CALLBACK is used."
  (pcase-let* ((`(,query . ,opts) (consult-omni--split-command input (seq-difference args (list :callback callback))))
               (opts (car-safe opts))
               (hidden (if (plist-member opts :hidden) (plist-get opts :hidden) consult-omni-find-show-hidden-files))
               (ignore (plist-get opts :ignore))
               (ignore (if ignore (format "%s" ignore)))
               (dir (plist-get opts :dir))
               (dir (if dir (file-truename (format "%s" dir))))
               (default-directory (or dir default-directory))
               (`(_ ,paths _) (consult--directory-prompt "" dir))
               (paths (if dir
                          (mapcar (lambda (path) (file-truename (concat dir path))) paths)
                        paths))
               (consult-find-args (concat consult-omni-find-args
                                          (if (not hidden) " -not -iwholename *./[a-z]*")
                                          (if ignore (concat " -not -iwholename *" ignore "*")))))
    (funcall (consult--find-make-builder paths) query)))

;; Define the find Source
(consult-omni-define-source "find"
                            :narrow-char ?f
                            :category 'file
                            :type 'async
                            :require-match t
                            :face 'consult-omni-engine-title-face
                            :request #'consult-omni--find-builder
                            :transform #'consult-omni--find-transform
                            :filter #'consult-omni--find-filter
                            :on-preview #'consult-omni--find-preview
                            :on-return #'identity
                            :on-callback #'consult-omni--find-callback
                            :preview-key consult-omni-preview-key
                            :search-hist 'consult-omni--search-history
                            :select-hist 'consult-omni--selection-history
                            :group #'consult-omni--group-function
                            :sort t
                            :interactive consult-omni-intereactive-commands-type
                            :enabled (lambda () (executable-find "find"))
                            :annotate nil)

;;; provide `consult-omni-find' module

(provide 'consult-omni-find)

(add-to-list 'consult-omni-sources-modules-to-load 'consult-omni-find)
;;; consult-omni-find.el ends here
