;;; consult-omni-git-grep.el --- Consulting Git Grep Command -*- lexical-binding: t -*-

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
;; consult-omni-git-grep provides commands for running “git grep” shell
;; commands similar to consult-git-grep but using consult-omni.


;;; Code:

(require 'consult-omni)
(require 'consult-omni-grep)

(defun consult-omni--git-grep-transform (candidates &optional query)
  "Format CANDIDATES of `consult-omni-git-grep' from QUERY."
  (consult-omni--grep-format candidates :source "git-grep" :query query :regexp-pattern consult--grep-match-regexp))

(cl-defun consult-omni--git-grep-builder (input &rest args &key callback &allow-other-keys)
  "Make builder command line args for “git-grep” from INPUT with ARGS.

CALLBACK is a function used internally to update the list of candidates in
the minibuffer asynchronously.  It is called with a list of strings, which
are new annotated candidates \(e.g. as they arrive from an asynchronous
process\) to be added to the minibuffer completion cnadidates.  See the
section on REQUEST in documentation for `consult-omni-define-source' as
well as the function
`consult-omni--multi-update-dynamic-candidates' for how CALLBACK is used."
  (pcase-let* ((`(,query . ,opts) (consult-omni--split-command input (seq-difference args (list :callback callback))))
               (opts (car-safe opts))
               (dir (plist-get opts :dir))
               (dir (if dir (file-truename (format "%s" dir))))
               (default-directory (or dir default-directory)))
    (funcall (consult-omni--grep-make-builder #'consult--git-grep-make-builder dir) query)))

;; Define the git-grep Source
(consult-omni-define-source "git-grep"
                            :narrow-char ?r
                            :type 'async
                            :require-match t
                            :face 'consult-omni-engine-title-face
                            :request #'consult-omni--git-grep-builder
                            :transform #'consult-omni--git-grep-transform
                            :on-preview #'consult-omni--grep-preview
                            :on-return #'identity
                            :on-callback #'consult-omni--grep-preview
                            :preview-key consult-omni-preview-key
                            :search-hist 'consult-omni--search-history
                            :select-hist 'consult-omni--selection-history
                            :group #'consult-omni--group-function
                            :enabled (lambda () (and (executable-find "git")
                                                    (fboundp 'consult-git-grep)))
                            :sort nil
                            :interactive consult-omni-intereactive-commands-type
                            :annotate nil)

;;; provide `consult-omni-git-grep' module

(provide 'consult-omni-git-grep)

(add-to-list 'consult-omni-sources-modules-to-load 'consult-omni-git-grep)
;;; consult-omni-git-grep.el ends here
