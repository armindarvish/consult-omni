;;; consult-omni-ripgrep.el --- Consulting Ripgrep Command -*- lexical-binding: t -*-

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
;; consult-omni-ripgrep provides commands for running “rg” shell commands
;; similar to consult-ripgrep but using consult-omni.

;;; Code:

(require 'consult-omni)
(require 'consult-omni-grep)

(defun consult-omni--ripgrep-transform (candidates &optional query)
  "Format CANDIDATES for QUERY from `consult-omni-ripgrep'."
  (consult-omni--grep-format candidates :source "ripgrep" :query query :regexp-pattern consult--grep-match-regexp))

(cl-defun consult-omni--ripgrep-builder (input &rest args &key callback &allow-other-keys)
  "Make builder command line args for “ripgrep” with INPUT and ARGS.

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
    (funcall (consult-omni--grep-make-builder #'consult--ripgrep-make-builder dir) query)))

;; Define the ripgrep source
(consult-omni-define-source "ripgrep"
                            :narrow-char ?r
                            :type 'async
                            :require-match t
                            :face 'consult-omni-engine-title-face
                            :request #'consult-omni--ripgrep-builder
                            :transform #'consult-omni--ripgrep-transform
                            :on-preview #'consult-omni--grep-preview
                            :on-return #'identity
                            :on-callback #'consult-omni--grep-preview
                            :preview-key consult-omni-preview-key
                            :search-hist 'consult-omni--search-history
                            :select-hist 'consult-omni--selection-history
                            :group #'consult-omni--group-function
                            :enabled (lambda () (and (executable-find "rg")
                                                     (fboundp 'consult-ripgrep)))
                            :sort nil
                            :interactive consult-omni-intereactive-commands-type
                            :annotate nil)

;;; provide `consult-omni-ripgrep' module

(provide 'consult-omni-ripgrep)

(add-to-list 'consult-omni-sources-modules-to-load 'consult-omni-ripgrep)
;;; consult-omni-ripgrep.el ends here
