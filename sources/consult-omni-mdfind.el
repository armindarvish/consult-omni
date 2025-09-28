;;; consult-omni-mdfind.el --- Consulting mdfind Command -*- lexical-binding: t -*-

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
;; consult-omni-mdfind provides commands for running “mdfind” shell
;; commands

;;; Code:

(require 'consult-omni)

;;; User Options (a.k.a. Custom Variables)

(defcustom consult-omni-mdfind-interpret t
  "Whether to toggle “-interpret” arg in mdfind.
See mdfind documents (e.g. “man mdfind”) for more details."
  :group 'consult-omni
  :type 'boolean)

(defcustom consult-omni-mdfind-args "mdfind"
  "Command line arguments for mdfind.

Similar to other command line args for consult but for mdfind.
See `consult-locate-args' for an example."
  :group 'consult-omni
  :type '(choice string (repeat (choice string sexp))))

(defun consult-omni--mdfind-preview (cand)
  "Preview function for CAND from `consult-omni-mdfind'."
  (funcall (consult--file-preview) 'preview cand))

(defun consult-omni--mdfind-callback (cand)
  "Callback for CAND from `consult-omni-locate'."
  (consult--file-action cand))

(cl-defun consult-omni--mdfind-builder (input &rest args &key callback &allow-other-keys)
  "Make builder command line args for “mdfind” with INPUT and ARGS.

CALLBACK is a function used internally to update the list of candidates in
the minibuffer asynchronously.  It is called with a list of strings, which
are new annotated candidates \(e.g. as they arrive from an asynchronous
process\) to be added to the minibuffer completion cnadidates.  See the
section on REQUEST in documentation for `consult-omni-define-source' as
well as the function
`consult-omni--multi-update-dynamic-candidates' for how CALLBACK is used."
  (pcase-let* ((`(,query . ,opts) (consult-omni--split-command input (seq-difference args (list :callback callback))))
               (opts (car-safe opts))
               (count (plist-get opts :count))
               (dir (plist-get opts :dir))
               (dir (if dir (file-truename (format "%s" dir))))
               (count (or (and count (integerp (read count)) (string-to-number count))
                          consult-omni-default-count))
               (default-directory (or dir default-directory))
               (consult-locate-args (concat consult-omni-mdfind-args
                                            (if consult-omni-mdfind-interpret " -interpret")
                                            (if dir (format " -onlyin %s" dir)))))
    (funcall #'consult--locate-builder query)))

;; Define the mdfind source
(consult-omni-define-source "mdfind"
                            :narrow-char ?f
                            :category 'file
                            :type 'async
                            :require-match t
                            :face 'consult-omni-engine-title-face
                            :request #'consult-omni--mdfind-builder
                            :on-preview #'consult-omni--mdfind-preview
                            :on-return #'identity
                            :on-callback #'consult-omni--mdfind-callback
                            :preview-key consult-omni-preview-key
                            :search-hist 'consult-omni--search-history
                            :select-hist 'consult-omni--selection-history
                            :group #'consult-omni--group-function
                            :enabled (lambda () (executable-find "mdfind"))
                            :sort nil
                            :interactive consult-omni-intereactive-commands-type
                            :annotate nil)

;;; provide `consult-omni-mdfind' module

(provide 'consult-omni-mdfind)

(add-to-list 'consult-omni-sources-modules-to-load 'consult-omni-mdfind)
;;; consult-omni-mdfind.el ends here
