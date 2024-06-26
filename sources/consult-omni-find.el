;;; consult-omni-find.el --- Consulting Find Command -*- lexical-binding: t -*-

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

(defcustom consult-omni-find-show-hidden-files t
  "Whether to show hidden files in `consult-omni-find'."
  :type 'boolean)

(defcustom consult-omni-find-args  "find ."
  "Command line arguments for find.

Similar to `consult-find-args' bur for consult-omni."
  :type '(choice string (repeat (choice string sexp))))

(defun consult-omni--find-transform (candidates &optional query)
  "Formats candidates of `consult-omni-find'.
"
  (mapcar (lambda (candidate)
           (string-remove-prefix (file-truename default-directory) candidate))
          candidates))

(defun consult-omni--find-filter (candidates &optional query)
  "Filters for candidates of `consult-omni-find'.
"
  (seq-filter (lambda (candidate) (not (string-match "^find:.*$" candidate nil nil))) candidates))

(defun consult-omni--find-preview (cand)
  "Grep preview function."
(funcall (consult--file-preview) 'preview cand))

(defun consult-omni--find-callback (cand)
  "Find callback function."
  (consult--file-action cand)
  )

(cl-defun consult-omni--find-builder (input &rest args &key callback &allow-other-keys)
  "Makes builder command line args for “find”.
"
  (pcase-let* ((`(,query . ,opts) (consult-omni--split-command input (seq-difference args (list :callback callback))))
               (opts (car-safe opts))
               (count (plist-get opts :count))
               (hidden (if (plist-member opts :hidden) (plist-get opts :hidden) consult-omni-find-show-hidden-files))
               (ignore (plist-get opts :ignore))
               (ignore (if ignore (format "%s" ignore)))
               (dir (plist-get opts :dir))
               (dir (if dir (file-truename (format "%s" dir))))
               (count (or (and count (integerp (read count)) (string-to-number count))
                          consult-omni-default-count))
               (default-directory (or dir default-directory))
               (`(_ ,paths _) (consult--directory-prompt "" dir))
               (paths (if dir
                        (mapcar (lambda (path) (file-truename (concat dir path))) paths)
                      paths))
               (consult-find-args (concat consult-omni-find-args
                                          (if (not hidden) " -not -iwholename *./[a-z]*")
                                          (if ignore (concat " -not -iwholename *" ignore "*"))))
               )
   (funcall (consult--find-make-builder paths) query)
            ))

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
                           :static 'both
                           :enabled (lambda () (if (executable-find "find") t nil))
                           :annotate nil
                           )

;;; provide `consult-omni-find' module

(provide 'consult-omni-find)

(add-to-list 'consult-omni-sources-modules-to-load 'consult-omni-find)
;;; consult-omni-find.el ends here
