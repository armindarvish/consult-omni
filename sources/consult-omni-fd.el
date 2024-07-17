;;; consult-omni-fd.el --- Consulting Fd Command -*- lexical-binding: t -*-

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

(defcustom consult-omni-fd-show-hidden-files t
  "Whether to show hidden files in `consult-omni-fd'."
  :type 'boolean)

(defcustom consult-omni-fd-args (list (if (executable-find "fdfind" 'remote) "fdfind" "fd")
 "--full-path --color=never")
  "Command line arguments for fd.

Similar to `consult-fd-args' bur for consult-omni."
  :type '(choice string (repeat (choice string sexp))))

(defun consult-omni--fd-transform (candidates &optional query)
  "Formats candidates of `consult-omni-fd'."
  (mapcar (lambda (candidate)
            (string-trim (string-remove-prefix (file-truename default-directory) candidate)))
          candidates))

(defun consult-omni--fd-preview (cand)
  "Preview function for `consult-omni-find'."
  (funcall (consult--file-preview) 'preview cand))

(defun consult-omni--fd-callback (cand)
  "Callback for `consult-omni-find'."
  (consult--file-action cand))

(cl-defun consult-omni--fd-builder (input &rest args &key callback &allow-other-keys)
  "Makes builder command line args for “fd”."
  (pcase-let* ((`(,query . ,opts) (consult-omni--split-command input (seq-difference args (list :callback callback))))
               (opts (car-safe opts))
               (count (plist-get opts :count))
               (hidden (if (plist-member opts :hidden) (plist-get opts :hidden) consult-omni-fd-show-hidden-files))
               (case-sensitive (if (plist-member opts :case) (plist-get opts :case) nil))
               (exclude (or (plist-get opts :ignore) (plist-get opts :exclude)))
               (exclude (if exclude (format "%s" exclude)))
               (dir (plist-get opts :dir))
               (dir (if dir (file-truename (format "%s" dir))))
               (count (or (and count (integerp (read count)) (string-to-number count))
                          consult-omni-default-count))
               (default-directory (or dir default-directory))
               (`(_ ,paths _) (consult--directory-prompt "" dir))
               (paths (if dir
                          (mapcar (lambda (path) (file-truename (concat dir path))) paths)
                        paths))
               (consult-fd-args (append consult-omni-fd-args
                                          (and hidden (list "--hidden"))
                                          (and case-sensitive (list "--case-sensitive"))
                                          (and exclude (list (concat "--exclude " exclude))))))
    (funcall (consult--fd-make-builder paths) query)))

;; Define the Find Source
(consult-omni-define-source "fd"
                            :narrow-char ?f
                            :category 'file
                            :type 'async
                            :require-match t
                            :face 'consult-omni-engine-title-face
                            :request #'consult-omni--fd-builder
                            :transform #'consult-omni--fd-transform
                            :on-preview #'consult-omni--fd-preview
                            :on-return #'identity
                            :on-callback #'consult-omni--fd-callback
                            :preview-key consult-omni-preview-key
                            :search-hist 'consult-omni--search-history
                            :select-hist 'consult-omni--selection-history
                            :group #'consult-omni--group-function
                            :sort t
                            :interactive consult-omni-intereactive-commands-type
                            :enabled (lambda () (or (executable-find "fdfind")
                                               (executable-find "fd")))
                            :annotate nil)

;;; provide `consult-omni-fd' module

(provide 'consult-omni-fd)

(add-to-list 'consult-omni-sources-modules-to-load 'consult-omni-fd)
;;; consult-omni-fd.el ends here
