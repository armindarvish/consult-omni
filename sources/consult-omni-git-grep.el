;;; consult-omni-git-grep.el --- Consulting Git Grep Command -*- lexical-binding: t -*-

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

(defun consult-omni--git-grep-transform (candidates &optional query)
  "Formats candidates of `consult-omni-git-grep'."
  (consult-omni--grep-format candidates :source "git-grep" :query query :regexp-pattern consult--grep-match-regexp))

(cl-defun consult-omni--git-grep-builder (input &rest args &key callback &allow-other-keys)
  "Makes builder command line args for “git-grep”."
  (pcase-let* ((`(,query . ,opts) (consult-omni--split-command input (seq-difference args (list :callback callback))))
               (opts (car-safe opts))
               (count (plist-get opts :count))
               (dir (plist-get opts :dir))
               (dir (if dir (file-truename (format "%s" dir))))
               (count (or (and count (integerp (read count)) (string-to-number count))
                          consult-omni-default-count))
               (default-directory (or dir default-directory)))
    (funcall (consult-omni--grep-make-builder #'consult--git-grep-make-builder dir) query)))

;; Define the Ripgrep Source
(consult-omni-define-source "git-grep"
                            :narrow-char ?r
                            :type 'async
                            :require-match t
                            :face 'consult-omni-engine-title-face
                            :request #'consult-omni--git-grep-builder
                            :transform #'consult-omni--git-grep-transform
                            :on-preview #'consult-omni--grep-preview
                            :on-return #'identity
                            :on-callback #'consult-omni--grep-callback
                            :preview-key consult-omni-preview-key
                            :search-hist 'consult-omni--search-history
                            :select-hist 'consult-omni--selection-history
                            :group #'consult-omni--group-function
                            :enabled (lambda () (if (and (executable-find "git")
                                                         (fboundp 'consult-git-grep))
                                                    t
                                                  nil))
                            :sort t
                            :static 'both
                            :annotate nil)

;;; provide `consult-omni-git-grep' module

(provide 'consult-omni-git-grep)

(add-to-list 'consult-omni-sources-modules-to-load 'consult-omni-git-grep)
;;; consult-omni-git-grep.el ends here
