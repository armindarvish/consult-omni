;;; consult-omni-notes.el --- Consulting Note Files -*- lexical-binding: t -*-

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
(require 'consult-omni-ripgrep-all)

(defcustom consult-omni-notes-files (apply #'append
                                           (when (bound-and-true-p consult-notes-file-dir-sources)
                                             ;; dir sources
                                             (apply #'append (mapcar #'cddr consult-notes-file-dir-sources)))
                                           ;; org roam
                                           (when (bound-and-true-p org-roam-directory)
                                             (list (expand-file-name org-roam-directory)))
                                           ;; denote
                                           (when (bound-and-true-p denote-directory)
                                             (list (expand-file-name denote-directory)))
                                           ;; org agenda files
                                           (when (bound-and-true-p consult-notes-org-headings-mode)
                                             (list (mapcar #'expand-file-name consult-notes-org-headings-files))))
  "List of all note files for consult-omni-notes."
  :type '(repeat :tag "list of files" string))

(defcustom consult-omni-notes-backend-command "rga"
  "What command-line program to use for searching files?

Can be either:
  grep  uses grep as backend command
  rg    uses ripgrep as backend command
  rga   uses ripgrep-all as backend command"
  :type 'boolean)

(defcustom consult-omni--notes-new-func #'consult-omni--notes-new-capture-org
"Function to use to create new notes.

This is used when the a new candidate is selcted (e.g. by `vertico-exit-input'.)"
:type '(choice (function :tag "(Default) Use org-capture" consult-omni--notes-new-capture-org)
                 (function :tag "Custom Function")))

(defun consult-omni--notes-transform (candidates &optional query)
  "Formats `consult-omni-notes' candidates."

(cond
 ((and (equal consult-omni-notes-backend-command "rga") (executable-find consult-omni-notes-backend-command))
  (consult-omni--ripgrep-all-format candidates :source "Notes Search" :query query :regexp-pattern consult-omni-ripgrep-all-match-regexp)
  )
 ((and (or (equal consult-omni-notes-backend-command "rg") (equal consult-omni-notes-backend-command "grep")) (executable-find consult-omni-notes-backend-command))
  (consult-omni--grep-format candidates :source "Notes Search" :query query :regexp-pattern consult--grep-match-regexp))
 (t nil)))

(defun consult-omni--notes-preview (cand)
  "Preview function for `consult-omni-ripgrep-all'."
  (if (equal consult-omni-notes-backend-command "rga")
      (consult-omni--ripgrep-all-preview cand)
    (consult-omni--grep-preview cand)))

(defun consult-omni--notes-callback (cand)
  "Callback function for `consult-omni-ripgrep-all'."
  (if (equal consult-omni-notes-backend-command "rga")
      (consult-omni--ripgrep-all-callback cand)
    (consult-omni--grep-callback cand)))

(defun consult-omni--notes-new-capture-org (&optional string)
  "Makes new org note"
(let ((old-marker org-capture-last-stored-marker))
  (org-capture-string string)
  (consult-omni-propertize-by-plist string `(:title ,string :source "Notes Search" :url nil :search-url nil :query ,string :file ,(cadr (org-capture-get :target))) 0 1)))

(defun consult-omni--notes-new-capture-org-roam (&optional string)
 "Makes new org-roam note"
  (when (org-roam-node-find nil string)
  (consult-omni-propertize-by-plist string `(:title ,string :source "Notes Search" :url nil :search-url nil :query ,string :file ,(file-truename (buffer-file-name))) 0 1)))

(defun consult-omni--notes-new-create-denote (&optional string)
  "Makes new denote note"
  (if-let* ((_ (push string denote-title-history))
           (file (denote--command-with-features #'denote nil nil t nil)))
  (consult-omni-propertize-by-plist string `(:title ,string :source "Notes Search" :url nil :search-url nil :query ,string :file ,(file-truename file)))))

(defun consult-omni--notes-new (cand)
  "New function for `consult-omni-notes'."
  (funcall consult-omni--notes-new-func cand))

(cl-defun consult-omni--notes-builder (input &rest args &key callback &allow-other-keys)
  "Makes builder command line args for `consult-omni-notes'."
  (pcase-let* ((`(,query . ,opts) (consult-omni--split-command input args))
               (opts (car-safe opts))
               (count (plist-get opts :count))
               (dir (plist-get opts :dir))
               (dir (if dir (file-truename (format "%s" dir))))
               (dir (or dir consult-omni-notes-files))
               (count (or (and count (integerp (read count)) (string-to-number count))
                          consult-omni-default-count))
               (backend-builder (cond
                       ((and (or (equal consult-omni-notes-backend-command "rga") (equal consult-omni-notes-backend-command "rg")) (executable-find consult-omni-notes-backend-command))
                                #'consult--ripgrep-make-builder)
                       ((and (equal consult-omni-notes-backend-command "grep") (executable-find "grep")) #'consult--ripgrep-make-builder)
                       (t nil))))
  (when backend-builder
    (funcall (consult-omni--grep-make-builder backend-builder dir) query))))

;; Define the Notes Search Source
(consult-omni-define-source "Notes Search"
                            :narrow-char ?n
                            :type 'async
                            :require-match nil
                            :face 'consult-omni-engine-title-face
                            :request #'consult-omni--notes-builder
                            :transform #'consult-omni--notes-transform
                            :on-preview #'consult-omni--notes-preview
                            :on-return #'identity
                            :on-callback #'consult-omni--notes-callback
                            :on-new #'consult-omni--notes-new
                            :preview-key (or consult-omni-preview-key any)
                            :search-hist 'consult-omni--search-history
                            :select-hist 'consult-omni--selection-history
                            :group #'consult-omni--group-function
                            :sort t
                            :static 'both
                            :annotate nil)

;;; provide `consult-omni-notes' module

(provide 'consult-omni-notes)

(add-to-list 'consult-omni-sources-modules-to-load 'consult-omni-notes)
;;; consult-omni-notes.el ends here
