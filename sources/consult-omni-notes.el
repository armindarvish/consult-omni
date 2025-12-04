;;; consult-omni-notes.el --- Consulting Note Files -*- lexical-binding: t -*-

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
;; consult-omni-notes enables searching note files in consult-omni.
;; It provides commands to search notes using grep-like process (e.g. grep, ;; ripgrep, ...) and getting the results directly in the minibuffer.
;;
;; This is simply a convenient wrapper for `consult-omni-grep' and some
;; utilities to define a specific set of files/folders as notes and run
;; grep on the those files/folders.

;;; Code:

(require 'consult-omni)
(require 'consult-omni-grep)
(require 'consult-omni-ripgrep-all)

(defcustom consult-omni-notes-files (append
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
                                       (mapcar #'expand-file-name consult-notes-org-headings-files)))
  "List of all note files for consult-omni-notes."
  :group 'consult-omni
  :type '(repeat (string :tag "list of files")))

(defcustom consult-omni-notes-backend-command (or (and (executable-find "rga") "rga")
                                                  (and (executable-find "rg") "rg")
                                                  (and (executable-find "grep") "grep"))
  "What command-line program to use for searching files?

Can be either:
  grep  uses grep as backend command
  rg    uses ripgrep as backend command
  rga   uses ripgrep-all as backend command"
  :group 'consult-omni
  :type 'boolean)

(defcustom consult-omni--notes-new-func #'consult-omni--notes-new-capture-org
  "Function to use to create new notes.

This is used when a new candidate is selected (e.g. by `vertico-exit-input'.)"
  :group 'consult-omni
  :type '(choice (function :tag "(Default) Use Org Capture" consult-omni--notes-new-capture-org)
                 (function :tag "Use Org Roam" consult-omni--notes-new-capture-org-roam)
                 (function :tag "Use Denote" consult-omni--notes-new-create-denote)
                 (function :tag "Custom Function")))

(defun consult-omni--notes-transform (candidates &optional query)
  "Format CANDIDATES for QUERY from `consult-omni-notes'."
  (cond
   ((and (equal consult-omni-notes-backend-command "rga") (executable-find consult-omni-notes-backend-command))
    (consult-omni--ripgrep-all-format candidates :source "Notes Search" :query query :regexp-pattern consult-omni-ripgrep-all-match-regexp))
   ((and (or (equal consult-omni-notes-backend-command "rg") (equal consult-omni-notes-backend-command "grep")) (executable-find consult-omni-notes-backend-command))
    (consult-omni--grep-format candidates :source "Notes Search" :query query :regexp-pattern consult--grep-match-regexp))
   (t nil)))

(defun consult-omni--notes-preview (cand)
  "Preview function for CAND from `consult-omni-ripgrep-all'."
  (if (equal consult-omni-notes-backend-command "rga")
      (consult-omni--ripgrep-all-preview cand)
    (consult-omni--grep-preview cand)))

(defun consult-omni--notes-callback (cand)
  "Callback function for CAND from `consult-omni-ripgrep-all'."
  (if (equal consult-omni-notes-backend-command "rga")
      (consult-omni--ripgrep-all-preview cand)
    (consult-omni--grep-preview cand)))

(defun consult-omni--notes-new-capture-org (&optional title)
  "Make a new org note with TITLE."
   (when (org-capture-string title)
    (consult-omni-propertize-by-plist title `(:title ,title :source "Notes Search" :url nil :search-url nil :query ,title :file ,(cadr (org-capture-get :target))) 0 1)))

(defun consult-omni--notes-new-capture-org-roam (&optional title)
  "Make new org-roam note with TITLE."
  (when (org-roam-node-find nil title)
    (consult-omni-propertize-by-plist title `(:title ,title :source "Notes Search" :url nil :search-url nil :query ,title :file ,(file-truename (buffer-file-name))) 0 1)))

(defun consult-omni--notes-new-create-denote (&optional title)
  "Make a new denote note with TITLE."
  (when (featurep 'denote)
    (require 'denote nil t)
    (if-let* ((_ (push title denote-title-history))
            (file (denote--command-with-features #'denote nil nil t nil)))
      (consult-omni-propertize-by-plist title `(:title ,title :source "Notes Search" :url nil :search-url nil :query ,title :file ,(file-truename file))))))

(defun consult-omni--notes-new (cand)
  "New function for new non-existing CAND from `consult-omni-notes'."
  (funcall consult-omni--notes-new-func cand))

(cl-defun consult-omni--notes-builder (input &rest args &key _callback &allow-other-keys)
  "Make builder command line with INPUT and ARGS for `consult-omni-notes'.

CALLBACK is a function used internally to update the list of candidates in
the minibuffer asynchronously.  It is called with a list of strings, which
are new annotated candidates \(e.g. as they arrive from an asynchronous
process\) to be added to the minibuffer completion cnadidates.  See the
section on REQUEST in documentation for `consult-omni-define-source' as
well as the function
`consult-omni--multi-update-dynamic-candidates' for how CALLBACK is used."
  (pcase-let* ((`(,query . ,opts) (consult-omni--split-command input args))
               (opts (car-safe opts))
               (dir (plist-get opts :dir))
               (dir (if dir (file-truename (format "%s" dir))))
               (dir (or dir consult-omni-notes-files))
               (backend-builder (cond
                                 ((and (or (equal consult-omni-notes-backend-command "rga") (equal consult-omni-notes-backend-command "rg")) (executable-find consult-omni-notes-backend-command))
                                  #'consult--ripgrep-make-builder)
                                 ((and (equal consult-omni-notes-backend-command "grep") (executable-find "grep")) #'consult--ripgrep-make-builder)
                                 (t nil))))
    (let ((consult-ripgrep-args (if (equal consult-omni-notes-backend-command "rga") consult-omni-ripgrep-all-args consult-ripgrep-args)))
      (when backend-builder
        (funcall (consult-omni--grep-make-builder backend-builder dir) query)))))

;; Define the Notes Search source
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
                            :preview-key (or consult-omni-preview-key 'any)
                            :search-hist 'consult-omni--search-history
                            :select-hist 'consult-omni--selection-history
                            :group #'consult-omni--group-function
                            :sort t
                            :interactive consult-omni-intereactive-commands-type
                            :annotate nil
                            :enabled (lambda () (bound-and-true-p consult-omni-notes-files)))

;;; provide `consult-omni-notes' module

(provide 'consult-omni-notes)

(add-to-list 'consult-omni-sources-modules-to-load 'consult-omni-notes)
;;; consult-omni-notes.el ends here
