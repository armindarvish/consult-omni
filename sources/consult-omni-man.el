;;; consult-omni-man.el --- Consulting Man Command -*- lexical-binding: t -*-

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

(defcustom consult-omni-man-args "man -k"
  "Command line arguments for man.

Similar to `consult-man-args' bur for consult-omni."
  :type '(choice string (repeat (choice string sexp))))

(defun consult-omni--man-preview (cand)
  "Preview for `consult-omni-man'."
  (when-let* ((Man-notify-method 'quiet)
             (path (get-text-property 0 :path cand)))
    (funcall (consult--buffer-preview) 'preview (man path))))

(defun consult-omni--man-callback (cand)
  "Callback for `consult-omni-man'."
  (when-let ((path (get-text-property 0 :path cand)))
    (man path)))

(defun consult-omni--man-transform (candidates &optional query)
  "Formats candidates of `consult-omni-man'."
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
                              (and source "\t") source)))
            (if consult-omni-highlight-matches
                (cond
                 ((listp match-str)
                  (mapcar (lambda (match) (setq str (consult-omni--highlight-match match str t))) match-str))
                 ((stringp match-str)
                  (setq str (consult-omni--highlight-match match-str str t)))))
            (push (propertize str :source "man" :query query :title title :path path :url nil :search-url nil :snippet desc) results)))))
    (nreverse results)))

(cl-defun consult-omni--man-builder (input &rest args &key callback &allow-other-keys)
  "Makes builder command line args for “man”."
  (pcase-let* ((`(,query . ,opts) (consult-omni--split-command input (seq-difference args (list :callback callback))))
               (consult-man-args consult-omni-man-args))
    (funcall #'consult--man-builder query)))

;; Define the Man Source
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
                            :enabled (lambda () (if (executable-find "man") t nil))
                            :sort t
                            :static 'both
                            :annotate nil)

;;; provide `consult-omni-man' module

(provide 'consult-omni-man)

(add-to-list 'consult-omni-sources-modules-to-load 'consult-omni-man)
;;; consult-omni-man.el ends here
