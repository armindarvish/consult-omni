;;; consult-omni-notmuch.el --- Consulting Notmuch Command -*- lexical-binding: t -*-

;; Copyright (C) 2024 Armin Darvish

;; Author: Armin Darvish
;; Maintainer: Armin Darvish
;; Created: 2024
;; Version: 0.1
;; Package-Requires: ((emacs "28.1") (consult "1.1") (consult-omni "0.2") (consult-notmuch "0.8.1"))
;; Homepage: https://github.com/armindarvish/consult-omni
;; Keywords: convenience

;;; Commentary:

;;; Code:

(require 'consult-omni)
(require 'consult-notmuch)

(defun consult-omni-notmuch--transformer (str &optional query)
  "Transforms STR to notmuch display style."
  (let ((string (if consult-notmuch-show-single-message
      (consult-notmuch--show-transformer str)
    (consult-notmuch--search-transformer str))))
    (if (stringp string)
        (propertize string :source "notmuch" :query query))
    ))

(defun consult-omni--notmuch--preview (cand)
  "Preview function for notmuch candidates."
  (when-let ((id (consult-notmuch--candidate-id (cdr (get-text-property 0 'multi-category cand)))))
    (when (get-buffer consult-notmuch--buffer-name)
      (kill-buffer consult-notmuch--buffer-name))
    (consult-notmuch--show-id id consult-notmuch--buffer-name)))

(defun consult-omni--notmuch-callback (cand)
  "Callback function for notmuch candidates.
"
  (consult-notmuch--show (cdr (get-text-property 0 'multi-category cand))))

(cl-defun consult-omni--notmuch-command-builder (input &rest args &key callback &allow-other-keys)
  "makes builder command line args for “notmuch”.
"
  (setq consult-notmuch--partial-parse nil)
  (pcase-let* ((`(,query . ,opts) (consult-omni--split-command input (seq-difference args (list :callback callback))))
               (opts (car-safe opts)))
  (consult-notmuch--command query)
  ))

(consult-omni-define-source "notmuch"
                           :narrow-char ?m
                           :type 'async
                           :require-match nil
                           :category 'notmuch-result
                           :face 'consult-omni-engine-title-face
                           :request #'consult-omni--notmuch-command-builder
                           :on-preview #'consult-omni--notmuch--preview
                           :on-return #'identity
                           :on-callback #'consult-omni--notmuch-callback
                           :preview-key consult-omni-preview-key
                           :search-hist 'consult-omni--search-history
                           :select-hist 'consult-omni--selection-history
                           :group #'consult-omni--group-function
                           :sort t
                           :static 'both
                           :transform (lambda (items &optional query) (remove nil (mapcar (lambda (string)
                                                                (consult-omni-notmuch--transformer string query)) items)))
                           :enabled (lambda () (if (and (executable-find "notmuch")
                                                   (fboundp 'consult-notmuch))
                                                   t nil))
                           :annotate nil
                           )

;;; provide `consult-omni-notmuch' module

(provide 'consult-omni-notmuch)

(add-to-list 'consult-omni-sources-modules-to-load 'consult-omni-notmuch)
;;; consult-omni-notmuch.el ends here
