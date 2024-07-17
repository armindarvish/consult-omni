;;; consult-omni-numi.el --- Consulting numi Command -*- lexical-binding: t -*-

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

(defcustom consult-omni-numi-args "numi-cli"
  "Command line arguments for “numi-cli”.

Similar to other command line args for consult but for numi-cli.
See `consult-locate-args' for example."
  :type '(choice string (repeat (choice string sexp))))

(defun consult-omni--numi-preview (cand)
  "Preview function for `consult-omni-numi' command.")

(defun consult-omni--numi-callback (cand)
  "Callback function for `consult-omni-numi' command."
  (let ((result  (get-text-property 0 :title cand)))
    (kill-new result)))

(defun consult-omni--numi-filter (candidates &optional query)
  "Filters `consult-omni-numi' candidates."
  (cl-loop for candidate in candidates
           when (not (equal candidate "?"))
           collect candidate))

(cl-defun consult-omni--numi-builder (input &rest args &key callback &allow-other-keys)
  "Makes builder command line args for “numi-cli”."
  (pcase-let* ((`(,query . ,opts) (consult-omni--split-command input (seq-difference args (list :callback callback))))
               (opts (car-safe opts)))
    (funcall #'consult-omni--async-builder (shell-quote-argument query) consult-omni-numi-args)))

;; Define the Numi Source
(consult-omni-define-source "Numi"
                            :narrow-char ?N
                            :category 'consult-omni-calc
                            :type 'async
                            :require-match t
                            :face 'consult-omni-engine-title-face
                            :request #'consult-omni--numi-builder
                            :filter #'consult-omni--numi-filter
                            :on-preview #'ignore
                            :on-return #'identity
                            :on-callback #'consult-omni--numi-callback
                            :preview-key consult-omni-preview-key
                            :search-hist 'consult-omni--search-history
                            :select-hist 'consult-omni--selection-history
                            :group #'consult-omni--group-function
                            :enabled (lambda () (executable-find "numi-cli"))
                            :sort t
                            :interactive consult-omni-intereactive-commands-type
                            :annotate nil)

;;; provide `consult-omni-numi module

(provide 'consult-omni-numi)

(add-to-list 'consult-omni-sources-modules-to-load 'consult-omni-numi)
;;; consult-omni-numi.el ends here
