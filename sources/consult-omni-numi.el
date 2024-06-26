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
"Command line arguments for mdfind.

Similar to other command line args for consult but for mdfind.
See `consult-locate-args' for example."
  :type '(choice string (repeat (choice string sexp))))

(defun consult-omni--numi-preview (cand)
  "Mdfind preview function.")

(defun consult-omni--numi-callback (cand)
  "Muni callback function."
  (let ((result  (get-text-property 0 :title cand)))
  (kill-new result))
  )

(defun consult-omni--numi-filter (candidates &optional query)
  "Formats `consult-omni-mdfind' candidates.
"
  (cl-loop for candidate in candidates
           when (not (equal candidate "?"))
           collect candidate))

(cl-defun consult-omni--numi-builder (input &rest args &key callback &allow-other-keys)
  "makes builder command line args for “mdfind”.
"
  (pcase-let* ((`(,query . ,opts) (consult-omni--split-command input (seq-difference args (list :callback callback))))
               (opts (car-safe opts)))
    (funcall #'consult-omni--async-builder (shell-quote-argument query) consult-omni-numi-args)
   ;; (funcall #'consult--locate-builder (shell-quote-argument query))
   ;; (list consult-omni-numi-args query)
            ))

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
                           :enabled (lambda () (if (executable-find "numi-cli") t nil))
                           :sort t
                           :static 'both
                           :annotate nil
                           )

;;; provide `consult-omni-numi module

(provide 'consult-omni-numi)

(add-to-list 'consult-omni-sources-modules-to-load 'consult-omni-numi)
;;; consult-omni-numi.el ends here
