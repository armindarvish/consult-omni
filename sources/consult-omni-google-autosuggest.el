;;; consult-omni-google-autosuggest.el --- Consulting Google Autosuggest -*- lexical-binding: t -*-

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
;; consult-omni-google-autosuggest provides commands for getting
;; autosuggestion from Google in Emacs using consult-omni.

;;; Code:

(require 'consult-omni)

(defvar consult-omni-google-autosuggest-api-url "http://suggestqueries.google.com/complete/search"
  "API URL for Google AutoSuggest.")

(defun consult-omni--google-autosuggest-new (cand)
  "Return CAND for NEW non-existing candidates."
  (when (listp cand) (setq cand (car-safe cand)))
  (or (and (stringp cand) (string-trim cand (consult-omni--get-split-style-character)))
      cand))

(cl-defun consult-omni--google-autosuggest-fetch-results (input &rest args &key callback &allow-other-keys)
  "Fetch search results from Google Autosuggest for INPUT with ARGS.

Uses `consult-omni-google-autosuggest-api-url' as autosuggest api url.

CALLBACK is a function used internally to update the list of candidates in
the minibuffer asynchronously.  It is called with a list of strings, which
are new annotated candidates \(e.g. as they arrive from an asynchronous
process\) to be added to the minibuffer completion cnadidates.  See the
section on REQUEST in documentation for `consult-omni-define-source' as
well as the function
`consult-omni--multi-update-dynamic-candidates' for how CALLBACK is used."
  (pcase-let* ((`(,query . _) (consult-omni--split-command input (seq-difference args (list :callback callback))))
               (params `(("q" . ,query)
                         ("client" . "chrome")))
               (headers '(("Accept" . "application/json"))))
    (consult-omni--fetch-url consult-omni-google-autosuggest-api-url consult-omni-http-retrieve-backend
                             :encoding 'utf-8
                             :params params
                             :headers headers
                             :parser #'consult-omni--json-parse-buffer
                             :callback
                             (lambda (attrs)
                               (when-let* ((raw-results (append (list (car-safe attrs)) (car-safe (cdr-safe attrs))))
                                           (annotated-results
                                            (mapcar (lambda (item)
                                                      (let* ((source "Google AutoSuggest")
                                                             (word item)
                                                             (url                                  (concat "https://www.google.com/search?q="  (replace-regexp-in-string " " "+" word)))
                                                             (decorated (propertize word 'face 'consult-omni-default-face)))
                                                        (propertize decorated
                                                                    :source source
                                                                    :title word
                                                                    :url url
                                                                    :search-url url
                                                                    :query query)))
                                                    raw-results)))
                                 (funcall callback annotated-results)
                                 annotated-results)))))

;; Define the Google AutoSuggest source
(consult-omni-define-source "Google AutoSuggest"
                            :narrow-char ?G
                            :type 'dynamic
                            :require-match nil
                            :face 'consult-omni-engine-title-face
                            :request #'consult-omni--google-autosuggest-fetch-results
                            :on-preview #'ignore
                            :on-return #'identity
                            :on-callback #'string-trim
                            :on-new #'consult-omni--google-autosuggest-new
                            :search-hist 'consult-omni--search-history
                            :select-hist t
                            :group #'consult-omni--group-function
                            :enabled (lambda () (bound-and-true-p consult-omni-google-autosuggest-api-url))
                            :sort t
                            :interactive consult-omni-intereactive-commands-type)

;;; provide `consult-omni-google-autosuggest' module

(provide 'consult-omni-google-autosuggest)

(add-to-list 'consult-omni-sources-modules-to-load 'consult-omni-google-autosuggest)
;;; consult-omni-google-autosuggest.el ends here
