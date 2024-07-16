;;; consult-omni-google-autosuggest.el --- Consulting Google Autosuggest -*- lexical-binding: t -*-

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

(defvar consult-omni-google-autosuggest-api-url "http://suggestqueries.google.com/complete/search"
"API URL for Google AutoSuggest")

(defun consult-omni--google-autosuggest-new (cand)
  "Return CAND for NEW non-existing candidates."
  (when (listp cand) (setq cand (car-safe cand)))
  (or (and (stringp cand) (string-trim cand (consult--async-split-initial nil)))
      cand))

(cl-defun consult-omni--google-autosuggest-fetch-results (input &rest args &key callback &allow-other-keys)
  "Fetch search results for INPUT from Google Autosuggest.

Uses `consult-omni-google-autosuggest-api-url' as autosuggest api url."
  (pcase-let* ((`(,query . ,opts) (consult-omni--split-command input (seq-difference args (list :callback callback))))
               (opts (car-safe opts))
               (count (plist-get opts :count))
               (page (plist-get opts :page))
               (count (or (and count (integerp (read count)) (string-to-number count))
                          consult-omni-default-count))
               (page (or (and (integerp page) page)
                         (and page (string-to-number (format "%s" page)))
                         consult-omni-default-count))
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
                                                             (urlobj (and url (url-generic-parse-url url)))
                                                             (domain (and (url-p urlobj) (url-domain urlobj)))
                                                             (domain (and (stringp domain)
                                                                          (propertize domain 'face 'font-lock-variable-name-face)))
                                                             (path (and (url-p urlobj) (url-filename urlobj)))
                                                             (path (and (stringp path)
                                                                        (propertize path 'face 'font-lock-warning-face)))
                                                             (search-url nil)
                                                             (decorated (propertize word 'face 'consult-omni-default-face)))
                                                        (propertize decorated
                                                                    :source source
                                                                    :title word
                                                                    :url url
                                                                    :search-url search-url
                                                                    :query query)))
                                                    raw-results)))
                                 (funcall callback annotated-results)
                                 annotated-results)))))

;; Define the Google AutoSuggest Source
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
