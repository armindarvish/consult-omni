;;; consult-omni-brave-autosuggest.el --- Consulting Brave Autosuggest -*- lexical-binding: t -*-

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
;; consult-omni-brave-autosuggest provides commands for getting
;; autosuggestion from Brave in Emacs using consult-omni.

;;; Code:

(require 'consult-omni)

;;; User Options (a.k.a. Custom Variables)
(defcustom consult-omni-brave-autosuggest-api-key nil
  "Key for Brave Autosuggest API.

Can be a key string or a function that returns a key string.

Refer to URL `https://brave.com/search/api/' for more info on getting an API key."
  :group 'consult-omni
  :type '(choice (string :tag "Brave Autosuggest API Key")
                 (function :tag "Custom Function")))

(defvar consult-omni-brave-autosuggest-api-url "https://api.search.brave.com/res/v1/suggest/search"
  "API URL for Brave AutoSuggest.")

(defun consult-omni--brave-autosuggest-return (cand)
  "Return the string of CAND with no properties."
  (when (stringp cand)
    (substring-no-properties (string-trim cand))))

(defun consult-omni--brave-autosuggest-new (cand)
  "Return CAND for NEW non-existing candidates."
  (when (listp cand) (setq cand (car-safe cand)))
  (or (and (stringp cand) (string-trim cand (consult-omni--get-split-style-character)))
      cand))

(cl-defun consult-omni--brave-autosuggest-fetch-results (input &rest args &key callback &allow-other-keys)
  "Fetch search results for INPUT from Brave Autosuggest API with ARGS.

CALLBACK is a function used internally to update the list of candidates in
the minibuffer asynchronously.  It is called with a list of strings, which
are new annotated candidates \(e.g. as they arrive from an asynchronous
process\) to be added to the minibuffer completion cnadidates.  See the
section on REQUEST in documentation for `consult-omni-define-source' as
well as the function
`consult-omni--multi-update-dynamic-candidates' for how CALLBACK is used."
  (pcase-let* ((`(,query . ,opts) (consult-omni--split-command input (seq-difference args (list :callback callback))))
               (opts (car-safe opts))
               (count (plist-get opts :count))
               (page (plist-get opts :page))
               (count (or (and count (integerp (read count)) (string-to-number count))
                          consult-omni-default-count))
               (page (or (and page (integerp (read page)) (string-to-number page))
                         consult-omni-default-page))
               (count (min (max count 1) 20))
               (params  `(("q" . ,(replace-regexp-in-string " " "+" query))
                          ("count" . ,(format "%s" count))
                          ("page" . ,(format "%s" page))
                          ("country" . "US")))
               (headers `(("User-Agent" . "Emacs:consult-omni/0.1 (Emacs consult-omni package; https://github.com/armindarvish/consult-omni)")
                          ("Accept" . "application/json")
                          ("Accept-Encoding" . "gzip")
                          ("X-Subscription-Token" . ,(consult-omni-expand-variable-function consult-omni-brave-autosuggest-api-key)))))
    (consult-omni--fetch-url consult-omni-brave-autosuggest-api-url consult-omni-http-retrieve-backend
                             :encoding 'utf-8
                             :params params
                             :headers headers
                             :parser #'consult-omni--json-parse-buffer
                             :callback
                             (lambda (attrs)
                               (when-let* ((original (make-hash-table :test 'equal))
                                           (_ (puthash "query" (gethash "original" (gethash "query" attrs)) original))
                                           (raw-results  (append (map-nested-elt attrs '("results")) (list original)))
                                           (annotated-results
                                            (mapcar (lambda (item)
                                                      (let* ((source "Brave AutoSuggest")
                                                             (word (gethash "query" item))
                                                             (url (concat "https://search.brave.com/search?q="  (replace-regexp-in-string " " "+" word)))
                                                             (search-url url)
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

;; Define the Brave AutoSuggest Source
(consult-omni-define-source "Brave AutoSuggest"
                            :narrow-char ?B
                            :type 'dynamic
                            :require-match nil
                            :face 'consult-omni-engine-title-face
                            :request #'consult-omni--brave-autosuggest-fetch-results
                            :group #'consult-omni--group-function
                            :on-preview #'ignore
                            :on-return #'consult-omni--brave-autosuggest-return
                            :on-callback #'string-trim
                            :on-new #'consult-omni--brave-autosuggest-new
                            :search-hist 'consult-omni--search-history
                            :select-hist t
                            :enabled (lambda () (bound-and-true-p consult-omni-brave-autosuggest-api-key))
                            :sort t
                            :interactive consult-omni-intereactive-commands-type)

;;; provide `consult-omni-brave-autosuggest' module

(provide 'consult-omni-brave-autosuggest)

(add-to-list 'consult-omni-sources-modules-to-load 'consult-omni-brave-autosuggest)
;;; consult-omni-brave-autosuggest.el ends here
