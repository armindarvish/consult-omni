;;; consult-omni-duckduckgo.el --- Consulting DuckDuckGo -*- lexical-binding: t -*-

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
;; consult-omni-duckduckgo provides commands for searching DuckDuckGo's
;; “limited” API in Emacs using consult-omni.

;;; Code:

(require 'consult-omni)

(defvar consult-omni-duckduckgo-api-url "http://api.duckduckgo.com/"
  "API URL for DuckDuckGo.")

(defvar consult-omni-duckduckgo-search-url "https://duckduckgo.com/"
  "Search URL for DuckDuckGo.")

(cl-defun consult-omni--duckduckgoapi-fetch-results (input &rest args &key callback &allow-other-keys)
  "Fetch search results for INPUT from DuckDuckGo limited API with ARGS.

See URL `https://duckduckgo.com/duckduckgo-help-pages/settings/params/'
for some limited documentation.

CALLBACK is a function used internally to update the list of candidates in
the minibuffer asynchronously.  It is called with a list of strings, which
are new annotated candidates \(e.g. as they arrive from an asynchronous
process\) to be added to the minibuffer completion cnadidates.  See the
section on REQUEST in documentation for `consult-omni-define-source' as
well as the function
`consult-omni--multi-update-dynamic-candidates' for how CALLBACK is used."
  (pcase-let* ((`(,query . _) (consult-omni--split-command input))
               (params `(("q" . ,(replace-regexp-in-string " " "+" query))
                         ("format" . "json")))
               (headers `(("Accept" . "application/json"))))
    (consult-omni--fetch-url consult-omni-duckduckgo-api-url consult-omni-http-retrieve-backend
                             :encoding 'utf-8
                             :params params
                             :headers headers
                             :parser #'consult-omni--json-parse-buffer
                             :callback
                             (lambda (attrs)
                               (let* ((raw-results (gethash "RelatedTopics" attrs))
                                      (annotated-results
                                       (remove nil (mapcar (lambda (item)
                                                             (let*
                                                                 ((source "DuckDuckGo API")
                                                                  (url (gethash "FirstURL" item))
                                                                  (title (gethash "Result" item))
                                                                  (title (if (and title (stringp title) (string-match "<a href=.*>\\(?1:.*\\)</a>.*" title)) (match-string 1 title) nil))
                                                                  (snippet (format "%s" (gethash "Text" item)))
                                                                  (search-url (consult-omni--make-url-string consult-omni-duckduckgo-search-url params '("format")))
                                                                  (decorated (if title (funcall consult-omni-default-format-candidate :source source :query query :url url :title title :snippet snippet) nil)))
                                                               (if decorated (propertize decorated
                                                                                         :source source
                                                                                         :title title
                                                                                         :url url
                                                                                         :search-url search-url
                                                                                         :query query))))
                                                           raw-results))))
                                 (when (and annotated-results (functionp callback))
                                   (funcall callback annotated-results))
                                 annotated-results)))))

;; Define the DuckDuckGo API source
(consult-omni-define-source "DuckDuckGo API"
                            :narrow-char ?d
                            :type 'dynamic
                            :require-match nil
                            :face 'consult-omni-engine-title-face
                            :request #'consult-omni--duckduckgoapi-fetch-results
                            :on-new (apply-partially #'consult-omni-external-search-with-engine "DuckDuckGo")
                            :preview-key consult-omni-preview-key
                            :search-hist 'consult-omni--search-history
                            :select-hist 'consult-omni--selection-history
                            :enabled (lambda () (bound-and-true-p consult-omni-duckduckgo-search-url))
                            :group #'consult-omni--group-function
                            :sort t
                            :interactive consult-omni-intereactive-commands-type
                            :annotate nil)

;;; provide `consult-omni-duckduckgo' module

(provide 'consult-omni-duckduckgo)

(add-to-list 'consult-omni-sources-modules-to-load 'consult-omni-duckduckgo)
;;; consult-omni-duckduckgo.el ends here
