;;; consult-omni-google.el --- Consulting Google -*- lexical-binding: t -*-

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

(defcustom consult-omni-google-customsearch-key nil
  "Key for Google custom search API

See URL `https://developers.google.com/custom-search/' and
URL `https://developers.google.com/custom-search/v1/introduction' for details"
  :group 'consult-omni
  :type '(choice (const :tag "API Key" string)
                 (function :tag "Custom Function")))

(defcustom consult-omni-google-customsearch-cx nil
  "CX for Google custom search API

See URL `https://developers.google.com/custom-search/' and
URL `https://developers.google.com/custom-search/v1/introduction' for details"
  :group 'consult-omni
  :type '(choice (const :tag "CX String" string)
                 (function :tag "Custom Function")))

(defvar consult-omni-google-search-url "https://www.google.com/search"
"Search URL for Google")

(defvar consult-omni-google-customsearch-api-url "https://www.googleapis.com/customsearch/v1"
"API URL for Google Custom Search")

(cl-defun consult-omni--google-fetch-results (input &rest args &key callback &allow-other-keys)
  "Fetches search results for INPUT from “Google Custom Search” service.

Refer to URL `https://programmablesearchengine.google.com/about/' and
URL `https://developers.google.com/custom-search/' for more info."
  (pcase-let* ((`(,query . ,opts) (consult-omni--split-command input (seq-difference args (list :callback callback))))
               (opts (car-safe opts))
               (count (plist-get opts :count))
               (page (plist-get opts :page))
               (filter (plist-get opts :filter))
               (count (or (and count (integerp (read count)) (string-to-number count))
                          consult-omni-default-count))
               (page (or (and page (integerp (read page)) (string-to-number page))
                         consult-omni-default-page))
               (filter (or (and (integerp filter) filter)
                           (and filter (string-to-number (format "%s" filter)))
                           1))
               (filter (if (member filter '(0 1)) filter 1))
               (count (min count 10))
               (page (+ (* page count) 1))
               (page (min page (- 100 count)))
               (params `(("q" . ,(replace-regexp-in-string " " "+" query))
                         ("key" . ,(consult-omni-expand-variable-function consult-omni-google-customsearch-key))
                         ("cx" . ,(consult-omni-expand-variable-function consult-omni-google-customsearch-cx))
                         ("gl" . "en")
                         ("filter" . ,(format "%s" filter))
                         ("num" . ,(format "%s" count))
                         ("start" . ,(format "%s" page))))
               (headers '(("Accept" . "application/json")
                          ("Accept-Encoding" . "gzip")
                          ("User-Agent" . "consult-omni (gzip)"))))
    (consult-omni--fetch-url consult-omni-google-customsearch-api-url consult-omni-http-retrieve-backend
                             :encoding 'utf-8
                             :params params
                             :headers headers
                             :parser #'consult-omni--json-parse-buffer
                             :callback
                             (lambda (attrs)
                               (let* ((raw-results (gethash "items" attrs))
                                      (annotated-results
                                       (mapcar (lambda (item)
                                                 (let*
                                                     ((source "Google")
                                                      (url (format "%s" (gethash "link" item)))
                                                      (title (format "%s" (gethash "title" item)))
                                                      (snippet (string-trim (format "%s" (gethash "snippet" item))))
                                                      (search-url (consult-omni--make-url-string consult-omni-google-search-url params '("key" "cx" "gl")))
                                                      (decorated (funcall consult-omni-default-format-candidate :source source :query query :url url :search-url search-url :title title :snippet snippet)))
                                                   (propertize decorated
                                                               :source source
                                                               :title title
                                                               :url url
                                                               :search-url search-url
                                                               :query query
                                                               :snippet snippet)))
                                               raw-results)))
                                 (when (and annotated-results (functionp callback))
                                   (funcall callback annotated-results))
                                 annotated-results)))))

;; Define the Google Source
(consult-omni-define-source "Google"
                            :narrow-char ?g
                            :type 'dynamic
                            :require-match nil
                            :face 'consult-omni-engine-title-face
                            :request #'consult-omni--google-fetch-results
                            :on-new (apply-partially #'consult-omni-external-search-with-engine "Google")
                            :preview-key consult-omni-preview-key
                            :search-hist 'consult-omni--search-history
                            :select-hist 'consult-omni--selection-history
                            :enabled (lambda () (bound-and-true-p consult-omni-google-customsearch-key))
                            :group #'consult-omni--group-function
                            :sort t
                            :interactive consult-omni-intereactive-commands-type
                            :annotate nil)

;;; provide `consult-omni-google' module

(provide 'consult-omni-google)

(add-to-list 'consult-omni-sources-modules-to-load 'consult-omni-google)
;;; consult-omni-google.el ends here
