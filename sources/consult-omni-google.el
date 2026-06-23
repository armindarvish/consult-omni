;;; consult-omni-google.el --- Consulting Google -*- lexical-binding: t -*-

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

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; consult-omni-google provides commands for searching Goggle in Emacs
;; using consult-omni.

;;; Code:

(require 'consult-omni)

(defcustom consult-omni-google-customsearch-key nil
  "Key for Google custom search API.

Can be a key string or a function that returns a key string.

Refer to URL `https://developers.google.com/custom-search/' and
URL `https://developers.google.com/custom-search/v1/introduction' for
getting an API key."
  :group 'consult-omni
  :type '(choice (string :tag "API Key")
                 (function :tag "Custom Function")))

(defcustom consult-omni-google-customsearch-cx nil
  "CX for Google custom search API.

Can be a key string or a function that returns a key string.

See URL `https://developers.google.com/custom-search/' and
URL `https://developers.google.com/custom-search/v1/introduction' for getting a custom search CX number."
  :group 'consult-omni
  :type '(choice (string :tag "CX String")
                 (function :tag "Custom Function")))

(defvar consult-omni-google-search-url "https://www.google.com/search"
  "Search URL for Google.")

(defvar consult-omni-google-customsearch-api-url "https://www.googleapis.com/customsearch/v1"
  "API URL for “Google Custom Search”.")

(cl-defun consult-omni--google-fetch-results (input &rest args &key callback &allow-other-keys)
  "Fetch search results from “Google Custom Search” for INPUT with ARGS.

Refer to URL `https://programmablesearchengine.google.com/about/' and
URL `https://developers.google.com/custom-search/' for more info on
“Google Custom Search”.

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
                                                      (decorated (funcall consult-omni-default-format-candidate :source source :query query :url url :title title :snippet snippet)))
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

;; Define the Google source
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
