;;; consult-omni-wikipedia.el --- Consulting Wikipedia -*- lexical-binding: t -*-

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
;; consult-omni-wikipedia provides commands for searching Wikipedia in
;; Emacs using consult-omni as the frontend.

;;; Code:

(require 'consult-omni)

(defvar consult-omni-wikipedia-search-url "https://www.wikipedia.org/search-redirect.php"
  "Search URL for Wikipedia.")

(defvar consult-omni-wikipedia-url "https://wikipedia.org/"
  "Main URL for Wikipedia.")

(defvar consult-omni-wikipedia-api-url "https://wikipedia.org/w/api.php"
  "API URL for Wikipedia.")

(cl-defun consult-omni--wikipedia-format-candidate (&rest args &key source query title snippet date face &allow-other-keys)
  "Format a candidate from Wikipedia search with ARGS.

Description of Arguments:

  SOURCE     a string; the source name (e.g. “Wikipedia”)
  QUERY      a string; query input from the user
  TITLE      a string; the title of the result (e.g. a Wikipedia article)
  SNIPPET    a string; a snippet/description of the Wikipedia article
  DATE       a string; the date that the article was last updated
  FACE       a string; the face to apply to TITLE"
  (let* ((frame-width-percent (floor (* (frame-width) 0.1)))
         (source (and (stringp source) (propertize source 'face 'consult-omni-source-type-face)))
         (date (and (stringp date) (propertize date 'face 'consult-omni-date-face)))
         (match-str (and (stringp query) (not (equal query ".*")) (consult--split-escaped query)))
         (face (or (consult-omni--get-source-prop source :face) face 'consult-omni-default-face))
         (title-str (propertize title 'face face))
         (title-str (consult-omni--set-string-width title-str (* 3 frame-width-percent)))
         (snippet (and (stringp snippet) (consult-omni--set-string-width (string-trim snippet) (* 4 frame-width-percent))))
         (snippet (and (stringp snippet) (propertize snippet 'face 'consult-omni-snippet-face)))
         (str (concat title-str
                      (when date (concat "\s" date))
                      (when snippet (concat "\s\s" snippet))
                      (when source (concat "\t" source)))))
    (if consult-omni-highlight-matches-in-minibuffer
        (cond
         ((listp match-str)
          (mapc (lambda (match) (setq str (consult-omni--highlight-match match str t))) match-str))
         ((stringp match-str)
          (setq str (consult-omni--highlight-match match-str str t)))))
    str))

(cl-defun consult-omni--wikipedia-fetch-results (input &rest args &key callback &allow-other-keys)
  "Fetch search results from Wikipedia for INPUT and ARGS.

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
               (count (max count 1))
               (params `(("action" . "query")
                         ("format" . "json")
                         ("list" . "search")
                         ("formatversion" . "2")
                         ("prop" . "info")
                         ("inprop" . "url")
                         ("srwhat" . "text")
                         ("srsearch" . ,(url-hexify-string query))
                         ("srlimit" . ,(format "%s" count))
                         ("sroffset" . ,(format "%s" page))))
               (headers '(("User-Agent" . "Emacs:consult-omni/0.1 (https://github.com/armindarvish/consult-omni);"))))
    (consult-omni--fetch-url consult-omni-wikipedia-api-url consult-omni-http-retrieve-backend
                             :encoding 'utf-8
                             :params params
                             :headers headers
                             :parser #'consult-omni--json-parse-buffer
                             :callback
                             (lambda (attrs)
                               (when-let* ((raw-results (map-nested-elt attrs '("query" "search")))
                                           (annotated-results
                                            (mapcar (lambda (item)
                                                      (let*
                                                          ((source "Wikipedia")
                                                           (title (format "%s" (gethash "title" item)))
                                                           (url (concat consult-omni-wikipedia-url "wiki/" (string-replace " " "_" title)))
                                                           (date (gethash "timestamp" item))
                                                           (date (format-time-string "%Y-%m-%d" (date-to-time date)))
                                                           (snippet (replace-regexp-in-string "<span.*?>\\|</span>\\|&quot;" "" (format "%s" (gethash "snippet" item))))
                                                           (search-url (concat  consult-omni-wikipedia-search-url "?" "search=" query))
                                                           (decorated (consult-omni--wikipedia-format-candidate :source source :query query :title title :snippet snippet :date date)))
                                                        (propertize decorated
                                                                    :source source
                                                                    :title title
                                                                    :url url
                                                                    :search-url search-url
                                                                    :query query
                                                                    :date date)))
                                                    raw-results)))
                                 (funcall callback annotated-results)
                                 annotated-results)))))

;; Define the Wikipedia source
(consult-omni-define-source "Wikipedia"
                            :narrow-char ?w
                            :type 'dynamic
                            :require-match nil
                            :face 'consult-omni-engine-title-face
                            :request #'consult-omni--wikipedia-fetch-results
                            :on-new (apply-partially #'consult-omni-external-search-with-engine "Wikipedia")
                            :preview-key consult-omni-preview-key
                            :search-hist 'consult-omni--search-history
                            :select-hist 'consult-omni--selection-history
                            :enabled (lambda () (bound-and-true-p consult-omni-wikipedia-api-url))
                            :group #'consult-omni--group-function
                            :sort t
                            :type 'dynamic
                            :interactive consult-omni-intereactive-commands-type)

;;; provide `consult-omni-wikipedia' module

(provide 'consult-omni-wikipedia)

(add-to-list 'consult-omni-sources-modules-to-load 'consult-omni-wikipedia)
;;; consult-omni-wikipedia.el ends here
