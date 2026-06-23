;;; consult-omni-stackoverflow.el --- Consulting StackOverflow -*- lexical-binding: t -*-

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
;; consult-omni-stackoverflow provides commands for searching StackOverflow
;; directly in Emacs using consult-omni as the frontend.

;;; Code:

(require 'consult-omni)

(defcustom consult-omni-stackexchange-api-key nil
  "Key for Stack Exchange API.

Can be a key string or a function that returns a key string.

See URL `https://api.stackexchange.com/', and
URL `https://stackapps.com/' for more info."
  :group 'consult-omni
  :type '(choice (const :tag "API Key" string)
                 (function :tag "Custom Function")))

(defvar consult-omni-stackoverflow-search-url "https://stackoverflow.com/search"
  "Search URL for StackOverflow.")

(defvar consult-omni-stackoverflow-api-url "https://api.stackexchange.com/2.3/search/advanced"
  "API URL for StackOverflow.")

(defvar consult-omni-stackoverflow-answered-mark "+"
  "Mark for answered StackOverflow's questions.")

(defvar consult-omni-stackoverflow-unanswered-mark "x"
  "Mark for unanswered StackOverflow's questions.")

(cl-defun consult-omni--stackoverflow-format-candidate (&rest args &key source query title date answered score face &allow-other-keys)
  "Format a candidate from “StackOverflow” search with ARGS.

Description of Arguments:

  SOURCE     a string; the source name \(e.g. “StackOveflow”\)
  QUERY      a string; query input from the user
  TITLE      a string; the title of the StackOverflow topic
  DATE       a string; the date string of the StackOverflow topic
  ANSWERED   a boolean; whether the question is answered on StackOveflow
  SCORE      a number; the score of the question on StackOverflow
  FACE       a symbol; the face to apply to TITLE"
  (let* ((frame-width-percent (floor (* (frame-width) 0.1)))
         (source (and (stringp source) (propertize source 'face 'consult-omni-source-type-face)))
         (date (and (stringp date) (propertize date 'face 'consult-omni-date-face)))
         (answered (if answered (propertize consult-omni-stackoverflow-answered-mark 'face 'consult-omni-domain-face)
                     (propertize consult-omni-stackoverflow-unanswered-mark 'face 'error)))
         (score (and score (propertize (format "%s" score) 'face 'consult-omni-path-face)))
         (match-str (and (stringp query) (not (equal query ".*")) (consult--split-escaped query)))
         (face (or (consult-omni--get-source-prop source :face) face 'consult-omni-default-face))
         (title-str (propertize title 'face face))
         (title-str (consult-omni--set-string-width title-str (* 7 frame-width-percent)))
         (str (concat title-str
                      (when date (concat "\s" date))
                      (when answered (concat "\s" answered))
                      (when score (concat "\s" score))
                      (when source (concat "\t" source)))))
    (if consult-omni-highlight-matches-in-minibuffer
        (cond
         ((listp match-str)
          (mapc (lambda (match) (setq str (consult-omni--highlight-match match str t))) match-str))
         ((stringp match-str)
          (setq str (consult-omni--highlight-match match-str str t)))))
    str))

(cl-defun consult-omni--stackoverflow-fetch-results (input &rest args &key callback &allow-other-keys)
  "Fetch search results for INPUT from StackOverflow with ARGS.

See URL `https://api.stackexchange.com/' for more info.

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
               (order (plist-get opts :order))
               (sort (plist-get opts :sort))
               (count (or (and count (integerp (read count)) (string-to-number count))
                          consult-omni-default-count))
               (page (or (and page (integerp (read page)) (string-to-number page))
                         consult-omni-default-page))
               (count (min count 25))
               (page (max page 1))
               (order (if (and order (member (format "%s" order) '("desc" "asc"))) (format "%s" order)))
               (sort (if (and sort (member (format "%s" sort) '("activity" "votes" "creation" "relevance"))) (format "%s" sort)))
               (params `(("order" . ,(or order "desc"))
                         ("sort" . ,(or sort "relevance"))
                         ("site" . "stackoverflow")
                         ("q" . ,(replace-regexp-in-string " " "+" query))
                         ("pagesize" . ,(format "%s" count))
                         ("page" . ,(format "%s" page))
                         ("key" . ,(consult-omni-expand-variable-function consult-omni-stackexchange-api-key))))
               (headers '(("Accept" . "application/json"))))
    (consult-omni--fetch-url consult-omni-stackoverflow-api-url consult-omni-http-retrieve-backend
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
                                                     ((source "StackOverflow")
                                                      (url (format "%s" (gethash "link" item)))
                                                      (title (format "%s" (gethash "title" item)))
                                                      (date (gethash "last_edit_date" item))
                                                      (date (format-time-string "%Y-%m-%d" (seconds-to-time date)))
                                                      (answered (gethash "is_answered" item))
                                                      (score (gethash "score" item))
                                                      (search-url (concat consult-omni-stackoverflow-search-url "?q=" input))
                                                      (decorated (consult-omni--stackoverflow-format-candidate :source source :query query :title title :date date :answered answered :score score)))
                                                   (propertize decorated
                                                               :source source
                                                               :title title
                                                               :url url
                                                               :search-url search-url
                                                               :query query
                                                               :date date
                                                               :answered answered
                                                               :score score)))
                                               raw-results)))
                                 (when (and annotated-results (functionp callback))
                                   (funcall callback annotated-results))
                                 annotated-results)))))

;; Define the StackOverflow source
(consult-omni-define-source "StackOverflow"
                            :narrow-char ?s
                            :type 'dynamic
                            :require-match nil
                            :face 'consult-omni-engine-title-face
                            :request #'consult-omni--stackoverflow-fetch-results
                            :preview-key consult-omni-preview-key
                            :search-hist 'consult-omni--search-history
                            :select-hist 'consult-omni--selection-history
                            :enabled (lambda () (bound-and-true-p consult-omni-stackexchange-api-key))
                            :group #'consult-omni--group-function
                            :sort t
                            :interactive consult-omni-intereactive-commands-type
                            :annotate nil)

;;; provide `consult-omni-stackoverflow' module

(provide 'consult-omni-stackoverflow)

(add-to-list 'consult-omni-sources-modules-to-load 'consult-omni-stackoverflow)
;;; consult-omni-stackoverflow.el ends here
