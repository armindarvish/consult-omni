;;; consult-omni-stackoverflow.el --- Consulting StackOverflow -*- lexical-binding: t -*-

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

(defcustom consult-omni-stackexchange-api-key nil
  "Key for Stack Exchange API.

See URL `https://api.stackexchange.com/', and
URL `https://stackapps.com/' for more info."
  :group 'consult-omni
  :type '(choice (const :tag "API Key" string)
                 (function :tag "Custom Function")))

(defvar consult-omni-stackoverflow-search-url "https://stackoverflow.com/search"
  "Search URL for StackOverflow")

(defvar consult-omni-stackoverflow-api-url "https://api.stackexchange.com/2.3/search/advanced"
  "API URL for StackOverflow")

(defvar consult-omni-stackoverflow-answered-mark "+"
  "Mark for answered StackOverflow's questions")

(defvar consult-omni-stackoverflow-unanswered-mark "x"
  "Mark for unanswered StackOverflow's questions")

(cl-defun consult-omni--stackoverflow-format-candidate (&rest args &key source query url search-url title snippet date answered score face &allow-other-keys)
  "Returns a formatted string for “StackOverflow” searches.

Description of Arguments:

  SOURCE     the name to use (e.g. “StackOveflow”)
  QUERY      query input from the user
  URL        a string pointing to url of the candidate
  SEARCH-URL the web search url
  TITLE      the title of the result/paper (e.g. title of paper)
  SNIPPET    a string containing a snippet/description of candidate
  DATE       a date string of candidate
  ANSWERED   whether the question is answered on StackOveflow
  SCORE      score of the question on StackOverflow
  FACE       the face to apply to TITLE"
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
    (if consult-omni-highlight-matches
        (cond
         ((listp match-str)
          (mapcar (lambda (match) (setq str (consult-omni--highlight-match match str t))) match-str))
         ((stringp match-str)
          (setq str (consult-omni--highlight-match match-str str t)))))
    str))

(cl-defun consult-omni--stackoverflow-fetch-results (input &rest args &key callback &allow-other-keys)
  "Fetch search results for INPUT from StackOverflow.
See URL `https://api.stackexchange.com/' for more info."
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
                                                      (decorated (consult-omni--stackoverflow-format-candidate :source source :query query :url url :search-url search-url :title title :date date :answered answered :score score)))
                                                   (propertize decorated
                                                               :source source
                                                               :title title
                                                               :url url
                                                               :search-url search-url
                                                               :query query
                                                               :date date
                                                               :answered answered
                                                               :score score
                                                               )))
                                               raw-results)))
                                 (when (and annotated-results (functionp callback))
                                   (funcall callback annotated-results))
                                 annotated-results)))))

;; Define the StackOverflow Source
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
                            :static 'both
                            :annotate nil)

;;; provide `consult-omni-stackoverflow' module

(provide 'consult-omni-stackoverflow)

(add-to-list 'consult-omni-sources-modules-to-load 'consult-omni-stackoverflow)
;;; consult-omni-stackoverflow.el ends here
