;;; consult-omni-wikipedia.el --- Consulting Wikipedia -*- lexical-binding: t -*-

;; Copyright (C) 2024 Armin Darvish

;; Author: Armin Darvish
;; Maintainer: Armin Darvish
;; Created: 2024
;; Version: 0.1
;; Package-Requires: ((emacs "28.1") (consult-omni "0.2") (consult "1.1"))
;; Homepage: https://github.com/armindarvish/consult-omni
;; Keywords: convenience

;;; Commentary:

;;; Code:

(require 'consult-omni)

(defvar consult-omni-wikipedia-search-url "https://www.wikipedia.org/search-redirect.php")
(defvar consult-omni-wikipedia-url "https://wikipedia.org/")
(defvar consult-omni-wikipedia-api-url "https://wikipedia.org/w/api.php")

(cl-defun consult-omni--wikipedia-format-candidate (&rest args &key source query url search-url title snippet date face &allow-other-keys)
  "Returns a formatted string for Wikipedia's searches.

SOURCE is the name string of the source for candidate

QUERY is the query string used for searching

URL is a string pointing to url of the candidate

SEARCH-URL is the web search url e.g.:
https://www.wikipedia.org/search-redirect.php?search=query

TITLE is the title of the candidate

SNIPPET is a string containing a snippet/description of candidate

DATE is the date the article was last updated

FACE is the face to apply to TITLE
"
  (let* ((frame-width-percent (floor (* (frame-width) 0.1)))
         (source (and (stringp source) (propertize source 'face 'consult-omni-source-type-face)))
         (date (and (stringp date) (propertize date 'face 'consult-omni-date-face)))
         (match-str (and (stringp query) (consult--split-escaped query) nil))
         (face (or (consult-omni--get-source-prop source :face) face 'consult-omni-default-face))
         (title-str (propertize title 'face face))
         (title-str (consult-omni--set-string-width title-str (* 3 frame-width-percent)))
         (snippet (and (stringp snippet) (consult-omni--set-string-width (string-trim snippet) (* 6 frame-width-percent))))
         (snippet (and (stringp snippet) (propertize snippet 'face 'consult-omni-snippet-face)))
         (str (concat title-str
                      (when date (concat "\s" date))
                      (when snippet (concat "\s\s" snippet))
                      (when source (concat "\t" source)))))
    (if consult-omni-highlight-matches
        (cond
         ((listp match-str)
          (mapcar (lambda (match) (setq str (consult-omni--highlight-match match str t))) match-str))
         ((stringp match-str)
          (setq str (consult-omni--highlight-match match-str str t)))))
    str))

(cl-defun consult-omni--wikipedia-fetch-results (input &rest args &key callback &allow-other-keys)
  "Fetches search results from Wikipedia for INPUT.
"

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
                                    (decorated (consult-omni--wikipedia-format-candidate :source source :query query :url url :search-url search-url :title title :snippet snippet :date date)))
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

(consult-omni-define-source "Wikipedia"
                           :narrow-char ?w
                           :type 'dynamic
                           :require-match t
                           :face 'consult-omni-engine-title-face
                           :request #'consult-omni--wikipedia-fetch-results
                           :preview-key consult-omni-preview-key
                           :search-hist 'consult-omni--search-history
                           :select-hist 'consult-omni--selection-history
                           :enabled (lambda () (boundp  'consult-omni-wikipedia-api-url))
                           :group #'consult-omni--group-function
                           :sort t
                           :type 'dynamic
                           :static 'both
                            )

;;; provide `consult-omni-wikipedia' module

(provide 'consult-omni-wikipedia)

(add-to-list 'consult-omni-sources-modules-to-load 'consult-omni-wikipedia)
;;; consult-omni-wikipedia.el ends here
