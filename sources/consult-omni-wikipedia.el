;;; consult-omni-wikipedia.el --- Consulting Wikipedia -*- lexical-binding: t -*-

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

(defvar consult-omni-wikipedia-search-url "https://www.wikipedia.org/search-redirect.php"
  "Search URL for Wikipedia.")

(defvar consult-omni-wikipedia-url "https://wikipedia.org/"
  "Main URL for Wikipedia")

(defvar consult-omni-wikipedia-api-url "https://wikipedia.org/w/api.php"
  "API URL for Wikipedia")

(cl-defun consult-omni--wikipedia-format-candidate (&rest args &key source query url search-url title snippet date face &allow-other-keys)
  "Returns a formatted string for Wikipedia's searches.

Description of Arguments:

  SOURCE     the name to use (e.g. “Wikipedia”)
  QUERY      query input from the user
             the search results of QUERY on the SOURCE website
  URL        the url of  candidate
  SEARCH-URL the web search url
             (e.g. https://www.wikipedia.org/search-redirect.php?search=query)
  TITLE      the title of the result/paper (e.g. title of paper)
  SNIPPET    a string containing a snippet/description of candidate
  DATE       the date the article was last updated
  FACE       the face to apply to TITLE"
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
          (mapcar (lambda (match) (setq str (consult-omni--highlight-match match str t))) match-str))
         ((stringp match-str)
          (setq str (consult-omni--highlight-match match-str str t)))))
    str))

(cl-defun consult-omni--wikipedia-fetch-results (input &rest args &key callback &allow-other-keys)
  "Fetches search results from Wikipedia for INPUT."
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

;; Define the Wikipedia Source
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
