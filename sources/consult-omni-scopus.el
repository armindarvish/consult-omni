;;; consult-omni-scopus.el --- Consulting Scopus -*- lexical-binding: t -*-

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

(defcustom consult-omni-scopus-api-key nil
  "Key for Scopus API.

See URL `https://dev.elsevier.com/documentation/SCOPUSSearchAPI.wadl' for more info"
  :group 'consult-omni
  :type '(choice (const :tag "Scopus API Key" string)
                 (function :tag "Custom Function")))

(defvar consult-omni-scopus-search-url "https://www.scopus.com/record/display.uri?"
"Search URL for Scopus.")

(defvar consult-omni-scopus-api-url "https://api.elsevier.com/content/search/scopus"
"API URL for Scopus.")

(cl-defun consult-omni--scopus-format-candidate (&rest args &key source query url search-url title authors date journal doi face &allow-other-keys)
  "Returns a formatted string for candidates of `consult-omni-scopus'.

Description of Arguments:

  SOURCE     the name to use (e.g. “Scopus”)
  QUERY      query input from the user
  URL        the url of  candidate
  SEARCH-URL the web search url
             (e.g. https://www.scopus.com/record/display.uri?&eid=%s)
  TITLE      the title of the result/paper (e.g. title of paper)
  AUTHORS    the authors of the result/paper
  DATE       the publish date of the result/paper
  JOURNAL    the journal that the result/paper is published in
  DOI        the doi of the result/paper
  FACE       the face to apply to TITLE"
  (let* ((frame-width-percent (floor (* (frame-width) 0.1)))
         (source (if (stringp source) (propertize source 'face 'consult-omni-source-type-face) nil))
         (date (if (stringp date) (propertize date 'face 'consult-omni-date-face) nil))
         (journal (if (stringp journal) (propertize journal 'face 'consult-omni-domain-face) nil))
         (authors (cond
                   ((and authors (listp authors))
                    (concat (first authors) ",..., " (car (last authors))))
                   ((stringp authors)
                    authors)
                   (t nil)))
         (authors (if (and authors (stringp authors)) (propertize authors 'face 'consult-omni-source-type-face)))
         (doi (if (stringp doi) (propertize doi 'face 'link) nil))
         (match-str (if (and (stringp query) (not (equal query ".*"))) (consult--split-escaped query) nil))
         (face (or (consult-omni--get-source-prop source :face) face 'consult-omni-default-face))
         (title-str (propertize title 'face face))
         (title-str (consult-omni--set-string-width title-str (* 5 frame-width-percent)))
         (str (concat title-str
                      (if journal (format "\t%s" journal))
                      (if date (format "\s\s%s" date))
                      (if authors (format "\s\s%s" authors))
                      (if source (concat "\t" source)))))
    (if consult-omni-highlight-matches-in-minibuffer
        (cond
         ((listp match-str)
          (mapcar (lambda (match) (setq str (consult-omni--highlight-match match str t))) match-str))
         ((stringp match-str)
          (setq str (consult-omni--highlight-match match-str str t)))))
    str))

(defun consult-omni--scopus-callback (cand)
  "Callback function for `consult-omni-scopus'."
  (let* ((doi (get-text-property 0 :doi cand))
         (url (if doi (consult-omni--doi-to-url doi)
                (get-text-property 0 :url cand))))
    (funcall consult-omni-default-browse-function url)))

(defun consult-omni--scopus-preview (cand)
  "Preview function for `consult-omni-scopus'."
  (let* ((doi (get-text-property 0 :doi cand))
         (url (if doi (consult-omni--doi-to-url doi)
                (get-text-property 0 :url cand))))
    (funcall consult-omni-default-preview-function url)))

(cl-defun consult-omni--scopus-fetch-results (input &rest args &key callback &allow-other-keys)
  "Retrieve search results from SCOPUS for INPUT."
  (pcase-let* ((`(,query . ,opts) (consult-omni--split-command input (seq-difference args (list :callback callback))))
               (opts (car-safe opts))
               (count (plist-get opts :count))
               (page (plist-get opts :page))
               (count (or (and count (integerp (read count)) (string-to-number count))
                          consult-omni-default-count))
               (page (or (and page (integerp (read page)) (string-to-number page))
                         consult-omni-default-page))
               (count (min (max count 1) 25))
               (page (* count page))
               (params `(("query" . ,(replace-regexp-in-string " " "+" query))
                         ("count" . ,(format "%s" count))
                         ("start" . ,(format "%s" page))
                         ("apiKey" . ,(consult-omni-expand-variable-function consult-omni-scopus-api-key))))
               (headers `(("Accept" . "application/json"))))
    (consult-omni--fetch-url consult-omni-scopus-api-url consult-omni-http-retrieve-backend
                             :encoding 'utf-8
                             :params params
                             :headers headers
                             :parser #'consult-omni--json-parse-buffer
                             :callback
                             (lambda (attrs)
                               (when-let* ((raw-results (map-nested-elt attrs '("search-results" "entry")))
                                           (annotated-results
                                            (mapcar
                                             (lambda (item)
                                               (let*
                                                   ((source "Scopus")
                                                    (title (gethash "dc:title" item))
                                                    (journal (gethash "prism:publicationName" item))
                                                    (volume (gethash "prism:volume" item))
                                                    (pages (gethash "prism:pageRange" item))
                                                    (authors (gethash "dc:creator" item))
                                                    (authors (cond
                                                              ((stringp authors) (list authors))
                                                              (t authors)))
                                                    (date (gethash "prism:coverDate" item))
                                                    (eid (gethash "eid" item))
                                                    (doi (gethash "prism:doi" item))
                                                    (url (concat consult-omni-scopus-search-url "&eid=" eid "&origin=inward"))
                                                    (search-url (concat consult-omni-scopus-search-url "&eid=" eid "&origin=inward"))
                                                    (decorated (consult-omni--scopus-format-candidate :source source :query query :url url :search-url search-url :title title :authors authors :date date :journal journal :doi doi)))
                                                 (propertize decorated
                                                             :source source
                                                             :url url
                                                             :title title
                                                             :search-url search-url
                                                             :query query
                                                             :journal journal
                                                             :volume volume
                                                             :pages pages
                                                             :authors authors
                                                             :date date
                                                             :doi doi
                                                             :eid eid)))
                                             raw-results)))
                                 (funcall callback annotated-results))))))

;; Define the Scopus Source
(consult-omni-define-source "Scopus"
                            :narrow-char ?s
                            :type 'dynamic
                            :require-match nil
                            :category 'consult-omni-scholar
                            :face 'consult-omni-scholar-title-face
                            :request #'consult-omni--scopus-fetch-results
                            :preview-key consult-omni-preview-key
                            :on-preview #'consult-omni--scopus-preview
                            :on-return #'identity
                            :on-callback #'consult-omni--scopus-callback
                            :search-hist 'consult-omni--search-history
                            :select-hist 'consult-omni--selection-history
                            :enabled (lambda () (bound-and-true-p consult-omni-scopus-api-key))
                            :group #'consult-omni--group-function
                            :sort t
                            :interactive consult-omni-intereactive-commands-type
                            :annotate nil)

;;; provide `consult-omni-scopus' module

(provide 'consult-omni-scopus)

(add-to-list 'consult-omni-sources-modules-to-load 'consult-omni-scopus)
;;; consult-omni-scopus.el ends here
