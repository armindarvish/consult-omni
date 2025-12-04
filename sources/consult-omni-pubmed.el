;;; consult-omni-pubmed.el --- Consulting PubMed -*- lexical-binding: t -*-

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
;; consult-omni-pubmed provides commands for searching PubMed database
;; directly in Emacs using consult-omni as the frontend.

;;; Code:

(require 'consult-omni)

(defcustom consult-omni-pubmed-api-key nil
  "Key for Pubmed Entrez API.

See URL `https://www.ncbi.nlm.nih.gov/books/NBK25501/' for more info"
  :group 'consult-omni
  :type '(choice (const :tag "API Key" string)
                 (function :tag "Custom Function")))

(defvar consult-omni-pubmed-search-url "https://pubmed.ncbi.nlm.nih.gov/"
  "Search URL for PubMed.")

(defvar  consult-omni-pubmed-esearch-api-url "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi"
  "API URL for PubMed Eutils Entrez Esearch.")

(defvar consult-omni-pubmed-esummary-api-url "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi"
  "API URL for PubMed Eutils Entrez Esummary.")

(cl-defun consult-omni-dynamic--pubmed-format-candidate (&rest args &key source query title authors date journal doi face &allow-other-keys)
  "Format candidates from `consult-omni-pubmed' with ARGS.

Description of Arguments:

  SOURCE     the name to use (e.g. “PubMed”)
  QUERY      query input from the user
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
                    (concat (car authors) ",..., " (car (last authors))))
                   ((stringp authors)
                    authors)
                   (t nil)))
         (authors (if (and authors (stringp authors)) (propertize authors 'face 'consult-omni-source-type-face)))
         (doi (if (stringp doi) (propertize doi 'face 'link)))
         (match-str (if (and (stringp query) (not (equal query ".*"))) (consult--split-escaped query) nil))
         (face (or (consult-omni--get-source-prop source :face) face 'consult-omni-default-face))
         (title-str (propertize title 'face face))
         (title-str (consult-omni--set-string-width title-str (* 5 frame-width-percent)))
         (str (concat title-str
                      (if journal (format "\t%s" journal))
                      (if date (format "\s\s%s" date))
                      (if authors (format "\s\s%s" authors))
                      (if doi (format "\s\s%s" doi))
                      (if source (concat "\t" source)))))
    (if consult-omni-highlight-matches-in-minibuffer
        (cond
         ((listp match-str)
          (mapc (lambda (match) (setq str (consult-omni--highlight-match match str t))) match-str))
         ((stringp match-str)
          (setq str (consult-omni--highlight-match match-str str t)))))
    str))

(cl-defun consult-omni--pubmed-esearch-fetch-results (input &rest args &key db &allow-other-keys)
  "Fetch “esearch” results for INPUT and ARGS from PubMed Entrez Utilities.

DB is passed as db in query parameters \(This is the database to search\).

Refer to URL `https://www.ncbi.nlm.nih.gov/books/NBK25501/'
for more info."

  (pcase-let* ((`(,query . ,opts) (consult-omni--split-command input (seq-difference args (list :db db))))
               (opts (car-safe opts))
               (count (plist-get opts :count))
               (page (plist-get opts :page))
               (count (or (and count (integerp (read count)) (string-to-number count))
                          consult-omni-default-count))
               (page (or (and page (integerp (read page)) (string-to-number page))
                         consult-omni-default-page))
               (count (min count 20))
               (page (* page count))
               (db (if db (format "%s" db) "pubmed"))
               (params `(("db" . ,db)
                         ("term" . ,(replace-regexp-in-string " " "+" query))
                         ("usehistory" . "y")
                         ("retmax" . ,(format "%s" count))
                         ("retstart" . ,(format "%s" page))
                         ("retmode" . "json")))
               (headers `(("tool" . "consult-omni")
                          ("email" . "contact@armindarvish.com")
                          ("api_key" . ,(consult-omni-expand-variable-function consult-omni-pubmed-api-key)))))
    (consult-omni--fetch-url
     consult-omni-pubmed-esearch-api-url consult-omni-http-retrieve-backend
     :sync t
     :params params
     :headers headers
     :parser #'consult-omni--json-parse-buffer
     :callback
     (lambda (attrs)
       (let* ((results (gethash "esearchresult" attrs))
              (webenv (gethash "webenv" results))
              (qk (gethash "querykey" results))
              (idlist (gethash "idlist" results)))
         `(:webenv ,webenv :qk ,qk :idlist ,idlist))))))

(cl-defun consult-omni--pubmed-esummary-fetch-results (input &rest args &key callback webenv qk db &allow-other-keys)
  "Fetch “esummary” results for INPUT and ARGS from PubMed Entrez.

Description of Arguments:

  WEBENV a string; passed as webenv in query parameters
  qk     a string; passed as qk in query parameters
  DB     a string; passed as db in query parameters.

Refer to URL `https://www.ncbi.nlm.nih.gov/books/NBK25501/'
for more info.

CALLBACK is a function used internally to update the list of candidates in
the minibuffer asynchronously.  It is called with a list of strings, which
are new annotated candidates \(e.g. as they arrive from an asynchronous
process\) to be added to the minibuffer completion cnadidates.  See the
section on REQUEST in documentation for `consult-omni-define-source' as
well as the function
`consult-omni--multi-update-dynamic-candidates' for how CALLBACK is used."
  (pcase-let* ((`(,query . ,opts) (consult-omni--split-command input (seq-difference args (list :callback callback :webenv webenv :qk qk :db db))))
               (opts (car-safe opts))
               (count (plist-get opts :count))
               (page (plist-get opts :page))
               (count (or (and count (integerp (read count)) (string-to-number count))
                          consult-omni-default-count))
               (page (or (and page (integerp (read page)) (string-to-number page))
                         consult-omni-default-page))
               (page (* page count))
               (webenv (if webenv (format "%s" webenv)))
               (qk (if qk (format "%s" qk)))
               (retmax (min count 500))
               (retstart (max 0 page))
               (db (if db (format "%s" db) "pubmed"))
               (params `(("db" . ,db)
                         ("query_key" . ,qk)
                         ("WebEnv" . ,webenv)
                         ("retmax" . ,(format "%s" retmax))
                         ("retstart" . ,(format "%s" retstart))
                         ("retmode" . "json")))
               (headers `(("tool" . "consult-omni")
                          ("email" . "contact@armindarvish.com")
                          ("api_key" . ,(consult-omni-expand-variable-function consult-omni-pubmed-api-key)))))
    (consult-omni--fetch-url consult-omni-pubmed-esummary-api-url consult-omni-http-retrieve-backend
                             :encoding 'utf-8
                             :params params
                             :headers headers
                             :parser #'consult-omni--json-parse-buffer
                             :callback
                             (lambda (attrs)
                               (let* ((results (gethash "result" attrs))
                                      (uids (gethash "uids" results))
                                      (annotated-results
                                       (mapcar
                                        (lambda (uid)
                                          (let*
                                              ((source "PubMed")
                                               (url (url-unhex-string (concat consult-omni-pubmed-search-url (format "%s" uid))))
                                               (search-url (consult-omni--make-url-string consult-omni-pubmed-search-url `(("term" . ,(replace-regexp-in-string " " "+" query)))))
                                               (data (gethash uid results))
                                               (title (gethash "title" data))
                                               (pubdate (date-to-time (gethash "pubdate" data)))
                                               (date (format-time-string "%Y-%m-%d" pubdate))
                                               (journal (gethash "fulljournalname" data))
                                               (authors (mapcar (lambda (item) (gethash "name" item)) (gethash "authors" data)))
                                               (ids (gethash "articleids" data))
                                               (doi (car (remove nil (mapcar (lambda (item) (if (equal (gethash "idtype" item) "doi") (gethash "value" item))) ids))))
                                               (decorated (consult-omni-dynamic--pubmed-format-candidate :source source :query query search-url :title title :authors authors :date date :journal journal :doi doi)))
                                            (propertize decorated
                                                        :source source
                                                        :url url
                                                        :title title
                                                        :search-url search-url
                                                        :query query
                                                        :journal journal
                                                        :authors authors
                                                        :date date
                                                        :doi doi)))
                                        uids)))
                                 (when (and annotated-results (functionp callback))
                                   (funcall callback annotated-results))
                                 annotated-results)))))

(cl-defun consult-omni--pubmed-fetch-results (input &rest args &key callback &allow-other-keys)
  "Fetch results for INPUT and ARGS from PubMed Entrez Utilities.

CALLBACK is a function used internally to update the list of candidates in
the minibuffer asynchronously.  It is called with a list of strings, which
are new annotated candidates \(e.g. as they arrive from an asynchronous
process\) to be added to the minibuffer completion cnadidates.  See the
section on REQUEST in documentation for `consult-omni-define-source' as
well as the function
`consult-omni--multi-update-dynamic-candidates' for how CALLBACK is used."
  (let* ((esearch (consult-omni--pubmed-esearch-fetch-results input))
         (webenv (plist-get esearch :webenv))
         (qk (plist-get esearch :qk)))
    (consult-omni--pubmed-esummary-fetch-results input :callback callback :webenv webenv :qk qk)))

;; Define the PubMed source
(consult-omni-define-source "PubMed"
                            :narrow-char ?p
                            :type 'dynamic
                            :require-match nil
                            :category 'consult-omni-scholar
                            :face 'consult-omni-scholar-title-face
                            :request #'consult-omni--pubmed-fetch-results
                            :on-new (apply-partially #'consult-omni-external-search-with-engine "PubMed")
                            :preview-key consult-omni-preview-key
                            :search-hist 'consult-omni--search-history
                            :select-hist 'consult-omni--selection-history
                            :enabled (lambda () (bound-and-true-p consult-omni-pubmed-api-key))
                            :group #'consult-omni--group-function
                            :sort t
                            :interactive consult-omni-intereactive-commands-type
                            :annotate nil)

;;; provide `consult-omni-pubmed' module

(provide 'consult-omni-pubmed)

(add-to-list 'consult-omni-sources-modules-to-load 'consult-omni-pubmed)
;;; consult-omni-pubmed.el ends here
