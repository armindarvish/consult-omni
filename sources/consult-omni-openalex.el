;;; consult-omni-openalex.el --- Consulting OpenAlex -*- lexical-binding: t -*-

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
;; consult-omni-openalex provides commands for searching OpenAlex database
;; directly in Emacs using consult-omni as the frontend.
;;
;; See URL `https://docs.openalex.org/' for more info on using OpenAlex.

;;; Code:

(require 'consult-omni)

(defcustom consult-omni-openalex-default-entity "works"
  "Default entity to search with `consult-omni-openalex'.

For more info on entities see:
URL `https://docs.openalex.org/api-entities/entities-overview'

Currently, only “works”, “authors”, “sources” and “institutions” are
supported in consult-omni."
  :group 'consult-omni
  :type '(choice (const :tag "search publications" "works")
                 (const :tag "search authors" "authors")
                 (const :tag "search journals and publishers" "sources")
                 (const :tag "search institutions" "institutions")))

(defcustom consult-omni-openalex-extra-params (list)
  "Additional arguments for `consult-omni-openalex'.

These are appended to the input for `consult-omni-openalex'.
Can be either a string, or a list of strings."
  :group 'consult-omni
  :type '(alist :key-type (const :tag "parameter name" :value-type (string :tag "the value passed to parameter"))))


(defcustom consult-omni-openalex-select-entity-first t
  "Whether to select entity before runing the search for `consult-omni-openalex'."
  :group 'consult-omni
  :type 'boolean)

(defvar consult-omni-openalex-search-url "https://openalex.org/"
  "Search URL for OpenAlex.")

(defvar consult-omni-openalex-api-url "https://api.openalex.org/"
  "API URL for OpenAlex.")

(defvar consult-omni-openalex-category 'consult-omni-openalex
  "Category symbol for openalex search.")

(defvar url-http-end-of-headers)

(defun consult-omni--openalex-parse-buffer ()
  "Json parser used in `consult-omni-openalex'."
  (let ((end-of-headers (if (and (bound-and-true-p url-http-end-of-headers)
                                 (number-or-marker-p url-http-end-of-headers))
                            url-http-end-of-headers
                          (point-min))))
    (goto-char end-of-headers)
    (json-parse-buffer :object-type 'hash-table :array-type 'list :false-object :false :null-object nil)))

(cl-defun consult-omni--openalex-format-works-candidate (&rest args &key source query pdf-url title authors date journal face &allow-other-keys)
  "Format a candidate from `consult-omni-openalex' with ARGS.

Description of Arguments:

  SOURCE     a string; the name to use (e.g. “OpenAlex”)
  QUERY      a string; query input from the user
  PDF-URL    a string; the PDF web url
  TITLE      a string; the title of the result/paper
  AUTHORS    a string or list of strings; the authors of the result/paper
  DATE       a string; the publish date of the result/paper
  JOURNAL    a string; the journal that the result/paper is published in
  FACE       a symbol; the face to apply to TITLE"
  (let* ((frame-width-percent (floor (* (frame-width) 0.1)))
         (source (if (stringp source) (propertize source 'face 'consult-omni-source-type-face) nil))
         (date (if (stringp date) (propertize date 'face 'consult-omni-date-face) nil))
         (journal (if (stringp journal) (propertize journal 'face 'consult-omni-domain-face) nil))
         (authors (cond
                   ((and authors (listp authors))
                    (if  (length> authors 1)
                        (concat (car authors) " et. al.")
                      (car authors)))
                   ((stringp authors)
                    authors)
                   (t nil)))
         (authors (if (and authors (stringp authors)) (propertize authors 'face 'consult-omni-source-type-face)))
         (match-str (if (and (stringp query) (not (equal query ".*"))) (consult--split-escaped query) nil))
         (face (or (consult-omni--get-source-prop source :face) face 'consult-omni-default-face))
         (pdf-url (if (stringp pdf-url) (propertize "[PDF]" 'face 'consult-omni-keyword-face)))
         (title-str (propertize (or title "Untitled") 'face face))
         (title-str (if (stringp title-str) (consult-omni--set-string-width title-str (* 5 frame-width-percent))))
         (str (concat title-str
                      (if authors (format "\t%s" authors))
                      (if journal (format "\s\s%s" journal))
                      (if date (format "\s%s" date))
                      (if pdf-url (format "\s\s%s" pdf-url))
                      (if source (concat "\t" source)))))
    (if consult-omni-highlight-matches-in-minibuffer
        (cond
         ((listp match-str)
          (mapc (lambda (match) (setq str (consult-omni--highlight-match match str t))) match-str))
         ((stringp match-str)
          (setq str (consult-omni--highlight-match match-str str t)))))
    str))

(cl-defun consult-omni--openalex-format-authors-candidate (&rest args &key source query title affiliations h-index i10-index works-count cite-count face &allow-other-keys)
  "Format an “author” candidate from `consult-omni-openalex' with ARGS.

Description of Arguments:

  SOURCE       a string; the name to use (e.g. “OpenAlex”)
  QUERY        a string; query input from the user
  TITLE        a string; the name of the author
  AFFILIATIONS a string or list of strings; the affiliations of the author
  OPENALEXID   a string; OpenAlex id
  H-INDEX      a number; h-index of the author
  I10-INDEX    a number; i10-index of the author
  WORKS-COUNT  a number; number of works from the author
  CITE-COUNT   a number; number of times author is cited
  FACE       a symbol; the face to apply to TITLE"
  (let* ((frame-width-percent (floor (* (frame-width) 0.1)))
         (source (if (stringp source) (propertize source 'face 'consult-omni-source-type-face) nil))
         (affiliations (cond
                        ((and affiliations (listp affiliations))
                         (if  (length> affiliations 1)
                             (mapconcat #'identity affiliations ", ")
                           (car affiliations)))
                        ((stringp affiliations)
                         affiliations)
                        (t nil)))
         (affiliations (if (and affiliations (stringp affiliations)) (propertize affiliations 'face 'consult-omni-default-face)))
         (affiliations (if (stringp affiliations) (consult-omni--set-string-width affiliations (* 2 frame-width-percent))))
         (works-count (if works-count (format "%s%s" (propertize "#works: " 'face 'consult-omni-comment-face) (propertize (format "%s" works-count) 'face 'consult-omni-default-face))))
         (cite-count (if cite-count (format "%s%s" (propertize "#cited: " 'face 'consult-omni-comment-face) (propertize (format "%s" cite-count) 'face 'consult-omni-default-face))))
         (h-index (if h-index (format "%s%s" (propertize "#h: " 'face 'consult-omni-comment-face) (propertize (format "%s" h-index) 'face 'consult-omni-domain-face))))
         (i10-index (if i10-index (format "%s%s" (propertize "#i10: " 'face 'consult-omni-comment-face) (propertize (format "%s" i10-index) 'face 'consult-omni-path-face))))
         (match-str (if (and (stringp query) (not (equal query ".*"))) (consult--split-escaped query) nil))
         (face (or (consult-omni--get-source-prop source :face) face 'consult-omni-default-face))
         (title-str (propertize (or title "Untitled") 'face face))
         (title-str (if (stringp title-str) (consult-omni--set-string-width title-str (* 2 frame-width-percent))))
         (str (concat title-str
                      "\t"
                      (if affiliations (format "%s\s\s" affiliations))
                      (if works-count (format "%s\s" works-count))
                      (if cite-count (format "%s\s" cite-count))
                      (if h-index (format "%s\s" h-index))
                      (if i10-index (format "%s\s" i10-index))
                      (if source (concat "\t" source)))))
    (if consult-omni-highlight-matches-in-minibuffer
        (cond
         ((listp match-str)
          (mapc (lambda (match) (setq str (consult-omni--highlight-match match str t))) match-str))
         ((stringp match-str)
          (setq str (consult-omni--highlight-match match-str str t)))))
    str))

(cl-defun consult-omni--openalex-format-sources-candidate (&rest args &key source query title type concepts impact cost face &allow-other-keys)
  "Format a “sources” candidate from `consult-omni-openalex' with ARGS.

Description of Arguments:

  SOURCE     a string; the name to use (e.g. “OpenAlex”)
  QUERY      a string; query input from the user
  TITLE      a string; the name of the author
  TYPE       a string; the type of source
  CONCEPTS   a string or list of strings; the topic concepts of the source
  IMPACT     a number; impact factor of the source
  COST       a number; processing cost in usd
  FACE       a symbol; the face to apply to TITLE"
  (let* ((frame-width-percent (floor (* (frame-width) 0.1)))
         (source (if (stringp source) (propertize source 'face 'consult-omni-source-type-face) nil))
         (concepts (cond
                        ((and concepts (listp concepts))
                         (if  (length> concepts 1)
                             (mapconcat #'identity concepts ", ")
                           (car concepts)))
                        ((stringp concepts)
                         concepts)
                        (t nil)))
         (concepts (if (and concepts (stringp concepts)) (propertize concepts 'face 'consult-omni-default-face)))
         (concepts (if (stringp concepts) (consult-omni--set-string-width concepts (* 4 frame-width-percent))))
         (type (if (stringp type) (propertize type 'face 'consult-omni-domain-face)))
         (impact (if impact (propertize (consult-omni--set-string-width (format "%.1f" impact) 5) 'face 'consult-omni-date-face)))
         (cost (if cost (propertize (format "$%s" cost) 'face 'consult-omni-comments-face)))
         (match-str (if (and (stringp query) (not (equal query ".*"))) (consult--split-escaped query) nil))
         (face (or (consult-omni--get-source-prop source :face) face 'consult-omni-default-face))
         (title-str (propertize (or title "Untitled") 'face face))
         (title-str (if (stringp title-str) (consult-omni--set-string-width title-str (* 3 frame-width-percent))))
         (str (concat title-str
                      "\t"
                      (if type (format "%s\s\s" type))
                      (if impact (format "%s\s" impact))
                      (if concepts (format "%s\s\s" concepts))
                      (if cost (format "%s\s" cost) (format "\s\s\s\s\s"))
                      (if source (concat "\t" source)))))
    (if consult-omni-highlight-matches-in-minibuffer
        (cond
         ((listp match-str)
          (mapc (lambda (match) (setq str (consult-omni--highlight-match match str t))) match-str))
         ((stringp match-str)
          (setq str (consult-omni--highlight-match match-str str t)))))
    str))

(cl-defun consult-omni--openalex-format-institutions-candidate (&rest args &key source query title acronym type impact works-count cite-count concepts face &allow-other-keys)
  "Format a “sources” candidate from `consult-omni-openalex' with ARGS.

Description of Arguments:

  SOURCE       a string; the name to use (e.g. “OpenAlex”)
  QUERY        a string; query input from the user
  TITLE        a string; the name of the author
  ACRONYM      a string; the acronym of the institution
  TYPE         a string; the type of source
  IMPACT       a number; impact factor of the source
  WORKS-COUNT  a number; number of works from the institution
  CITE-COUNT   a number; number of times the intitution is cited
  CONCEPTS     a string or list of strings; the concepts relevant
               to the works of the institution
  FACE         a symbol; the face to apply to TITLE"
  (let* ((frame-width-percent (floor (* (frame-width) 0.1)))
         (source (if (stringp source) (propertize source 'face 'consult-omni-source-type-face) nil))
         (concepts (cond
                    ((and concepts (listp concepts))
                     (if  (length> concepts 1)
                         (mapconcat #'identity concepts ", ")
                       (car concepts)))
                    ((stringp concepts)
                     concepts)
                    (t nil)))
         (concepts (if (and concepts (stringp concepts)) (propertize concepts 'face 'consult-omni-default-face)))
         (concepts (if (stringp concepts) (consult-omni--set-string-width concepts (* 2 frame-width-percent))))
         (works-count (if works-count (consult-omni--numbers-human-readable works-count "")))
         (works-count (if works-count (consult-omni--set-string-width (format "%s%s" (propertize "#works: " 'face 'consult-omni-comment-face) (format "%s" works-count)) 13)))
         (cite-count (if cite-count (consult-omni--numbers-human-readable cite-count "")))
         (cite-count (if cite-count (consult-omni--set-string-width (format "%s%s" (propertize "#cited: " 'face 'consult-omni-comment-face) (format "%s" cite-count)) 13)))
         (type (if (stringp type) (propertize type 'face 'consult-omni-path-face)))
         (impact (if impact (propertize (consult-omni--set-string-width (format "%.1f" impact) 5) 'face 'consult-omni-date-face)))
         (match-str (if (and (stringp query) (not (equal query ".*"))) (consult--split-escaped query) nil))
         (face (or (consult-omni--get-source-prop source :face) face 'consult-omni-default-face))
         (title-str (propertize (or title "Untitled") 'face face))
         (title-str (if (stringp title-str) (consult-omni--set-string-width title-str (* 3 frame-width-percent))))
         (acronym (if acronym (consult-omni--set-string-width (propertize acronym 'face 'consult-omni-keyword-face) 6) "\s\s\s\s\s\s"))
         (str (concat title-str
                      "\t"
                      (if acronym (format "%s\s" acronym))
                      (if impact (format "%s\s" impact))
                      (if concepts (format "%s\s\s" concepts))
                      (if works-count (format "%s\s" works-count))
                      (if cite-count (format "%s\s" cite-count))
                      (if type (format "%s\s\s" type))
                      (if source (concat "\t" source)))))
    (if consult-omni-highlight-matches-in-minibuffer
        (cond
         ((listp match-str)
          (mapc (lambda (match) (setq str (consult-omni--highlight-match match str t))) match-str))
         ((stringp match-str)
          (setq str (consult-omni--highlight-match match-str str t)))))
    str))

(defun consult-omni--openalex-annotate-works (results query params)
  "Annotate RESULTS for search QUERY of “works” in OpenAlex.

PARAMS are API parameters sent with query."
  (mapcar (lambda (item) (let*
      ((source "OpenAlex")
       (entity "works")
       (title (gethash "title" item))
       (biblio (gethash "biblio" item))
       (volume (and biblio
                    (hash-table-p biblio)
                    (gethash "volume" biblio)))
       (issue (and biblio
                   (hash-table-p biblio)
                   (gethash "issue" biblio)))
       (first-page (and biblio
                        (hash-table-p biblio)
                        (gethash "first_page" biblio)))
       (last-page (and biblio
                       (hash-table-p biblio)
                       (gethash "last_page" biblio)))
       (pages (concat (when first-page (format "%s-" first-page)) (when last-page (format "%s" last-page))))
       (authors (gethash "authorships" item))
       (authors (when (consp authors)
                  (mapcar (lambda (item) (let* ((name (map-nested-elt item '("author" "display_name")))
                                           (openalexid  (map-nested-elt item '("author" "id")))
                                           (id (when openalexid (string-remove-prefix consult-omni-openalex-search-url openalexid))))
                                           (when (stringp name) (propertize name :id id :openalexid openalexid))))
                          authors)))
       (authors (cond
                 ((stringp authors) (list authors))
                 (t authors)))
       (date (gethash "publication_date" item))
       (ids (gethash "ids" item))
       (openalexid (and ids
                        (hash-table-p ids)
                        (gethash "openalex" ids)))
       (id (if (stringp openalexid) (string-remove-prefix consult-omni-openalex-search-url openalexid)))
       (doi (and ids
                 (hash-table-p ids)
                 (gethash "doi" ids)))
       (pmid (and ids
                  (hash-table-p ids)
                  (gethash "pmid" ids)))
       (mag (and ids
                 (hash-table-p ids)
                 (gethash "mag" ids)))
       (pmic (and ids
                 (hash-table-p ids)
                 (gethash "pmic" ids)))
       (primary_location (gethash "primary_location" item))
       (url (and primary_location
                 (hash-table-p primary_location)
                 (gethash "landing_page_url" primary_location)))
       (pdf-url (and primary_location
                 (hash-table-p primary_location)
                 (gethash "pdf_url" primary_location)))
       (journal-source (and primary_location
                            (hash-table-p primary_location)
                            (gethash "source" primary_location)))
       (journal (and journal-source
                     (hash-table-p journal-source)
                     (gethash "display_name" journal-source)))
       (type (gethash "type" item))
       (search-url (consult-omni--make-url-string (concat consult-omni-openalex-search-url entity) params))
       (decorated (consult-omni--openalex-format-works-candidate :source source :query query :pdf-url pdf-url :title title :authors authors :date date :journal journal)))
    (propertize decorated
                :source source
                :type type
                :entity entity
                :url url
                :title title
                :search-url search-url
                :pdf-url pdf-url
                :query query
                :journal journal
                :issue issue
                :volume volume
                :pages pages
                :authors authors
                :date date
                :id id
                :openalexid openalexid
                :doi doi
                :pmid pmid
                :mag mag
                :pmic pmic)))
  results))

(defun consult-omni--openalex-annotate-authors (results query params)
  "Annotate RESULTS for search QUERY of “authors” in OpenAlex.

PARAMS are API parameters sent with query."
  (mapcar (lambda (item) (let*
      ((source "OpenAlex")
       (entity "authors")
       (title (gethash "display_name" item))
       (works-count (gethash "works_count" item))
       (cite-count (gethash "cited_by_count" item))
       (stats (gethash "summary_stats" item))
       (h-index (and stats
                    (hash-table-p stats)
                    (gethash "h_index" stats)))
       (i10-index (and stats
                   (hash-table-p stats)
                   (gethash "i10_index" stats)))
       (ids (gethash "ids" item))
       (openalexid (and ids
                        (hash-table-p ids)
                        (gethash "openalex" ids)))
       (id (if (stringp openalexid) (string-remove-prefix consult-omni-openalex-search-url openalexid)))
       (orcid (and ids
                 (hash-table-p ids)
                 (gethash "orcid" ids)))
       (mag (and ids
                 (hash-table-p ids)
                 (gethash "mag" ids)))
       (affiliations (gethash "affiliations" item))
       (affiliations (cond
                      ((consp affiliations)
                       (mapcar (lambda (item) (map-nested-elt item '("institution" "display_name"))) affiliations))
                  ((hash-table-p affiliations)
                   (map-nested-elt affiliations '("institution" "display_name")))))

       (affiliations (cond
                 ((stringp affiliations) (list affiliations))
                 (t affiliations)))
       (url openalexid)
       (search-url (consult-omni--make-url-string (concat consult-omni-openalex-search-url entity) params))
       (decorated (consult-omni--openalex-format-authors-candidate :source source :query query :title title :affiliations affiliations :h-index h-index :i10-index i10-index :works-count works-count :cite-count cite-count)))
    (propertize decorated
                :source source
                :entity entity
                :url url
                :title title
                :search-url search-url
                :query query
                :affiliations affiliations
                :openalexid openalexid
                :id id
                :orcid orcid
                :mag mag
                :h-index h-index
                :i10-index i10-index
                :works-count works-count
                :cite-count cite-count)))
  results))

(defun consult-omni--openalex-annotate-sources (results query params)
  "Annotate RESULTS for search QUERY of “sources” in OpenAlex.

PARAMS are API parameters sent with query."
  (mapcar (lambda (item) (let*
      ((source "OpenAlex")
       (entity "sources")
       (title (gethash "display_name" item))

       (stats (gethash "summary_stats" item))
       (impact (and stats
                   (hash-table-p stats)
                   (gethash "2yr_mean_citedness" stats)))
       (h-index (and stats
                    (hash-table-p stats)
                    (gethash "h_index" stats)))
       (i10-index (and stats
                   (hash-table-p stats)
                   (gethash "i10_index" stats)))
       (ids (gethash "ids" item))
       (openalexid (and ids
                        (hash-table-p ids)
                        (gethash "openalex" ids)))
       (id (if (stringp openalexid) (string-remove-prefix consult-omni-openalex-search-url openalexid)))
       (issn (and ids
                 (hash-table-p ids)
                 (gethash "issn" ids)))
       (mag (and ids
                 (hash-table-p ids)
                 (gethash "mag" ids)))
       (cost (gethash "apc_usd" item))
       (concepts (gethash "x_concepts" item))
       (concepts (cond
                      ((consp concepts)
                       (mapcar (lambda (item) (gethash "display_name" item)) concepts))
                  ((hash-table-p concepts)
                   (gethash "display_name" concepts))))
       (concepts (cond
                 ((stringp concepts) (list concepts))
                 (t concepts)))
       (url openalexid)
       (home-url (gethash "homepage_url" item))
       (type (gethash "type" item))
              (search-url (consult-omni--make-url-string (concat consult-omni-openalex-search-url entity) params))
       (decorated (consult-omni--openalex-format-sources-candidate :source source :query query :title title :type type :concepts concepts :impact impact :cost cost)))
    (propertize decorated
                :source source
                :entity entity
                :url url
                :home-url home-url
                :type type
                :title title
                :search-url search-url
                :query query
                :concepts concepts
                :openalexid openalexid
                :id id
                :issn issn
                :mag mag
                :h-index h-index
                :i10-index i10-index
                :impact impact
                :cost cost)))
  results))

(defun consult-omni--openalex-annotate-institutions (results query params)
  "Annotate RESULTS for search QUERY of “institutions” in OpenAlex.

PARAMS are API parameters sent with query."
  (mapcar (lambda (item) (let*
      ((source "OpenAlex")
       (entity "institutions")
       (title (gethash "display_name" item))
       (acronyms (gethash "display_name_acronyms" item))
       (acronym (car-safe acronyms))
       (works-count (gethash "works_count" item))
       (cite-count (gethash "cited_by_count" item))
       (stats (gethash "summary_stats" item))
       (h-index (and stats
                    (hash-table-p stats)
                    (gethash "h_index" stats)))
       (i10-index (and stats
                   (hash-table-p stats)
                   (gethash "i10_index" stats)))
       (impact (and stats
                   (hash-table-p stats)
                   (gethash "2yr_mean_citedness" stats)))
       (ids (gethash "ids" item))
       (openalexid (and ids
                        (hash-table-p ids)
                        (gethash "openalex" ids)))
       (id (if (stringp openalexid) (string-remove-prefix consult-omni-openalex-search-url openalexid)))
       (grid (and ids
                 (hash-table-p ids)
                 (gethash "grid" ids)))
       (mag (and ids
                 (hash-table-p ids)
                 (gethash "mag" ids)))
       (country (gethash "country_code" item))
       (concepts (gethash "x_concepts" item))
       (concepts (cond
                      ((consp concepts)
                       (mapcar (lambda (item) (gethash "display_name" item)) concepts))
                  ((hash-table-p concepts)
                   (gethash "display_name" concepts))))
       (concepts (cond
                 ((stringp concepts) (list concepts))
                 (t concepts)))
       (url openalexid)
       (home-url (gethash "homepage_url" item))
       (type (gethash "type" item))
       (associations (gethash "associated_institutions" item))
       (associations (cond
                      ((consp associations)
                       (mapcar (lambda (item) (map-nested-elt item '("institution" "display_name"))) associations))
                  ((hash-table-p associations)
                   (map-nested-elt associations '("institution" "display_name")))))

       (associations (cond
                 ((stringp associations) (list associations))
                 (t associations)))
       (search-url (consult-omni--make-url-string (concat consult-omni-openalex-search-url entity) params))
       (decorated (consult-omni--openalex-format-institutions-candidate :source source :query query :title title :acronym acronym :impact impact :works-count works-count :cite-count cite-count :concepts concepts :type type)))
    (propertize decorated
                :source source
                :type type
                :entity entity
                :url url
                :title title
                :acronym acronym
                :search-url search-url
                :query query
                :associations associations
                :country country
                :home-url home-url
                :openalexid openalexid
                :id id
                :grid grid
                :mag mag
                :h-index h-index
                :i10-index i10-index
                :impact impact
                :works-count works-count
                :cite-count cite-count)))
  results))

(defun consult-omni--openalex-callback (cand)
  "Callback function for CAND from `consult-omni-openalex'."
  (let* ((url (get-text-property 0 :url cand))
         (openalexurl (get-text-property 0 :openalexid cand)))
    (funcall consult-omni-default-browse-function (url-encode-url (or url openalexurl)))))

(defun consult-omni--openalex-preview (cand)
  "Preview function for CAND from `consult-omni-openalex'."
  (if-let* ((url (get-text-property 0 :url cand)))
    (funcall consult-omni-default-preview-function url)))

(cl-defun consult-omni--openalex-fetch-results (input &rest args &key callback &allow-other-keys)
  "Fetch search results from OpenAlex for INPUT and ARGS.

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
               (sort (plist-get opts :sort))
               (entity (or (plist-get opts :entity)
                           (plist-get opts :e)))
               (filter (or (plist-get opts :filter)
                           (plist-get opts :f)))
               (count (or (and count (integerp (read count)) (string-to-number count))
                          consult-omni-default-count))
               (page (or (and page (integerp (read page)) (string-to-number page))
                         consult-omni-default-page))
               (page (+ page 1))
               (entity (or (and entity (format "%s" entity)) consult-omni-openalex-default-entity))
               (params `(("search" . ,query)
                         ("per-page" . ,(format "%s" count))
                         ("page" . ,(format "%s" page))))
               (_ (when consult-omni-openalex-extra-params (setq params (append params consult-omni-openalex-extra-params))))
               (_ (when filter (setq params (append params `(("filter" . ,(format "%s" filter)))))))
               (_ (pcase entity
                    ("sources"
                     (setq params (append params `(("sort" . ,(or sort "cited_by_count:desc"))))))))
               (headers `(("Accept" . "application/json"))))
    (consult-omni--fetch-url (concat consult-omni-openalex-api-url entity) consult-omni-http-retrieve-backend
                             :encoding 'utf-8
                             :params params
                             :headers headers
                             :parser #'consult-omni--openalex-parse-buffer
                             :callback
                             (lambda (attrs)
                               (when-let* ((raw-results (gethash "results" attrs))
                                           (annotated-results
                                           (pcase entity
                                                  ("works" (consult-omni--openalex-annotate-works raw-results query params))
                                                  ("authors" (consult-omni--openalex-annotate-authors raw-results query params))
                                                  ("sources" (consult-omni--openalex-annotate-sources raw-results query params))
                                                  ("institutions" (consult-omni--openalex-annotate-institutions raw-results query params)))))
                                 (funcall callback annotated-results))))))

;; Define the OpenAlex source
(consult-omni-define-source "OpenAlex"
                            :narrow-char ?o
                            :type 'dynamic
                            :require-match nil
                            :category 'consult-omni-openalex
                            :face 'consult-omni-scholar-title-face
                            :request #'consult-omni--openalex-fetch-results
                            :preview-key consult-omni-preview-key
                            :on-preview #'consult-omni--openalex-preview
                            :on-return #'identity
                            :on-callback #'consult-omni--openalex-callback
                            :search-hist 'consult-omni--search-history
                            :select-hist 'consult-omni--selection-history
                            :group #'consult-omni--group-function
                            :sort t
                            :interactive consult-omni-intereactive-commands-type
                            :annotate nil)

;;Define an interactive function for selecting openalex entity first
(defun consult-omni-openalex-entity (&optional initial prompt no-callback &rest args)
  "Search openalex after selecting an entity first."
  (interactive "P")
  (let* ((consult-omni-openalex-default-entity (consult--read '(("Papers and Publications" ."works")
                                                               ("Authors" . "authors")
                                                               ("Sources (e.g. Journals, Repositories, Conferences, ...)" . "sources")
                                                               ("Institutions (e.g. Universities, Companies, Government Organizations, ...". "institutions"))
                                                             :prompt "Select the entity you want to search:"
                                                             :lookup #'consult--lookup-cdr)))
    (consult-omni-openalex initial prompt no-callback args)))

;;; provide `consult-omni-openalex' module

(provide 'consult-omni-openalex)

(add-to-list 'consult-omni-sources-modules-to-load 'consult-omni-openalex)
;;; consult-omni-openalex.el ends here
