;;; consult-omni-doi.el --- Consulting DOI.org -*- lexical-binding: t -*-

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
;; consult-omni-doi enables looking up DOIs in consult-omni.
;; It provides commands to search DOIs using DOI.org as the backend.
;;
;; For more info DOI see:
;; URL `https://www.doi.org/'

;;; Code:

(require 'consult-omni)

(defvar consult-omni-doiorg-api-url "https://doi.org/api/handles/"
  "API URL for “DOI.org”.")

(defvar consult-omni-doiorg-search-url "https://doi.org/"
  "Search URL for “DOI.org”.")

(defvar consult-omni--doi-search-history (list)
  "History variables for search terms of `consult-omni-doi'.")

(defvar consult-omni--doi-selection-history (list)
  "History variables for selected items of `consult-omni-doi'.")

(defun consult-omni--doi-to-url (doi)
  "Convert DOI value to target url."
  (let* ((doi (if doi (format "%s" doi)))
         (url (concat consult-omni-doiorg-api-url doi)))
      (consult-omni--fetch-url url consult-omni-http-retrieve-backend
                               :sync t
                               :encoding 'utf-8
                               :parser #'consult-omni--json-parse-buffer
                               :callback
                               (lambda (attrs)
                                 (let* ((raw-results (map-nested-elt attrs '("values")))
                                        (result (car-safe (remove nil
                                                                  (mapcar
                                                                   (lambda (item)
                                                                     (if-let* ((type (gethash "type" item))
                                                                               (link (if (equal type "URL") (map-nested-elt item '("data" "value")))))
                                                                         link))
                                                                   raw-results)))))
                                   result)))))

(cl-defun consult-omni--doiorg-fetch-results (doi &rest args &key callback &allow-other-keys)
  "Fetch target url of DOI with ARGS.

CALLBACK is a function used internally to update the list of candidates in
the minibuffer asynchronously.  It is called with a list of strings, which
are new annotated candidates \(e.g. as they arrive from an asynchronous
process\) to be added to the minibuffer completion cnadidates.  See the
section on REQUEST in documentation for `consult-omni-define-source' as
well as the function
`consult-omni--multi-update-dynamic-candidates' for how CALLBACK is used."
  (pcase-let* ((`(,query . _) (consult-omni--split-command doi (seq-difference args (list :callback callback))))
               (source "doiorg")
               (url (consult-omni--doi-to-url query))
               (title (if url (format "%s" url) (format "%s - Not Found" query)))
               (search-url (concat consult-omni-doiorg-search-url query))
               (decorated (funcall consult-omni-default-format-candidate :source source :query query :url url :title title))
               (annotated-results (propertize decorated
                                              :source source
                                              :title title
                                              :url url
                                              :search-url search-url
                                              :query query)))
    (when url
      (list annotated-results))))

;; Define the doiorg (for DOI.org) source
(consult-omni-define-source "doiorg"
                            :narrow-char ?D
                            :type 'sync
                            :require-match t
                            :face 'link
                            :request #'consult-omni--doiorg-fetch-results
                            :preview-key consult-omni-preview-key
                            :search-hist 'consult-omni--doi-search-history
                            :select-hist 'consult-omni--doi-selection-history
                            :enabled (lambda () (bound-and-true-p consult-omni-doiorg-search-url))
                            :group #'consult-omni--group-function
                            :sort t
                            :interactive consult-omni-intereactive-commands-type)

;;; provide `consult-omni-doi' module

(provide 'consult-omni-doi)

(add-to-list 'consult-omni-sources-modules-to-load 'consult-omni-doi)
;;; consult-omni-doi.el ends here
