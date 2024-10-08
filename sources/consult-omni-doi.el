;;; consult-omni-doi.el --- Consulting DOI.org -*- lexical-binding: t -*-

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

(defvar consult-omni-doiorg-api-url "https://doi.org/api/handles/"
"API URL for DOI.org")

(defvar consult-omni-doiorg-search-url "https://doi.org/"
"Search URL for DOI.org")

(defvar consult-omni--doi-search-history (list)
  "History variables for search terms of `consult-omni-doi'.")

(defvar consult-omni--doi-selection-history (list)
  "History variables for selected items of `consult-omni-doi'.")

(defun consult-omni--doi-to-url (doi)
  "Converts DOI value to target url."
  (let ((out))
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
                                   result))))))

(cl-defun consult-omni--doiorg-fetch-results (input &rest args &key callback &allow-other-keys)
  "Fetch target url of DOI."
  (pcase-let* ((`(,query . ,opts) (consult-omni--split-command input (seq-difference args (list :callback callback))))
               (opts (car-safe opts))
               (source "doiorg")
               (url (consult-omni--doi-to-url query))
               (title (if url (format "%s" url) (format "%s - Not Found" query)))
               (search-url (concat consult-omni-doiorg-search-url query))
               (decorated (funcall consult-omni-default-format-candidate :source source :query query :url url :search-url search-url :title title))
               (annotated-results (propertize decorated
                                              :source source
                                              :title title
                                              :url url
                                              :search-url search-url
                                              :query query)))
    (when url
      (list annotated-results))))

;; Define the DOI.org Source
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
