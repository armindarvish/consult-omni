;;; consult-omni-brave.el --- Consulting Brave -*- lexical-binding: t -*-

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

(defcustom consult-omni-brave-api-key nil
  "Key for Brave API.

See URL `https://brave.com/search/api/' for more info"
  :group 'consult-omni
  :type '(choice (const :tag "Brave API Key" string)
                 (function :tag "Custom Function")))

(defvar consult-omni-brave-search-url "https://search.brave.com/search")

(defvar consult-omni-brave-url "https://api.search.brave.com/res/v1/web/search")

(cl-defun consult-omni--brave-fetch-results (input &rest args &key callback &allow-other-keys)
  "Retrieve search results from Brave for INPUT."
  (pcase-let* ((`(,query . ,opts) (consult-omni--split-command input (seq-difference args (list :callback callback))))
               (opts (car-safe opts))
               (count (plist-get opts :count))
               (page (plist-get opts :page))
               (count (or (and count (integerp (read count)) (string-to-number count))
                          consult-omni-default-count))
               (page (or (and page (integerp (read page)) (string-to-number page))
                         consult-omni-default-page))
               (count (min (max count 1) 20))
               (params `(("q" . ,(replace-regexp-in-string " " "+" query))
                         ("count" . ,(format "%s" count))
                         ("page" . ,(format "%s" page))))
               (headers `(("User-Agent" . "Emacs:consult-omni/0.1 (Emacs consult-omni package; https://github.com/armindarvish/consult-omni)")
                          ("Accept" . "application/json")
                          ("Accept-Encoding" . "gzip")
                          ("X-Subscription-Token" . ,(consult-omni-expand-variable-function consult-omni-brave-api-key)))))
    (consult-omni--fetch-url consult-omni-brave-url consult-omni-http-retrieve-backend
      :encoding 'utf-8
      :params params
      :headers headers
      :parser #'consult-omni--json-parse-buffer
      :callback
      (lambda (attrs)
        (when-let* ((raw-results (map-nested-elt attrs '("web" "results")))
                    (annotated-results
                     (mapcar (lambda (item)
                               (let*
                                   ((source "Brave")
                                    (url (gethash "url" item))
                                    (title (gethash "title" item))
                                    (snippet (gethash "description" item))
                                    (search-url (consult-omni--make-url-string consult-omni-brave-search-url params))
                                    (decorated (funcall consult-omni-default-format-candidate :source source :query query :url url :search-url search-url :title title :snippet snippet)))
                                 (propertize decorated
                                             :source source
                                             :title title
                                             :url url
                                             :search-url search-url
                                             :query query
                                             :snippet snippet)))
                             raw-results)))
          (funcall callback annotated-results)
          annotated-results)))))

;; Define the Brave Source
(consult-omni-define-source "Brave"
                            :narrow-char ?b
                            :type 'dynamic
                            :require-match nil
                            :face 'consult-omni-engine-title-face
                            :request #'consult-omni--brave-fetch-results
                            :on-new (apply-partially #'consult-omni-external-search-with-engine "Brave")
                            :preview-key consult-omni-preview-key
                            :search-hist 'consult-omni--search-history
                            :select-hist 'consult-omni--selection-history
                            :enabled (lambda () (bound-and-true-p consult-omni-brave-api-key))
                            :group #'consult-omni--group-function
                            :sort t
                            :static 'both)

;;; provide `consult-omni-brave' module

(provide 'consult-omni-brave)

(add-to-list 'consult-omni-sources-modules-to-load 'consult-omni-brave)
;;; consult-omni-brave.el ends here
