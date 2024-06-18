;;; consult-omni-bing.el --- Consulting Bing -*- lexical-binding: t -*-

;; Copyright (C) 2024 Armin Darvish

;; Author: Armin Darvish
;; Maintainer: Armin Darvish
;; Created: 2024
;; Version: 0.1
;; Package-Requires: ((emacs "28.1") (consult "1.1") (consult-omni "0.2"))
;; Homepage: https://github.com/armindarvish/consult-omni
;; Keywords: convenience

;;; Commentary:

;;; Code:
(require 'consult-omni)

(defvar consult-omni-bing-search-api-url "https://api.bing.microsoft.com/v7.0/search")

(defcustom consult-omni-bing-search-api-key nil
  "Key for Bing (Microsoft Azure) search API

See URL `https://www.microsoft.com/en-us/bing/apis/bing-web-search-api' and URL `https://learn.microsoft.com/en-us/bing/search-apis/bing-web-search/search-the-web' for details"
  :group 'consult-omni
  :type '(choice (const :tag "API Key" string)
                 (function :tag "Custom Function")))

(cl-defun consult-omni--bing-fetch-results (input &rest args &key callback &allow-other-keys)
  "Fetches search results for INPUT from Bing web search api.

Refer to URL `https://programmablesearchengine.google.com/about/' and `https://developers.google.com/custom-search/' for more info.
"
  (pcase-let* ((`(,query . ,opts) (consult-omni--split-command input args))
               (opts (car-safe opts))
               (count (plist-get opts :count))
               (page (plist-get opts :page))
               (count (or (and (integerp count) count)
                          (and count (string-to-number (format "%s" count)))
                          consult-omni-default-count))
               (page (or (and (integerp page) page)
                         (and page (string-to-number (format "%s" page)))
                         consult-omni-default-page))
               (count (max count 1))
               (page (* page count))
               (params `(("q" . ,(replace-regexp-in-string " " "+" query))
                         ("count" . ,(format "%s" count))
                         ("offset" . ,(format "%s" page))))
               (headers `(("Ocp-Apim-Subscription-Key" . ,(consult-omni-expand-variable-function consult-omni-bing-search-api-key)))))
    (consult-omni--fetch-url consult-omni-bing-search-api-url consult-omni-http-retrieve-backend
                            :encoding 'utf-8
                            :params params
                            :headers headers
                            :parser #'consult-omni--json-parse-buffer
                            :callback
                            (lambda (attrs)
                              (let* ((raw-results (map-nested-elt attrs '("webPages" "value")))
                                     (search-url (gethash "webSearchUrl" attrs))
                                     (annotated-results
                                      (mapcar (lambda (item)
                                                (let*
                                                    ((source "Bing")
                                                     (url (format "%s" (gethash "url" item)))
                                                     (title (gethash "name" item))
                                                     (snippet (gethash "snippet" item))
                                                     (decorated (funcall consult-omni-default-format-candidate :source source :query query :url url :search-url search-url :title title :snippet snippet)))
                                                  (propertize decorated
                                                              :source source
                                                              :title title
                                                              :url url
                                                              :search-url search-url
                                                              :query query
                                                              :snippet snippet)))

                                              raw-results)))
                                (when annotated-results
                                  (funcall callback annotated-results))
                                annotated-results)))))

(consult-omni-define-source "Bing"
                           :narrow-char ?m
                           :type 'dynamic
                           :face 'consult-omni-engine-source-face
                           :request #'consult-omni--bing-fetch-results
                           :preview-key consult-omni-preview-key
                           :search-history 'consult-omni--search-history
                           :selection-history 'consult-omni--selection-history
                           :enabled (lambda () (bound-and-true-p consult-omni-bing-search-api-key))
                           :group #'consult-omni--group-function
                           :sort t
                           :static 'both
                           :annotate nil
                           )

;;; provide `consult-omni-bing' module

(provide 'consult-omni-bing)

(add-to-list 'consult-omni-sources-modules-to-load 'consult-omni-bing)
;;; consult-omni-bing.el ends here
