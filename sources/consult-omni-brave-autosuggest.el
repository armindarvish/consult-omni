;;; consult-omni-brave-autosuggest.el --- Consulting Brave Autosuggest -*- lexical-binding: t -*-

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

(defun consult-omni--brave-autosuggest-return (cand)
  "Return the string of CAND with no properties
"
(when (stringp cand)
  (substring-no-properties (string-trim cand))))

(defvar consult-omni-brave-autosuggest-api-url "https://api.search.brave.com/res/v1/suggest/search")

(defcustom consult-omni-brave-autosuggest-api-key nil
  "Key for Brave Autosuggest API.

See URL `https://brave.com/search/api/' for more info"
  :group 'consult-omni
  :type '(choice (const :tag "Brave Autosuggest API Key" string)
                 (function :tag "Custom Function")))

(cl-defun consult-omni--brave-autosuggest-fetch-results (input &rest args &key callback &allow-other-keys)
  "Fetch search results for INPUT from Brave Autosuggest API.
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
               (count (min (max count 1) 20))
               (params  `(("q" . ,query)
                          ("count" . ,(format "%s" count))
                          ("page" . ,(format "%s" page))
                          ("country" . "US")))
               (headers `(("User-Agent" . "Emacs:consult-omni/0.1 (Emacs consult-omni package; https://github.com/armindarvish/consult-omni)")
                          ("Accept" . "application/json")
                          ("Accept-Encoding" . "gzip")
                          ("X-Subscription-Token" . ,(consult-omni-expand-variable-function consult-omni-brave-autosuggest-api-key))
                          )))
    (consult-omni--fetch-url consult-omni-brave-autosuggest-api-url consult-omni-http-retrieve-backend
                                  :params params
                                  :headers headers
                                  :parser #'consult-omni--json-parse-buffer
                                  :callback
                                  (lambda (attrs)
                                    (when-let* ((original (make-hash-table :test 'equal))
                                                (_ (puthash "query" (gethash "original" (gethash "query" attrs)) original))
                                                (raw-results  (append (map-nested-elt attrs '("results")) (list original)))
                                                (annotated-results
                                                 (mapcar (lambda (item)
                                                           (let* ((source "Brave AutoSuggest")
                                                                  (word (gethash "query" item))
                                                                  (url (concat "https://search.brave.com/search?q="  (replace-regexp-in-string " " "+" word)))
                                                                  (urlobj (and url (url-generic-parse-url url)))
                                                                  (domain (and (url-p urlobj) (url-domain urlobj)))
                                                                  (domain (and (stringp domain)
                                                                               (propertize domain 'face 'font-lock-variable-name-face)))
                                                                  (path (and (url-p urlobj) (url-filename urlobj)))
                                                                  (path (and (stringp path)
                                                                             (propertize path 'face 'font-lock-warning-face)))
                                                                  (search-url nil)
                                                                  (decorated (propertize word 'face 'consult-omni-default-face)))
                                                             (propertize decorated
                                                                         :source source
                                                                         :title word
                                                                         :url url
                                                                         :search-url search-url
                                                                         :query query)))

                                                         raw-results)))
                                      (funcall callback annotated-results)
                                      annotated-results)))))

(consult-omni-define-source "Brave AutoSuggest"
                           :narrow-char ?B
                           :type 'dynamic
                           :face 'consult-omni-engine-source-face
                           :request #'consult-omni--brave-autosuggest-fetch-results
                           :group #'consult-omni--group-function
                           :on-preview #'ignore
                           :on-return #'consult-omni--brave-autosuggest-return
                           :on-callback #'string-trim
                           :search-history 'consult-omni--search-history
                           :selection-history t
                           :enabled (lambda () (bound-and-true-p consult-omni-brave-autosuggest-api-key))
                           :sort t
                           :static t
                           )

;;; provide `consult-omni-brave-autosuggest' module

(provide 'consult-omni-brave-autosuggest)

(add-to-list 'consult-omni-sources-modules-to-load 'consult-omni-brave-autosuggest)
;;; consult-omni-brave-autosuggest.el ends here
