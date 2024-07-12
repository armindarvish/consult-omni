;;; consult-omni-browser-history.el --- Consulting Browser History -*- lexical-binding: t -*-

;; Copyright (C) 2024 Armin Darvish

;; Author: Armin Darvish
;; Maintainer: Armin Darvish
;; Created: 2024
;; Version: 0.1
;; Package-Requires: ((emacs "28.1") (consult "1.4") (consult-omni "0.1") (browser-hist "0.0.1"))
;; Homepage: https://github.com/armindarvish/consult-omni
;; Keywords: convenience

;;; Commentary:

;;; Code:

(require 'consult-omni)
(require 'browser-hist)

(cl-defun consult-omni--browser-history-format-candidate (&rest args &key source query url search-url title face &allow-other-keys)
  "Formats candidates of `consult-omni-browser-history'.

Description of Arguments:

  SOURCE     the name string of the source for candidate
  QUERY      the query string used for searching
  URL        a string pointing to url of the candidate
  SEARCH-URL a string pointing to the url for
             the search results of QUERY on the SOURCE website
  TITLE      the title of the candidate
  SNIPPET    a string containing a snippet/description of candidate"
  (let* ((frame-width-percent (floor (* (frame-width) 0.1)))
         (source (and (stringp source) (propertize source 'face 'consult-omni-source-type-face)))
         (match-str (and (stringp query) (consult--split-escaped query)))
         (face (or (consult-omni--get-source-prop source :face) face 'consult-omni-default-face))
         (title-str (propertize title 'face face))
         (title-str (consult-omni--set-string-width title-str (* 4 frame-width-percent)))
         (urlobj (and url (url-generic-parse-url url)))
         (domain (and (url-p urlobj) (url-domain urlobj)))
         (domain (and (url-p urlobj) (or (url-domain urlobj) (url-host urlobj))))
         (port (and (url-p urlobj) (url-port urlobj)))
         (domain (if port (format "%s:%s" domain port) (format "%s" domain)))
         (domain (and (stringp domain) (propertize domain 'face 'consult-omni-domain-face)))
         (path (and (url-p urlobj) (url-filename urlobj)))
         (path (and (stringp path) (propertize path 'face 'consult-omni-path-face)))
         (url-str (consult-omni--set-url-width domain path (* frame-width-percent 5)))
         (str (concat title-str
                      (when url-str (concat "\s" url-str))
                      (when source (concat "\t" source)))))
    (if consult-omni-highlight-matches
        (cond
         ((listp match-str)
          (mapcar (lambda (match) (setq str (consult-omni--highlight-match match str t))) match-str))
         ((stringp match-str)
          (setq str (consult-omni--highlight-match match-str str t)))))
    str))

(cl-defun consult-omni--browser-history-fetch-results (input &rest args &key callback &allow-other-keys)
  "Fetch search results for INPUT from browser history."
 (pcase-let* ((`(,query . ,opts) (consult-omni--split-command input (seq-difference args (list :callback callback))))
               (opts (car-safe opts))
               (browser (or (plist-get opts :browser) browser-hist-default-browser))
               (browser-hist-default-browser browser)
               (results (browser-hist--send-query query))
               (source "Browser History"))
      (mapcar (lambda (item)
                (let* ((url (car-safe item))
                      (title (cdr-safe item))
                      (decorated (consult-omni--browser-history-format-candidate :source source :query query :url url :title title)))
                  (propertize decorated
                              :source source
                              :title title
                              :url url
                              :query query)))
              results)))

;; Define the Browse History Source
(consult-omni-define-source "Browser History"
                            :narrow-char ?H
                            :type 'sync
                            :require-match nil
                            :face 'consult-omni-engine-title-face
                            :request #'consult-omni--browser-history-fetch-results
                            :preview-key consult-omni-preview-key
                            :search-hist 'consult-omni--search-history
                            :select-hist 'consult-omni--selection-history
                            :enabled (lambda () (fboundp 'browser-hist-search))
                            :group #'consult-omni--group-function
                            :sort t
                            :static 'both
                            :annotate nil)

;;; provide `consult-omni-browser-history' module

(provide 'consult-omni-browser-history)

(add-to-list 'consult-omni-sources-modules-to-load 'consult-omni-browser-history)
;;; consult-omni-browser-history.el ends here
