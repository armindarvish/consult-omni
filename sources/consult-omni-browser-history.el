;;; consult-omni-browser-history.el --- Consulting Browser History -*- lexical-binding: t -*-

;; Copyright (C) 2024 Armin Darvish

;; Author: Armin Darvish
;; Maintainer: Armin Darvish
;; Created: 2024
;; Version: 0.3
;; Package-Requires: (
;;         (emacs "29.4")
;;         (consult "2.0")
;;         (browser-hist "0.0.1")
;;         (consult-omni "0.3"))
;;
;; Homepage: https://github.com/armindarvish/consult-omni
;; Keywords: convenience

;;; Commentary:
;; consult-omni-browser-history provides commands for searching browser
;; history in Emacs using consult-omni.  It uses the browser-hist.el
;; package as the backend.
;; See URL https://github.com/agzam/browser-hist.el


;;; Code:

(require 'consult-omni)
(require 'browser-hist)

(cl-defun consult-omni--browser-history-format-candidate (&rest args &key source query url title face &allow-other-keys)
  "Format candidates of `consult-omni-browser-history' with ARGS.

Description of Arguments:

  SOURCE     a string; the name string of the source for candidate
  QUERY      a string; the query string used for searching
  URL        a string; the URL of the candidate
  TITLE      a string; the title of the candidate
  FACE       a symbol; the face to use for title"
  (let* ((frame-width-percent (floor (* (frame-width) 0.1)))
         (source (and (stringp source) (propertize source 'face 'consult-omni-source-type-face)))
         (match-str (and (stringp query) (not (equal query ".*")) (consult--split-escaped query)))
         (face (or (consult-omni--get-source-prop source :face) face 'consult-omni-default-face))
         (title-str (propertize title 'face face))
         (title-str (consult-omni--set-string-width title-str (* 4 frame-width-percent)))
         (urlobj (and url (url-generic-parse-url url)))
         (domain (and (url-p urlobj) (or (url-domain urlobj) (url-host urlobj))))
         (port (and (url-p urlobj) (url-port urlobj)))
         (domain (and domain
                      (if port (format "%s:%s" domain port) (format "%s" domain))))
         (domain (and (stringp domain) (propertize domain 'face 'consult-omni-domain-face)))
         (path (and (url-p urlobj) (url-filename urlobj)))
         (path (and (stringp path) (propertize path 'face 'consult-omni-path-face)))
         (url-str (consult-omni--set-url-width domain path (* frame-width-percent 5)))
         (str (concat title-str
                      (when url-str (concat "\s" url-str))
                      (when source (concat "\t" source)))))
    (if consult-omni-highlight-matches-in-minibuffer
        (cond
         ((listp match-str)
          (mapc (lambda (match) (setq str (consult-omni--highlight-match match str t))) match-str))
         ((stringp match-str)
          (setq str (consult-omni--highlight-match match-str str t)))))
    str))

(cl-defun consult-omni--browser-history-fetch-results (input &rest args &key callback &allow-other-keys)
  "Fetch search results for INPUT from browser history with ARGS.

CALLBACK is a function used internally to update the list of candidates in
the minibuffer asynchronously.  It is called with a list of strings, which
are new annotated candidates \(e.g. as they arrive from an asynchronous
process\) to be added to the minibuffer completion cnadidates.  See the
section on REQUEST in documentation for `consult-omni-define-source' as
well as the function
`consult-omni--multi-update-dynamic-candidates' for how CALLBACK is used."
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
                            :interactive consult-omni-intereactive-commands-type
                            :annotate nil)

;;; provide `consult-omni-browser-history' module

(provide 'consult-omni-browser-history)

(add-to-list 'consult-omni-sources-modules-to-load 'consult-omni-browser-history)
;;; consult-omni-browser-history.el ends here
