;;; consult-omni-notmuch.el --- Consulting Notmuch Command -*- lexical-binding: t -*-

;; Copyright (C) 2024 Armin Darvish

;; Author: Armin Darvish
;; Maintainer: Armin Darvish
;; Created: 2024
;; Version: 0.3
;; Package-Requires: (
;;         (emacs "29.4")
;;         (consult "2.0")
;;         (consult-notmuch "0.8.1")
;;         (consult-omni "0.3"))
;;
;; Homepage: https://github.com/armindarvish/consult-omni
;; Keywords: convenience

;;; Commentary:
;; consult-omni-notmuch enables searching notmuch emails in consult-omni.
;; It provides commands to search emails and getting the results
;; directly in the minibuffer.
;;
;; For more info on notmuch see the following URLs:
;; URL `https://github.com/notmuch/notmuch'
;; URL `https://notmuchmail.org/notmuch-emacs/'

;;; Code:

(require 'consult-omni)
(require 'notmuch)

(defcustom consult-omni-notmuch-message-buffer-name "*consult-omni-notmuch-message*"
  "Name of consult-omni-notmuch preview buffer."
  :group 'consult-omni
  :type 'string)

(defcustom consult-omni-notmuch-tree-buffer-name "*consult-omni-notmuch-tree*"
  "Name of consult-omni-notmuch tree buffer."
  :group 'consult-omni
  :type 'string)

(defcustom consult-omni-notmuch-command (or notmuch-command "notmuch")
  "Name of the notmuch binary.

By default inherits from `notmuch-command'."
  :group 'consult-omni
  :type 'string)

(defcustom consult-omni-notmuch-default-command-arg "search"
  "Default notmuch commandline arg for finding messages.
Can be either “search” or “show”"
  :group 'consult-omni
  :type  '(choice (const :tag "(Default) search" "search")
                  (const :tag "show" "show")))

(defcustom consult-omni-notmuch-extra-command-args (list)
  "Extra notmuch commandline arguments."
  :group 'consult-omni
  :type '(repeat (choice string)))

(defcustom consult-omni-notmuch-default-count consult-omni-default-count
  "Number of notmuch search results to retrieve.

By default inherits from `consult-omni-default-count'."
  :group 'consult-omni
  :type 'integer)

(defvar consult-omni--notmuch-format-func-alist '(("show" . consult-omni--notmuch-show-transform)
                                                  ("search" . consult-omni--notmuch-search-transform))
  "Alist of transfrom functions for notmuch commandline output.")

(cl-defun consult-omni--notmuch-format-candidate (&rest args &key source query title from date tags face &allow-other-keys)
  "Format a candidate from `consult-omni-notmuch' with ARGS.

Description of Arguments:

  SOURCE a string; the name to use (e.g. “YouTube”)
  QUERY  a string; the query input from the user
  TITLE  a string; the notmuch title string of the message
  FROM   a string; the notmuch sender string of the message
  DATE   a string; the notmuch date string of the message
  TAGS   a list; a list of notmuch tag string(s) for message
  FACE   a symbol; the face to apply to TITLE"
  (let* ((frame-width-percent (floor (* (frame-width) 0.1)))
         (source (if (stringp source) (propertize source 'face 'consult-omni-source-type-face) nil))
         (match-str (if (and (stringp query) (not (equal query ".*"))) (consult--split-escaped query) nil))
         (date (if (stringp date) (propertize date 'face 'consult-omni-date-face) "            "))
         (from (if (stringp from) (propertize from 'face 'consult-omni-path-face) ""))
         (from-str (and (stringp from) (consult-omni--set-string-width from (* 2 frame-width-percent))))
         (tags (cond ((and tags (listp tags)) (format "(%s)" (mapconcat #'identity tags ", ")))
                     ((and tags (stringp tags)) tags)
                     (t nil)))
         (tags (and tags (stringp tags) (propertize tags 'face 'consult-omni-keyword-face)))
         (face (or (consult-omni--get-source-prop source :face) face 'consult-omni-default-face))
         (title-str (if (stringp title) (propertize title 'face face)))
         (title-str (if (stringp title-str) (consult-omni--set-string-width title-str (* 5 frame-width-percent))))
         (str (if (string-empty-p from) ""
                (concat (if date (format "%s" date))
                        (if from-str (format "\s%s" from-str))
                        "\t"
                        (if title-str title-str)
                        (if tags (format "\s\s%s" tags))
                        (if source (concat "\t" source))))))
    (if consult-omni-highlight-matches-in-minibuffer
        (cond
         ((and (listp match-str) (stringp str))
          (mapc (lambda (match) (setq str (consult-omni--highlight-match match str t))) match-str))
         ((and (stringp match-str) (stringp str))
          (setq str (consult-omni--highlight-match match-str str t)))))
    str))

(defun consult-omni--notmuch-search-transform (candidates &optional query)
  "Transform CANDIDATES from “notmuch search” to consult-omni's style.

QUERY is the user input string.

Parses the output from command “notmuch search” and passes its
components to  `consult-omni--notmuch-format-candidate'."
  (remove nil (remove "" (mapcar (lambda (item)
                                   (when (and (stringp item) (string-match "thread:" item))
                                     (let* ((source "notmuch")
                                            (id (car (split-string item "\\ +")))
                                            (date (substring item 24 37))
                                            (mid (substring item 24))
                                            (c0 (string-match "[[]" mid))
                                            (c1 (string-match "[]]" mid))
                                            (count (substring mid c0 (1+ c1)))
                                            (senders (string-trim (nth 1 (split-string mid "[];]"))))
                                            (subject (string-trim (nth 1 (split-string mid "[;]"))))
                                            (headers (list :Subject subject :From senders))
                                            (t0 (string-match "([^)]*)\\s-*$" mid))
                                            (tags (split-string (substring mid (1+  t0) -1)))
                                            (decorated (consult-omni--notmuch-format-candidate :source source :query query :title subject :from senders :date date :tags tags)))
                                       (when (and (stringp decorated) (not (string-empty-p decorated)))
                                         (propertize decorated
                                                     :source source
                                                     :query query
                                                     :title subject
                                                     :url nil
                                                     :search-url nil
                                                     :id id
                                                     :from senders
                                                     :date date
                                                     :match t
                                                     :headers headers
                                                     :count count
                                                     :tags tags)))))
                                 candidates))))

(defun consult-omni--notmuch-show-transform (candidates &optional query)
  "Transform CANDIDATES from “notmuch show” to consult-omni's style.

QUERY is the user input string.

Parses the output from command “notmuch show” and passes its components
to `consult-omni--notmuch-format-candidate'."
  (let ((source "notmuch") (id) (headers) (subject) (senders) (cc) (to) (count) (date) (tags) (match) (info))
    (remove nil (mapcar (lambda (item)
                          (if (string-prefix-p "message}" item)
                              (prog1
                                  (let* ((subject (or subject (plist-get headers :Subject)))
                                         (date  (or date (plist-get headers :Date)))
                                         (senders (or senders (plist-get headers :From)))
                                         (cc  (or cc (plist-get headers :Cc)))
                                         (to  (or to (plist-get headers :To)))
                                         (decorated (consult-omni--notmuch-format-candidate :source source :query query :title subject :from senders :date date :tags tags)))
                                    (when (and (stringp decorated) (not (string-empty-p decorated)))
                                      (propertize decorated
                                                  :source source
                                                  :query query
                                                  :title subject
                                                  :url nil
                                                  :search-url nil
                                                  :id id
                                                  :from senders
                                                  :to to
                                                  :cc cc
                                                  :date date
                                                  :match match
                                                  :headers headers
                                                  :count count
                                                  :tags tags)))
                                (setq id nil
                                      headers nil
                                      senders nil
                                      subject nil
                                      to nil
                                      cc nil
                                      count nil
                                      date nil
                                      tags nil
                                      match nil
                                      info nil))
                            (cond ((string-match "message{ \\(id:[^ ]+\\) .+" item)
                                   (setq id (match-string 1 item))
                                   (setq match t))
                                  ((string-prefix-p "header{" item)
                                   (setq info t))
                                  ((and item info)
                                   (cond ((string-match "\\(.+\\) (\\([^)]+\\)) (\\([^)]*\\))$" item)
                                          (setq senders (match-string 1 item))
                                          (setq date (match-string 2 item))
                                          (setq tags (split-string (match-string 3 item))))
                                         ((string-match "\\(Subject\\|From\\|To\\|Cc\\|Date\\): \\(.+\\)?" item)
                                          (let ((k (intern (format ":%s" (match-string 1 item))))
                                                (v (or (match-string 2 item) "")))
                                            (setq headers
                                                  (plist-put headers k v))))))
                                  ((string-prefix-p "header}" item)
                                   (setq info nil)))
                            nil))
                        candidates))))

(defun consult-omni--notmuch-get-transform-func (&rest _args)
  "Get the appropriate transform function for notmuch commands with ARGS.

This is needed to get the right function for
parsing outputs of “notmuch search”, and
“notmuch show” accordingly."
  (cdr (assoc consult-omni-notmuch-default-command-arg consult-omni--notmuch-format-func-alist)))

(defun consult-omni--notmuch--preview (cand)
  "Preview function for CAND from `consult-omni-notmuch'."
  (let* ((query (get-text-property 0 :query cand))
         (id (get-text-property 0 :id cand)))
    (when id
      (when (get-buffer consult-omni-notmuch-message-buffer-name)
        (kill-buffer consult-omni-notmuch-message-buffer-name))
      (notmuch-show id nil nil query consult-omni-notmuch-message-buffer-name))))

(defun consult-omni--notmuch-callback (cand)
  "Callback function for CAND from `consult-omni-notmuch'."
  (let* ((query (get-text-property 0 :query cand))
         (id (get-text-property 0 :id cand)))
    (when id
      (when (get-buffer consult-omni-notmuch-message-buffer-name)
        (kill-buffer consult-omni-notmuch-message-buffer-name))
      (notmuch-tree query nil id consult-omni-notmuch-tree-buffer-name t nil nil nil))))

(cl-defun consult-omni--notmuch-command-builder (input &rest args &key callback &allow-other-keys)
  "Make builder command line args for “notmuch” with INPUT and ARGS.

CALLBACK is a function used internally to update the list of candidates in
the minibuffer asynchronously.  It is called with a list of strings, which
are new annotated candidates \(e.g. as they arrive from an asynchronous
process\) to be added to the minibuffer completion cnadidates.  See the
section on REQUEST in documentation for `consult-omni-define-source' as
well as the function
`consult-omni--multi-update-dynamic-candidates' for how CALLBACK is used."
  (setq consult-notmuch--partial-parse nil)
  (pcase-let* ((`(,query . ,opts) (consult-omni--split-command input (seq-difference args (list :callback callback))))
               (opts (car-safe opts))
               (count (plist-get opts :count))
               (page (plist-get opts :page))
               (count (or (and count (integerp (read count)) (string-to-number count))
                          consult-omni-notmuch-default-count))
               (page (or (and page (integerp (read page)) (string-to-number page))
                         consult-omni-default-page))
               (page (* count page))
               (consult-omni-notmuch-extra-command-args (unless (listp consult-omni-notmuch-extra-command-args) (list consult-omni-notmuch-extra-command-args)))
               (cmd (append (list notmuch-command) (list consult-omni-notmuch-default-command-arg) (when count (list "--limit" (format "%s" count))) (when (and page (not (equal page 0))) (list "--offset" (format "%s" page))) consult-omni-notmuch-extra-command-args (list query))))
    cmd))

;; Define the notmuch source
(consult-omni-define-source "notmuch"
                            :narrow-char ?m
                            :type 'async
                            :require-match nil
                            :category 'notmuch-result
                            :face 'consult-omni-engine-title-face
                            :request #'consult-omni--notmuch-command-builder
                            :on-preview #'consult-omni--notmuch--preview
                            :on-return #'identity
                            :on-callback #'consult-omni--notmuch-callback
                            :preview-key consult-omni-preview-key
                            :search-hist 'consult-omni--search-history
                            :select-hist 'consult-omni--email-select-history
                            :group #'consult-omni--group-function
                            :sort t
                            :interactive consult-omni-intereactive-commands-type
                            :transform (lambda (candidates &optional query) (funcall (consult-omni--notmuch-get-transform-func) candidates query))
                            :enabled (lambda () (and (bound-and-true-p notmuch-command)
                                                     (executable-find notmuch-command)))
                            :annotate nil)

;;; provide `consult-omni-notmuch' module

(provide 'consult-omni-notmuch)

(add-to-list 'consult-omni-sources-modules-to-load 'consult-omni-notmuch)
;;; consult-omni-notmuch.el ends here
