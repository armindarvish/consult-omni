;;; consult-omni-notmuch.el --- Consulting Notmuch Command -*- lexical-binding: t -*-

;; Copyright (C) 2024 Armin Darvish

;; Author: Armin Darvish
;; Maintainer: Armin Darvish
;; Created: 2024
;; Version: 0.1
;; Package-Requires: ((emacs "28.1") (consult "1.4") (consult-omni "0.1") (consult-notmuch "0.8.1"))
;; Homepage: https://github.com/armindarvish/consult-omni
;; Keywords: convenience

;;; Commentary:

;;; Code:

(require 'consult-omni)
(require 'notmuch)

(defcustom consult-omni-notmuch-message-buffer-name "*consult-omni-notmuch-message*"
  "Name of notmuch preview and result buffers."
  :type 'string)

(defcustom consult-omni-notmuch-tree-buffer-name "*consult-omni-notmuch-tree*"
  "Name of notmuch preview and result buffers."
  :type 'string)

(defcustom consult-omni-notmuch-command (or notmuch-command "notmuch")
  "Name of the notmuch binary.

By default inherits from `notmuch-command'."
  :type 'string)

(defcustom consult-omni-notmuch-default-command-arg "search"
  "Default notmuch commandline arg for finding messages.
Can be either “search” or “show”"
  :type  '(choice (const :tag "(Default) search" "search")
                  (const :tag "show" "show"))
  )

(defcustom consult-omni-notmuch-extra-command-args (list)
"extra notmuch commandline arguments."

 :type '(repeat (choice string)))

(defcustom consult-omni-notmuch-default-count consult-omni-default-count
   "Number of notmuch search results to retrieve."
 :type 'integer)

(setq consult-omni--notmuch-format-func-alist '(("show" . consult-omni--notmuch-show-transform)
                                                  ("search" . consult-omni--notmuch-search-transform)))

(cl-defun consult-omni--notmuch-format-candidate (&rest args &key source query title from date tags face &allow-other-keys)
  "Transform STR from notmuch search to consult-omni-notmuch style."
  (let* ((frame-width-percent (floor (* (frame-width) 0.1)))
         (source (if (stringp source) (propertize source 'face 'consult-omni-source-type-face) nil))
         (match-str (if (stringp query) (consult--split-escaped query) nil))
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
    (if consult-omni-highlight-matches
        (cond
         ((and (listp match-str) (stringp str))
          (mapcar (lambda (match) (setq str (consult-omni--highlight-match match str t))) match-str))
         ((and (stringp match-str) (stringp str))
          (setq str (consult-omni--highlight-match match-str str t)))))
     str))

(defun consult-omni--notmuch-search-transform (candidates &optional query)

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
                            (when (and (stringp decorated) (not (string-empty-p decorated))) (propertize decorated
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
                                        :tags tags
                                        )))))
                      candidates)
          )))

(defun consult-omni--notmuch-show-transform (candidates &optional query)
  "Parse output STR of notmuch show, extracting its components."

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
                                                :date date
                                                :match t
                                                :headers headers
                                                :count count
                                                :tags tags
                                                )))
                                (setq id nil
                                      headers nil
                                      senders nil
                                      subject nil
                                      count nil
                                      date nil
                                      tags nil
                                      match nil
                                      info nil
                                      ))
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
                                                  (plist-put headers k v))))
                                         )
                                   )
                                  ((string-prefix-p "header}" item)
                                   (setq info nil))
                                  )
                            nil)) candidates))))

(defun consult-omni--notmuch-get-transform-func (&rest args)
    (cdr (assoc consult-omni-notmuch-default-command-arg consult-omni--notmuch-format-func-alist)))

(defun consult-omni--notmuch--preview (cand)
  "Preview function for notmuch candidates."
  (let* ((query (get-text-property 0 :query cand))
         (id (get-text-property 0 :id cand)))
    (when id
      (when (get-buffer consult-notmuch--buffer-name)
        (kill-buffer consult-notmuch--buffer-name))
      (notmuch-show id nil nil query consult-omni-notmuch-message-buffer-name))))

(defun consult-omni--notmuch-callback (cand)
  "Callback function for notmuch candidates.
"
  (let* ((query (get-text-property 0 :query cand))
         (id (get-text-property 0 :id cand)))
    (when id
      (when (get-buffer consult-omni-notmuch--buffer-name)
        (kill-buffer consult-omni-notmuch--buffer-name))
      (notmuch-tree query nil id consult-omni-notmuch-tree-buffer-name t nil nil nil)))
)

(cl-defun consult-omni--notmuch-command-builder (input &rest args &key callback &allow-other-keys)
  "makes builder command line args for “notmuch”.
"
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
               (cmd (append (list notmuch-command) (list consult-omni-notmuch-default-command-arg) (when count (list "--limit" (format "%s" count))) (when (and page (not (equal page 0))) (list "--offset" (format "%s" page)))  consult-omni-notmuch-extra-command-args (list query)))
               )
    cmd
  ))

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
                           :select-hist 'consult-omni--selection-history
                           :group #'consult-omni--group-function
                           :sort t
                           :static 'both
                           :transform (lambda (candidates &optional query) (funcall (consult-omni--notmuch-get-transform-func) candidates query))
                           :enabled (lambda () (if (and (bound-and-true-p notmuch-command) (executable-find notmuch-command))
                                                   t nil))
                           :annotate nil
                           )

;;; provide `consult-omni-notmuch' module

(provide 'consult-omni-notmuch)

(add-to-list 'consult-omni-sources-modules-to-load 'consult-omni-notmuch)
;;; consult-omni-notmuch.el ends here
