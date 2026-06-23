;;; consult-omni-elfeed.el --- Consulting Elfeed -*- lexical-binding: t -*-

;; Copyright (C) 2024 Armin Darvish

;; Author: Armin Darvish
;; Maintainer: Armin Darvish
;; Created: 2024
;; Version: 0.3
;; Package-Requires: (
;;         (emacs "29.4")
;;         (consult "2.0")
;;         (elfeed "3.4.1")
;;         (consult-omni "0.3"))
;;
;; Homepage: https://github.com/armindarvish/consult-omni/blob/main/consult-omni-sources
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
;; consult-omni-elfeed enables searching Elfeed's feeds in consult-omni.
;; It provides commands to search feeds and getting the results
;; directly in the minibuffer.
;;
;; For more info on Elfeed see:
;; URL `https://github.com/skeeto/elfeed'

;;; Code:

(require 'elfeed)
(require 'consult-omni)

;;; User Options (a.k.a. Custom Variables)

(defcustom consult-omni-elfeed-search-buffer-name "*consult-omni-elfeed-search*"
  "Name for `consult-omni-elfeed-search' buffer."
  :group 'consult-omni
  :type 'string)

(defcustom consult-omni-elfeed-default-filter nil
  "Default Filter for `consult-omni-elfeed-search'."
  :group 'consult-omni
  :type 'string)

(defun consult-omni--elfeed-format-candidate (entries query)
  "Format the ENTRIES for QUERY from `consult-omni-elfeed'.

Description of Arguments:

  ENTRIES a list; list of entries from `consult-omni--elfeed-fetch-result'.
  QUERY   a string; the query input from the user"
  (let ((annotated-entries))
    (dolist (entry entries annotated-entries)
      (let* ((url (elfeed-entry-link entry))
             (urlobj (if url (url-generic-parse-url url)))
             (domain (if (url-p urlobj) (url-domain urlobj)))
             (domain (if (stringp domain) (propertize domain 'face 'consult-omni-domain-face)))
             (path (if (url-p urlobj) (url-filename urlobj)))
             (path (if (stringp path) (propertize path 'face 'consult-omni-path-face)))
             (title (or (elfeed-entry-title entry) ""))
             (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
             (feed (elfeed-entry-feed entry))
             (feed-title (when feed (elfeed-feed-title feed)))
             (date (format-time-string "%Y-%m-%d %H:%M" (elfeed-entry-date entry)))
             (id (elfeed-entry-id entry))
             (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
             (tags-str (mapconcat
                        (lambda (s) (propertize s 'face 'elfeed-search-tag-face))
                        tags ","))
             (title-width (- (floor (* (frame-width) 0.7)) elfeed-search-trailing-width))
             (title-column (elfeed-format-column
                            title (elfeed-clamp
                                   elfeed-search-title-min-width
                                   title-width
                                   elfeed-search-title-max-width)
                            :left))
             (match-str (if (and (stringp query) (not (equal query ".*"))) (consult--split-escaped (car (consult--command-split query))) nil))
             (str (concat (propertize title-column 'face title-faces 'kbd-help title) " "
                          (propertize date 'face 'elfeed-search-date-face)
                          (when feed-title
                            (concat " " (propertize feed-title 'face 'elfeed-search-feed-face)))
                          (when tags (concat " " "(" tags-str ")"))
                          (when domain (concat "\t" domain (when path path)))
                          (concat "\t" (propertize "elfeed" 'face 'consult-omni-source-type-face)))))
        (if consult-omni-highlight-matches-in-minibuffer
            (cond
             ((listp match-str)
              (mapc (lambda (match) (setq str (consult-omni--highlight-match match str t))) match-str))
             ((stringp match-str)
              (setq str (consult-omni--highlight-match match-str str t)))))
        (push (propertize str
                          :source "elfeed"
                          :title title
                          :url url
                          :search-url nil
                          :query query
                          :entry entry
                          :tags tags
                          :date date
                          :id id
                          :feed feed)
              annotated-entries)))))

(defun consult-omni--elfeed-search-buffer ()
  "Get or create buffer for `consult-omni-elfeed'."
  (get-buffer-create (or consult-omni-elfeed-search-buffer-name "*consult-omni-elfeed-search*")))

(defun consult-omni--elfeed-preview (cand)
  "Preview function for CAND from `consult-omni-elfeed'.

Uses `elfeed-show-entry'."
  (if (listp cand) (setq cand (or (car-safe cand) cand)))
  (let* ((entry (get-text-property 0 :entry cand))
         (buff (get-buffer-create (elfeed-show--buffer-name entry))))
    (with-current-buffer buff
      (elfeed-show-mode)
      (setq elfeed-show-entry entry)
      (elfeed-show-refresh))
    (funcall (consult--buffer-preview) 'preview
             buff)))

(cl-defun consult-omni--elfeed-fetch-results (input &rest args &key callback &allow-other-keys)
  "Return entries matching INPUT in elfeed database with ARGS.

Use INPUT as filter to find entries in elfeed database.  If
`consult-omni-elfeed-default-filter' is non-nil, it is used as additional
filter parameters.

CALLBACK is a function used internally to update the list of candidates in
the minibuffer asynchronously.  It is called with a list of strings, which
are new annotated candidates \(e.g. as they arrive from an asynchronous
process\) to be added to the minibuffer completion cnadidates.  See the
section on REQUEST in documentation for `consult-omni-define-source' as
well as the function
`consult-omni--multi-update-dynamic-candidates' for how CALLBACK is used."
  (cl-letf* (((symbol-function #'elfeed-search-buffer) #'consult-omni--elfeed-search-buffer))
    (pcase-let* ((`(,query . ,opts) (consult-omni--split-command input (seq-difference args (list :callback callback))))
                 (opts (car-safe opts))
                 (maxcount (plist-get opts :count))
                 (filter (and (plist-member opts :filter) (plist-get opts :filter)))
                 (maxcount (or (and (integerp maxcount) maxcount)
                               (and maxcount (string-to-number (format "%s" maxcount)))
                               consult-omni-default-count))
                 (elfeed-search-filter (concat (if maxcount (format "#%d " maxcount))
                                               (if filter (format "%s" filter)
                                                 consult-omni-elfeed-default-filter)
                                               (if query (format "%s" query))))
                 (filter (elfeed-search-parse-filter elfeed-search-filter))
                 (head (list nil))
                 (tail head)
                 (count 0)
                 (lexical-binding t)
                 (search-func (byte-compile (elfeed-search-compile-filter filter))))
      (with-elfeed-db-visit (entry feed)
        (when (funcall search-func entry feed count)
          (setf (cdr tail) (list entry)
                tail (cdr tail)
                count (1+ count))))
      (when-let* ((entries (cdr head)))
        (consult-omni--elfeed-format-candidate entries query)))))

;; Define the elfeed source
(consult-omni-define-source "elfeed"
                            :narrow-char ?e
                            :type 'sync
                            :require-match t
                            :face 'elfeed-search-unread-title-face
                            :request #'consult-omni--elfeed-fetch-results
                            :on-preview #'consult-omni--elfeed-preview
                            :on-return #'identity
                            :on-callback #'consult-omni--elfeed-preview
                            :preview-key consult-omni-preview-key
                            :search-hist 'consult-omni--search-history
                            :select-hist 'consult-omni--selection-history
                            :enabled (lambda () (boundp 'elfeed-db))
                            :group #'consult-omni--group-function
                            :sort t
                            :interactive consult-omni-intereactive-commands-type
                            :annotate nil)

;;; provide `consult-omni-elfeed' module

(provide 'consult-omni-elfeed)

(add-to-list 'consult-omni-sources-modules-to-load 'consult-omni-elfeed)
;;; consult-omni-elfeed.el ends here
