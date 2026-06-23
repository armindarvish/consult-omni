;;; consult-omni-gh.el --- Consulting Github Client -*- lexical-binding: t -*-

;; Copyright (C) 2024 Armin Darvish

;; Author: Armin Darvish
;; Maintainer: Armin Darvish
;; Created: 2024
;; Version: 0.3
;; Package-Requires: (
;;         (emacs "29.4")
;;         (consult "2.0")
;;         (consult-gh "2.0")
;;         (consult-omni "0.3"))
;;
;; Homepage: https://github.com/armindarvish/consult-omni
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
;; consult-omni-gh enables using consult-gh in consult-omni.
;; It provides commands to search GitHub repositories using consult-gh as ;; the backend.
;;
;; For more info on consult-gh see:
;; URL `https://github.com/armindarvish/consult-gh'

;;; Code:

(require 'consult-omni)
(require 'consult-gh)

(defun consult-omni--gh-transform (items &optional query)
  "Transform consult-gh ITEMS for QUERY to consult-omni style."
  (remove nil (mapcar (lambda (string)
                        (consult-gh--repo-format string (or query "") t))
                      items)))

(defun consult-omni--gh-preview (cand)
  "Preview function for CAND from `consult-omni-github'."
  (when-let* ((info (text-properties-at 0 (cdr (get-text-property 0 'multi-category cand))))
             (repo (plist-get info :repo))
             (query (plist-get info :query))
             (match-str (consult--build-args query))
             (buffer (get-buffer-create consult-gh-preview-buffer-name)))
    (add-to-list 'consult-gh--preview-buffers-list buffer)
    (consult-gh--repo-view (format "%s" repo) buffer)
    (with-current-buffer buffer
      (if consult-gh-highlight-matches
          (cond
           ((listp match-str)
            (mapc (lambda (item)
                    (highlight-regexp item 'consult-gh-preview-match-face))
                  match-str))
           ((stringp match-str)
            (highlight-regexp match-str 'consult-gh-preview-match-face)))))
    (funcall (consult--buffer-preview) 'preview
             buffer)))

(defun consult-omni--gh-callback (cand)
  "Callback function for CAND from `consult-omni-github'."
  (funcall consult-gh-repo-action (cdr (get-text-property 0 'multi-category cand))))

(cl-defun consult-omni--gh-search-repos-builder (input &rest args &key callback &allow-other-keys)
  "Make builder command line args for “GitHub CLI” with INPUT and ARGS.

CALLBACK is a function used internally to update the list of candidates in
the minibuffer asynchronously.  It is called with a list of strings, which
are new annotated candidates \(e.g. as they arrive from an asynchronous
process\) to be added to the minibuffer completion cnadidates.  See the
section on REQUEST in documentation for `consult-omni-define-source' as
well as the function
`consult-omni--multi-update-dynamic-candidates' for how CALLBACK is used."
  (pcase-let* ((`(,query . ,opts) (consult-omni--split-command input (if callback (seq-difference args (list :callback callback)) args)))
               (opts (car-safe opts))
               (count (plist-get opts :count))
               (count (or (and count (integerp (read count)) (string-to-number count))
                          consult-omni-default-count))
               (cmd (consult--build-args '("gh" "search" "repos")))
               (cmd-opts (list "--limit" (format "%s" count)))
               (`(,re . ,hl) (funcall consult--regexp-compiler query 'basic t)))
    (when re
      (cons (append cmd
                    (list (string-join re " "))
                    cmd-opts)
            hl))))

;; Define the GitHub source
(consult-omni-define-source "GitHub"
                            :narrow-char ?h
                            :type 'async
                            :require-match nil
                            :category 'consult-gh-repos
                            :face 'consult-omni-engine-title-face
                            :request #'consult-omni--gh-search-repos-builder
                            :on-preview #'consult-omni--gh-preview
                            :on-return #'identity
                            :on-callback #'consult-omni--gh-callback
                            :preview-key consult-omni-preview-key
                            :search-hist 'consult-omni--search-history
                            :select-hist 'consult-omni--selection-history
                            :group #'consult-omni--group-function
                            :sort t
                            :interactive consult-omni-intereactive-commands-type
                            :transform #'consult-omni--gh-transform
                            :enabled (lambda () (and (executable-find "gh")
                                                     (fboundp 'consult-gh-search-repos)))
                            :annotate nil)

;;; provide `consult-omni-gh' module

(provide 'consult-omni-gh)

(add-to-list 'consult-omni-sources-modules-to-load 'consult-omni-gh)
;;; consult-omni-gh.el ends here
