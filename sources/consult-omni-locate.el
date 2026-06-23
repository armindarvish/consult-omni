;;; consult-omni-locate.el --- Consulting Locate Command -*- lexical-binding: t -*-

;; Copyright (C) 2024 Armin Darvish

;; Author: Armin Darvish
;; Maintainer: Armin Darvish
;; Created: 2024
;; Version: 0.3
;; Package-Requires: (
;;         (emacs "29.4")
;;         (consult "2.0")
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
;; consult-omni-locate provides commands for running “locate” shell
;; commands similar to consult-locate but using consult-omni.


;;; Code:

(require 'consult-omni)

;;; User Options (a.k.a. Custom Variables)

(defcustom consult-omni-locate-limit consult-omni-default-count
  "Max number results for `consult-omni-locate'.

This is passed to “-l” command line argument."
  :group 'consult-omni
  :type 'integer)

(defcustom consult-omni-locate-args "locate -i"
  "Command line arguments for locate.

Similar to `consult-locate-args' bur for consult-omni."
  :group 'consult-omni
  :type '(choice string (repeat (choice string sexp))))

(defun consult-omni--locate-preview (cand)
  "Preview function for CAND from `consult-omni-locate'."
  (funcall (consult--file-preview) 'preview cand))

(defun consult-omni--locate-callback (cand)
  "Callback for CAND from `consult-omni-locate'."
  (consult--file-action cand))

(defun consult-omni--locate-transform (candidates &optional _query)
  "Format CANDIDATES of `consult-omni-locate'."
  (mapcar (lambda (candidate)
            (string-remove-prefix (file-truename default-directory)
                                  candidate))
          candidates))

(defun consult-omni--locate-filter (candidates &optional _query)
  "Filter CANDIDATES for `consult-omni-locate'."
  (seq-filter (lambda (candidate)
                (not (string-match "^locate:.*$" candidate nil nil)))
              candidates))

(cl-defun consult-omni--locate-builder (input &rest args &key callback &allow-other-keys)
  "Make builder command line args for “locate” with INPUT and ARGS.

CALLBACK is a function used internally to update the list of candidates in
the minibuffer asynchronously.  It is called with a list of strings, which
are new annotated candidates \(e.g. as they arrive from an asynchronous
process\) to be added to the minibuffer completion cnadidates.  See the
section on REQUEST in documentation for `consult-omni-define-source' as
well as the function
`consult-omni--multi-update-dynamic-candidates' for how CALLBACK is used."
  (pcase-let* ((`(,query . ,opts) (consult-omni--split-command input (seq-difference args (list :callback callback))))
               (opts (car-safe opts))
               (count (plist-get opts :count))
               (dir (plist-get opts :dir))
               (dir (if dir (file-truename (format "%s" dir))))
               (count (or (and (integerp count) count)
                          (and count (string-to-number (format "%s" count)))
                          consult-omni-locate-limit))
               (default-directory (or dir default-directory))
               (consult-locate-args (concat consult-omni-locate-args
                                            (if count (format " -l %s" count)))))
    (funcall #'consult--locate-builder query)))

;; Define the locate source
(consult-omni-define-source "locate"
                            :narrow-char ?f
                            :category 'file
                            :type 'async
                            :require-match t
                            :face 'consult-omni-engine-title-face
                            :request #'consult-omni--locate-builder
                            :filter #'consult-omni--locate-filter
                            :on-preview #'consult-omni--locate-preview
                            :on-return #'identity
                            :on-callback #'consult-omni--locate-callback
                            :preview-key consult-omni-preview-key
                            :search-hist 'consult-omni--search-history
                            :select-hist 'consult-omni--selection-history
                            :group #'consult-omni--group-function
                            :enabled (lambda () (executable-find "locate"))
                            :sort t
                            :interactive consult-omni-intereactive-commands-type
                            :annotate nil)

;;; provide `consult-omni-locate' module

(provide 'consult-omni-locate)

(add-to-list 'consult-omni-sources-modules-to-load 'consult-omni-locate)
;;; consult-omni-locate.el ends here
