;;; consult-omni-grep.el --- Consulting Grep Command -*- lexical-binding: t -*-

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
;; consult-omni-grep provides commands for running “grep” shell commands
;; similar to consult-grep but using consult-omni.

;;; Code:

(require 'consult-omni)

(cl-defun consult-omni--grep-format (candidates &rest args &key source query regexp-pattern)
  "Format CANDIDATES for grep based commands with ARGS.

Description of Arguments:
  SOURCE         a string; the source name \(e.g. “grep”\)
  QUERY          a string; query input from the user
  REGEXP-PATTERN a string; regexp to match file and line of grep output
                 \(for an example, see `consult--grep-match-regexp'\)

Adopted from `consult--grep-format'."
  (let* ((frame-width-percent (floor (* (frame-width) 0.1)))
         (file "")
         (file-len 0)
         result)
    (dolist (str candidates)
      (when (and (string-match regexp-pattern str)
                 ;; Filter out empty context lines
                 (or (/= (aref str (match-beginning 3)) ?-)
                     (/= (match-end 0) (length str))))
        ;; We share the file name across candidates to reduce
        ;; the amount of allocated memory.
        (unless (and (= file-len (- (match-end 1) (match-beginning 1)))
                     (eq t (compare-strings
                            file 0 file-len
                            str (match-beginning 1) (match-end 1) nil)))
          (setq file (match-string 1 str)
                file-len (length file)))
        (let* ((line (match-string 2 str))
               (ctx (= (aref str (match-beginning 3)) ?-))
               (sep (if ctx "-" ":"))
               (content (substring str (match-end 0)))
               (line-len (length line))
               (cand)
               (file-str (string-remove-prefix (file-truename default-directory) (file-truename file)))
               (file-str (if (> (length file-str) (* frame-width-percent 5))
                             (consult-omni--set-string-width file-str (* frame-width-percent 5) (* frame-width-percent 1))
                           file-str))
               (file-str-len (length file-str)))
          (when (length> content consult-grep-max-columns)
            (setq content  (consult-omni--set-string-width content consult-grep-max-columns)))
          (setq cand (concat file-str sep line sep content))
          ;; Store file name in order to avoid allocations in `consult--prefix-group'
          (add-text-properties 0 1 `(:source ,source :title ,cand :query ,query :file ,file :pos ,line :content ,content) cand)
          (add-text-properties 0 file-str-len `(face consult-file consult--prefix-group ,file) cand)
          (put-text-property (1+ file-str-len) (+ 1 file-str-len line-len) 'face 'consult-line-number cand)
          (when ctx
            (add-face-text-property (+ 2 file-str-len line-len) (length cand) 'consult-grep-context 'append cand))
          (push cand result))))
    result))

(defun consult-omni--grep-transform (candidates &optional query)
  "Format CANDIDATES for `consult-omni-grep' from QUERY."
  (consult-omni--grep-format candidates :source "grep" :query query :regexp-pattern consult--grep-match-regexp))

(defun consult-omni--grep-make-builder (make-builder &optional dir)
  "Build command line for grep and similar processes.

Description of Arguments:
  MAKE-BUILDER a function; takes one argument, PATHS, and builds grep
               command line across PATHS.
  DIR          a string; parent directory to use for getting PATHS"
  (pcase-let* ((`(_ ,paths ,dir) (consult--directory-prompt "" dir))
               (paths (if dir
                          (mapcar (lambda (path) (file-truename (concat dir path))) paths)
                        paths)))
    (funcall make-builder paths)))

(defun consult-omni--grep-preview (cand)
  "Preview function for CAND from `consult-omni-grep'."
  (let ((file (get-text-property 0 :file cand))
        (pos (get-text-property 0 :pos cand))
        (query (get-text-property 0 :query cand)))
    (when file
      (with-current-buffer (funcall #'consult--file-action file)
        (when (stringp pos) (forward-line (- (string-to-number pos) (line-number-at-pos))))
        (consult--invisible-open-permanently)
        (recenter nil t)
        (when consult-omni-highlight-matches-in-file
          (consult-omni--overlay-match query nil consult-omni-highlight-match-ignore-case)
          (add-to-history 'search-ring (isearch-string-propertize query)))
        (consult-omni--pulse-line))
      nil)))

(cl-defun consult-omni--grep-builder (input &rest args &key callback &allow-other-keys)
  "Make builder command line args for “grep” with INPUT and ARGS.

CALLBACK is a function used internally to update the list of candidates in
the minibuffer asynchronously.  It is called with a list of strings, which
are new annotated candidates \(e.g. as they arrive from an asynchronous
process\) to be added to the minibuffer completion cnadidates.  See the
section on REQUEST in documentation for `consult-omni-define-source' as
well as the function
`consult-omni--multi-update-dynamic-candidates' for how CALLBACK is used."
  (pcase-let* ((`(,query . ,opts) (consult-omni--split-command input (seq-difference args (list :callback callback))))
               (opts (car-safe opts))
               (dir (plist-get opts :dir))
               (dir (if dir (file-truename (format "%s" dir))))
               (default-directory (or dir default-directory)))
    (funcall (consult-omni--grep-make-builder #'consult--grep-make-builder dir) query)))

;; Define the grep Source
(consult-omni-define-source "grep"
                            :narrow-char ?r
                            :type 'async
                            :require-match t
                            :face 'consult-omni-engine-title-face
                            :request #'consult-omni--grep-builder
                            :transform #'consult-omni--grep-transform
                            :on-preview #'consult-omni--grep-preview
                            :on-return #'identity
                            :on-callback #'consult-omni--grep-preview
                            :preview-key consult-omni-preview-key
                            :search-hist 'consult-omni--search-history
                            :select-hist 'consult-omni--selection-history
                            :group #'consult-omni--group-function
                            :sort nil
                            :interactive consult-omni-intereactive-commands-type
                            :transform #'consult-omni--ripgrep-transform
                            :enabled (lambda () (and (executable-find "grep")
                                                (fboundp 'consult-grep)
                                                t))
                            :annotate nil)

;;; provide `consult-omni-grep' module

(provide 'consult-omni-grep)

(add-to-list 'consult-omni-sources-modules-to-load 'consult-omni-grep)
;;; consult-omni-grep.el ends here
