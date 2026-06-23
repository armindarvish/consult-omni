;;; consult-omni-line-multi.el --- Search Lines in All Buffers  -*- lexical-binding: t -*-

;; Copyright (C) 2024 Armin Darvish

;; Author: Armin Darvish
;; Maintainer: Armin Darvish
;; Created: 2024
;; Version: 0.3
;; Package-Requires: (
;;         (emacs "29.4")
;;         (consult "2.0")
;;         (consult-omni "0.3"))
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
;; consult-omni-line-multi provides commands for searching lines in
;; multiple buffers similar to consult-line-multi but using consult-omni.

;;; Code:

(require 'consult)
(require 'consult-omni)

(defun consult-omni--line-multi-preview (cand)
  "Preview function for CAND from `consult-omni-line-multi'."
  (let* ((marker (car (get-text-property 0 :marker cand))))
    (consult--jump marker)))

(cl-defun consult-omni--line-multi-format-candidate (&rest args &key source query marker title face &allow-other-keys)
  "Format the candidates of `consult-omni-line-multi' with ARGS.

Description of Arguments:

  SOURCE     the source name to use (e.g. “buffers text search”)
  QUERY      query input from the user
  MARKER     the marker pointing to results of line multi search
  TITLE      the title of the candidate (e.g. response from chatgpt)
  FACE       the face to apply to TITLE"
  (let* ((frame-width-percent (floor (* (frame-width) 0.1)))
         (source (if (stringp source) (propertize source 'face 'consult-omni-source-type-face)))
         (marker (car marker))
         (buff (marker-buffer marker))
         (pos (marker-position marker))
         (buff (and buff (propertize (format "%s" buff) 'face 'consult-omni-domain-face)))
         (pos (and pos (propertize (format "%s" pos) 'face 'consult-omni-path-face)))
         (match-str (if (and (stringp query) (not (equal query ".*")))
                        (consult--split-escaped (car (consult--command-split query)))
                      nil))
         (face (or (consult-omni--get-source-prop source :face) face 'consult-omni-default-face))
         (title-str (propertize title 'face face))
         (title-str (consult-omni--set-string-width title-str (* 6 frame-width-percent)))
         (str (concat title-str
                      (when buff (concat "\t" buff))
                      (when pos (concat "\s\s" pos ))
                      (when source (concat "\t" source)))))
    (if consult-omni-highlight-matches-in-minibuffer
        (cond
         ((listp match-str)
          (mapc (lambda (match) (setq str (consult-omni--highlight-match match str t))) match-str))
         ((stringp match-str)
          (setq str (consult-omni--highlight-match match-str str t)))))
    str))

(cl-defun consult-omni--line-multi-fetch-results (input &rest args &key callback &allow-other-keys)
  "Fetch search results for INPUT from `consult-line-multi' with ARGS.

CALLBACK is a function used internally to update the list of candidates in
the minibuffer asynchronously.  It is called with a list of strings, which
are new annotated candidates \(e.g. as they arrive from an asynchronous
process\) to be added to the minibuffer completion cnadidates.  See the
section on REQUEST in documentation for `consult-omni-define-source' as
well as the function
`consult-omni--multi-update-dynamic-candidates' for how CALLBACK is used."
  (pcase-let* ((`(,query . _) (consult-omni--split-command input (seq-difference args (list :callback callback))))
               (buffers (consult--buffer-query :directory (consult--normalize-directory default-directory) :sort 'alpha-current)))
     (consult--line-multi-candidates buffers input (lambda (items)
                                                    (funcall callback
                                                     (mapcar (lambda (item)
                                                     (let* ((source "buffers text search")
                                                            (marker  (consult--get-location item))
                                                            (title (substring-no-properties item 0 -1))
                                                            (decorated (consult-omni--line-multi-format-candidate :source source :query query :marker marker :title title)))
                                              (propertize decorated
                                                          :source source
                                                          :title title
                                                          :url nil
                                                          :marker marker
                                                          :query query)))
                                                             items))))))

;; Define the Buffers Text Search Source
(consult-omni-define-source "buffers text search"
                            :narrow-char ?s
                            :type 'dynamic
                            :require-match t
                            :category 'consult-location
                            :face 'default
                            :request #'consult-omni--line-multi-fetch-results
                            :preview-key consult-preview-key
                            :search-hist 'consult-omni--search-history
                            :select-hist 'consult-omni--selection-history
                            :on-preview #'consult-omni--line-multi-preview
                            :on-return #'identity
                            :on-callback #'consult-omni--line-multi-preview
                            :enabled (lambda () (fboundp 'consult--line-multi-candidates))
                            :group #'consult-omni--group-function
                            :sort t
                            :interactive consult-omni-intereactive-commands-type
                            :annotate nil)

;;; provide `consult-omni-line-multi' module

(provide 'consult-omni-line-multi)

(add-to-list 'consult-omni-sources-modules-to-load 'consult-omni-line-multi)
;;; consult-omni-line-multi.el ends here
