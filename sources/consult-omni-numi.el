;;; consult-omni-numi.el --- Consulting numi Command -*- lexical-binding: t -*-

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
;; consult-omni-numi provides commands for doing calculations directly in
;; Emacs minibuffer using consult-omni as the frontend and numi-cli shell
;; commands as the backend.
;;
;; For more info on nuumi and numi-cli see the following URLs:
;; URL `https://numi.app'
;; URL `https://github.com/nikolaeu/numi'

;;; Code:

(require 'consult-omni)

(defcustom consult-omni-numi-args "numi-cli"
  "Command line arguments for “numi-cli”.

Similar to other command line args for consult but for numi-cli.
See `consult-locate-args' for example."
  :group 'consult-omni
  :type '(choice string (repeat (choice string sexp))))

(defcustom consult-omni-numi-regexp-pattern "^=\\(.*\\)?"
  "Regexp to detect mathematical formula?

The first capturing group will be used as input for `consult-omni-numi-args'.
If there is no capture group, the whole input query is used.

Note that the default setting is simply any string with a leading “=”.
This allows the user to see the results from Numi calculator \(i.e.
`consult-omni--numi-fetch-results')\ with any string without consult-omni
detecting (i.e. trying to guess) what is a mathematical equation.  This
can specially be useful in multi-source searches, if the user does not
want to see random results from the Numi calculator on every search string.
The downside of this approach is that the user has to type “=” every time
before seeing results form the Numi calculator.  For example to see the
result of the equation 2+3, the user has to type:
  “#=2+3”
\(Note that the leading “#” above is from the default perl style of
`consult-async-split-style' and otherwise not neccessary\).

Alternatively, one can change this variable to regexp pattern that
detects/guesses a mathematical equation \(for example by looking for
strings that contain digits and/or mathematical operators\).  For an
example, see the default choices for this custom variable.  This would
remove the need to type a leading character every time but at the same
time may miss some edge cases if the user's query/equation does not match
the regexp.

To be safe the default setting of this variable uses the former approach
with a leading “=” character."

  :group 'consult-omni
  :type '(choice (regexp :tag "(Default) formula after =" "^=\\(.*\\)?")
                 (regexp :tag "Any string with digits, operators or brackets" "\\(.*[[:digit:]\/\*\+-=%^&$\(\{\[].*\\)")))

(defun consult-omni--numi-preview (_cand)
  "Preview function for CAND from `consult-omni-numi'."
  (ignore))

(defun consult-omni--numi-callback (cand)
  "Callback function for CAND from `consult-omni-numi'."
  (let ((result  (get-text-property 0 :title cand)))
    (kill-new result)))

(defun consult-omni--numi-filter (candidates &optional _query)
  "Filter CANDIDATES from `consult-omni-numi'."
  (cl-loop for candidate in candidates
           when (not (member candidate '("?" "error")))
           collect candidate))

(defun consult-omni--numi-valid-input-p (&optional input)
  "Check if INPUT matches `consult-omni-numi-regexp-pattern'."
  (cond
   ((stringp input)
    (if consult-omni-numi-regexp-pattern
        (if (string-match consult-omni-numi-regexp-pattern input nil)
            (or (match-string 1 input) input)
          nil)
      input))
   (t input)))

(cl-defun consult-omni--numi-builder (input &rest args &key callback &allow-other-keys)
  "Make builder command line args for “numi-cli” with INPUT and ARGS.

CALLBACK is a function used internally to update the list of candidates in
the minibuffer asynchronously.  It is called with a list of strings, which
are new annotated candidates \(e.g. as they arrive from an asynchronous
process\) to be added to the minibuffer completion cnadidates.  See the
section on REQUEST in documentation for `consult-omni-define-source' as
well as the function
`consult-omni--multi-update-dynamic-candidates' for how CALLBACK is used."
  (pcase-let* ((`(,query . _) (consult-omni--split-command input (seq-difference args (list :callback callback)))))
    (funcall #'consult-omni--async-builder (shell-quote-argument query) consult-omni-numi-args)))

;; Define the Numi source
(consult-omni-define-source "Numi"
                            :narrow-char ?N
                            :category 'consult-omni-calc
                            :type 'async
                            :require-match t
                            :face 'consult-omni-engine-title-face
                            :request #'consult-omni--numi-builder
                            :valid-input #'consult-omni--numi-valid-input-p
                            :filter #'consult-omni--numi-filter
                            :on-preview #'ignore
                            :on-return #'identity
                            :on-callback #'consult-omni--numi-callback
                            :preview-key consult-omni-preview-key
                            :search-hist 'consult-omni--search-history
                            :select-hist 'consult-omni--selection-history
                            :group #'consult-omni--group-function
                            :enabled (lambda () (executable-find "numi-cli"))
                            :sort t
                            :interactive consult-omni-intereactive-commands-type
                            :annotate nil)

;;; provide `consult-omni-numi module

(provide 'consult-omni-numi)

(add-to-list 'consult-omni-sources-modules-to-load 'consult-omni-numi)
;;; consult-omni-numi.el ends here
