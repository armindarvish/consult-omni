;;; consult-omni-calc.el --- Consulting Emacs Calculator -*- lexical-binding: t -*-

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
;; consult-omni-calc provides commands for getting calc results directly
;; in the minibuffer using consult-omni.

;;; Code:

(require 'consult-omni)
(require 'calc)
(require 'calc-aent nil t)
(require 'calc-units nil t)

;;; User Options (a.k.a. Custom Variables)

(defcustom consult-omni-calc-number-only nil
  "Only show calculator results when the query result is a number?"
  :group 'consult-omni
  :type 'boolean)

(defcustom consult-omni-calc-regexp-pattern "^=\\(.*\\)?"
  "Regexp to detect calculator formula?

The first capturing group will be used as input for `calc-eval'.
If there is no capture group, the whole input query is used.

Note that the default setting is simply any string with a leading “=”.
This allows the user to see the results from calculator \(i.e.
`consult-omni--calc-fetch-results')\ from any string without consult-omni
detecting (i.e. trying to guess) what is a mathematical equation.  This
can specially be useful in multi-source searches, if the user does not
want to see random results from the calculator on every search string.
The downside of this approach is that the user has to type “=” every time
before seeing results form the calculator.  For example to see the result
of the equation 2+3, the user has to type:
  “#=2+3”
\(Note that the leading “#” above is from the default perl style of
`consult-async-split-style' and otherwise not neccessary\).

Alternatively, one can change this variable to regexp pattern that
detects/guesses a mathematical equation \(for example by looking for
strings that contain digits and/or mathematical operators\).  For an
example, see the default choices for this custom variable.  This would
remove the need to type a leading character every time but at the same
time may miss some edge cases if the user's query/equation does not match
this regexp.

To be safe the default setting of this variable uses the former approach
with a leading “=” character."

  :group 'consult-omni
  :type '(choice (regexp :tag "(Default) formula after =" "^=\\(.*\\)?")
                 (regexp :tag "Any string with digits, operators or brackets" "\\(.*[[:digit:]\/\*\+-=%^&$\(\{\[].*\\)")))

(defcustom consult-omni-calc-message-errors nil
  "Whether to message errors for calc?

Setting this to non-nil will show messages when the calcultor cannot find
results, which may not be desirable in multi-source omni searches."
  :group 'consult-omni
  :type 'boolean)

(defun consult-omni--calc-callback (cand)
  "Copy the result, CAND, as well as the equation to the `kill-ring'."
  (let ((equ (get-text-property 0 :query cand))
        (result  (get-text-property 0 :title cand)))
    (kill-new (concat equ " => " result))
    (kill-new result)))

(defun consult-omni--calc-valid-input-p (&optional input)
  "Check if INPUT matches `consult-omni-calc-regexp-pattern'."
  (cond
   ((stringp input)
    (if consult-omni-calc-regexp-pattern
        (if (string-match consult-omni-calc-regexp-pattern input nil)
            (or (match-string 1 input) input)
          nil)
      input))
   (t input)))

(cl-defun consult-omni--calc-fetch-results (input &rest args &key callback &allow-other-keys)
  "Calculate the result of possible math equations in INPUT with ARGS.

This uses `calc-eval' to return the result of the INPUT string.  The INPUT
should match `consult-omni-calc-regexp-pattern', which by default has a
leading “=”.  For example to see the result of the equation 2+3, the user
should type:
  “#=2+3”
\(Note that the leading “#” above is from the default perl style of
`consult-async-split-style' and otherwise not neccessary\).

For more details on how to type equations refer to `calc-eval'
documentation on input strings and Emacs manual for calc in general as
well as this package's repo:
URL `https://github.com/armindarvish/consult-omni'

To change this behavior, `consult-omni-calc-regexp-pattern' can be edited.
For example, to remove the leading “=”, in which case the user can simply
enter “2+3” to see the results.  Keep in mind this is to ensure that
consult-omni-calc returns results only when the user intends the query as
a calc equation.  Removing “=”, may sometimes lead to confusing answers
from the calculator in multi-source searches, because it passes the INPUT
query to `calc-eval', which in turn tries its best to interpret the INPUT
as a mathematical equation.

For unit conversion, the INPUT should contain “:convert” keyword with unit
value as an optional argument using consult's syntax.  See the documention
of `consult-omni-multi' for how to pass extra arguments using
`consult-async-split-style' syntax.
For example:
 “#=100degF -- :convert degC”
converts 100 Farenheit to Celsius.

CALLBACK is a function used internally to update the list of candidates in
the minibuffer asynchronously.  It is called with a list of strings, which
are new annotated candidates \(e.g. as they arrive from an asynchronous
process\) to be added to the minibuffer completion cnadidates.  See the
section on REQUEST in documentation for `consult-omni-define-source' as
well as the function
`consult-omni--multi-update-dynamic-candidates' for how CALLBACK is used."
  (pcase-let* ((`(,query . ,opts) (consult-omni--split-command input (seq-difference args (list :callback callback))))
               (source "calc")
               (opts (car-safe opts))
               (opts (cl-substitute :convert :c opts :test 'equal))
               (convert (plist-get opts :convert))
               (extra-args (plist-get opts :$))
               (extra-args (and extra-args (car (read-from-string extra-args))))
               (extra-args (if (listp extra-args) extra-args (list extra-args)))
               (extra-args (mapcar (lambda (item) (cond
                                                   ((numberp item) (format "%s" item))
                                                   ((and (symbolp item) (numberp (symbol-value item))) (format "%s" (symbol-value item)))
                                                   ((and (functionp item) (numberp (funcall item))) (format "%s" (funcall item)))
                                                   ((and (numberp (eval item))) (format "%s" (eval item)))
                                                   (t item)))
                                   extra-args))
               (calc-eval-error t)
               (result)
               (annotated-result))
    (when query
      (condition-case err
          (if convert
              (cl-letf* (((symbol-function 'calc-convert-units)
                          (symbol-function 'calc-convert-exact-units)))
                (setq result (cond
                              ((string-match-p ".*deg.*" convert)
                               (setq result (calc-eval (math-convert-temperature (apply #'calc-eval (list query) 'raw extra-args) (apply #'calc-eval (list (replace-regexp-in-string "[[:digit:]\s$]+" "" query extra-args)) 'raw extra-args) (calc-eval (list convert) 'raw)))))
                              (t (calc-eval (math-convert-units (apply #'calc-eval (list query) 'raw extra-args) (calc-eval (list convert) 'raw)))))))
            (if consult-omni-calc-number-only
                (setq result (apply #'calc-eval (list query) 'num extra-args))
              (setq result (apply #'calc-eval (list query) nil extra-args))))
        (error (and consult-omni-calc-message-errors (message (error-message-string err)))))
      (when result (setq annotated-result (propertize result
                                                      :source source
                                                      :title result
                                                      :url nil
                                                      :query query)))
      (if annotated-result
          (list annotated-result)
        nil))))

;; Define the Calc Source
(consult-omni-define-source "calc"
                            :narrow-char ?c
                            :category 'consult-omni-calc
                            :type 'sync
                            :require-match t
                            :face 'consult-omni-date-face
                            :request #'consult-omni--calc-fetch-results
                            :min-input 0
                            :valid-input #'consult-omni--calc-valid-input-p
                            :on-preview #'ignore
                            :on-return #'identity
                            :on-callback #'consult-omni--calc-callback
                            :preview-key consult-omni-preview-key
                            :search-hist 'consult-omni--search-history
                            :select-hist 'consult-omni--selection-history
                            :enabled (lambda () (fboundp 'calc-eval))
                            :group #'consult-omni--group-function
                            :sort t
                            :interactive consult-omni-intereactive-commands-type
                            :annotate nil)

;;; provide `consult-omni-calc' module

(provide 'consult-omni-calc)

(add-to-list 'consult-omni-sources-modules-to-load 'consult-omni-calc)
;;; consult-omni-calc.el ends here
