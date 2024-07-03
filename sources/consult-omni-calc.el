;;; consult-omni-calc.el --- Consulting Emacs Calculator -*- lexical-binding: t -*-

;; Copyright (C) 2024 Armin Darvish

;; Author: Armin Darvish
;; Maintainer: Armin Darvish
;; Created: 2024
;; Version: 0.1
;; Package-Requires: ((emacs "28.1") (consult "1.4") (consult-omni "0.1"))
;; Homepage: https://github.com/armindarvish/consult-omni
;; Keywords: convenience

;;; Commentary:

;;; Code:

(require 'consult-omni)
(require 'calc)
(require 'calc-aent nil t)

(defcustom consult-omni-calc-number-only nil
  "Only show calculator results when the query result in a nunmber?"
  :type 'boolean)

(defcustom consult-omni-calc-message-errors nil
  "Whether to message errors for calc?

Setting this to non-nil will show messages
when the calcultor cannot find results"
  :type 'boolean)

(defun consult-omni--calc-callback (cand)
  "Copys the result as well as formula to kill ring."
  (let ((equ (get-text-property 0 :query cand))
        (result  (get-text-property 0 :title cand)))
  (kill-new (concat equ " => " result))
  (kill-new result)))

(cl-defun consult-omni--calc-fetch-results (input &rest args &key callback &allow-other-keys)
  "Calculate the result of possible math equations.

This uses `calc-eval' to return the result of input"
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
    (when (string-match "[[:digit:]\/\*\+-=%^&$\(\{\[]" query nil t)
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
        ('error (and consult-omni-calc-message-errors (message (error-message-string err))))))
    (when result (setq annotated-result (propertize result
                                                    :source source
                                                    :title result
                                                    :url nil
                                                    :query query)))
    (if annotated-result
        (list annotated-result)
      nil)))

;; Define the Calc Source
(consult-omni-define-source "calc"
                            :narrow-char ?c
                            :category 'consult-omni-calc
                            :type 'sync
                            :require-match t
                            :face 'consult-omni-date-face
                            :request #'consult-omni--calc-fetch-results
                            :on-preview #'ignore
                            :on-return #'identity
                            :on-callback #'consult-omni--calc-callback
                            :preview-key consult-omni-preview-key
                            :search-hist 'consult-omni--search-history
                            :select-hist 'consult-omni--selection-history
                            :enabled (lambda () (fboundp 'calc-eval))
                            :group #'consult-omni--group-function
                            :sort t
                            :static nil
                            :annotate nil)

;;; provide `consult-omni-calc' module

(provide 'consult-omni-calc)

(add-to-list 'consult-omni-sources-modules-to-load 'consult-omni-calc)
;;; consult-omni-calc.el ends here
