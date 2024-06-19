;;; consult-omni-calc.el --- Consulting Emacs Calculator -*- lexical-binding: t -*-

;; Copyright (C) 2024 Armin Darvish

;; Author: Armin Darvish
;; Maintainer: Armin Darvish
;; Created: 2024
;; Version: 0.1
;; Package-Requires: ((emacs "28.1") (consult "1.1") (consult-omni "0.2"))
;; Homepage: https://github.com/armindarvish/consult-omni
;; Keywords: convenience

;;; Commentary:

;;; Code:

(require 'consult-omni)

(defcustom consult-omni-calc-number-only nil
"Only show calculator rsults when the query result in a nunmber?"
:type 'boolean)

(defun consult-omni--calc-callback (cand)
  (let ((equ (get-text-property 0 :query cand))
        (result  (get-text-property 0 :title cand)))
  (kill-new (concat equ " => " result))
  (kill-new result)))

(cl-defun consult-omni--calc-fetch-results (input &rest args &key callback &allow-other-keys)
  "Makes cnaidate with INPUT as placeholder for `consult-omni-gptel'.

This makes a placeholder string “ask gptel: %s” %s=INPUT with
metadata so it can be send to `gptel'."
  (unless (featurep 'gptel)
    (error "consult-omni: gptel is not available. Make sure to install and load `gptel'."))
  (pcase-let* ((`(,query . ,opts) (consult-omni--split-command input args))
               (opts (car-safe opts))
               (source "calc")
               (calc-eval-error t)
               (result)
               (annotated-result)
               )
    (when (string-match "[[:digit:]\/\*\+-=%^&$\(\{\[]" query nil t)
      (condition-case err
          (if consult-omni-calc-number-only
              (setq result (calc-eval (list query) 'num))
            (setq result (calc-eval (list query))))
        ('error (message err))
        ))

     (when result (setq annotated-result (propertize result
                                        :source source
                                        :title result
                                        :url nil
                                        :query query
                                        )))
     (if annotated-result
        (list annotated-result)
       nil)
    ))



(consult-omni-define-source "calc"
                           :narrow-char ?C
                           :type 'sync
                           :face 'consult-omni-date-face
                           :request #'consult-omni--calc-fetch-results
                           :on-preview #'ignore
                           :on-return #'identity
                           :on-callback #'consult-omni--calc-callback
                           :preview-key consult-omni-preview-key
                           :search-history 'consult-omni--search-history
                           :selection-history 'consult-omni--selection-history
                           :enabled (lambda () (fboundp 'calc-eval))
                           :group #'consult-omni--group-function
                           :sort t
                           :static 'both
                           :annotate nil
                           )

;;; provide `consult-omni-calc' module

(provide 'consult-omni-calc)

(add-to-list 'consult-omni-sources-modules-to-load 'consult-omni-calc)
;;; consult-omni-calc.el ends here
