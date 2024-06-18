;;; consult-omni-buffer.el --- Consulting Buffers -*- lexical-binding: t -*-

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

(defun consult-omni--consult-buffer-preview (cand)
  "Preview function for `consult-omni--buffer'."
  (if cand
      (let* ((title (get-text-property 0 :title cand)))
        (when-let ((buff (get-buffer title)))
          (consult--buffer-action buff))
        )))

;; make consult-omni sources from `consult-buffer-sources'
(cl-loop for source in consult-buffer-sources
         do (if (symbolp source) (consult-omni--make-source-from-consult-source source
                                              :type 'sync
                                              :on-preview #'consult-omni--consult-buffer-preview
                                              :on-return #'identity
                                              :on-callback #'consult--buffer-action
                                              :search-history 'consult-omni--search-history
                                              :selection-history 'consult-omni--selection-history
                                              :static 'both
                                              :preview-key 'consult-omni-preview-key
                                              :group #'consult-omni--group-function
                                              )))

;;; provide `consult-omni-buffer' module

(provide 'consult-omni-buffer)

(add-to-list 'consult-omni-sources-modules-to-load 'consult-omni-buffer)
;;; consult-omni-buffer.el ends here
