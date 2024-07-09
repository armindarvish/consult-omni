;;; consult-omni-buffer.el --- Consulting Buffers -*- lexical-binding: t -*-

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

(defun consult-omni--consult-buffer-preview (cand)
  "Preview function for `consult-omni--buffer'."
  (if cand
      (let* ((title (get-text-property 0 :title cand)))
        (when-let ((buff (get-buffer title)))
          (consult--buffer-action buff)))))

(consult-omni--make-source-from-consult-source 'consult--source-buffer
                                               :type 'sync
                                               :on-preview #'consult-omni--consult-buffer-preview
                                               :on-return #'identity
                                               :on-callback #'consult--buffer-action
                                               :search-hist 'consult-omni--search-history
                                               :select-hist 'consult-omni--selection-history
                                               :static 'both
                                               :preview-key 'consult-omni-preview-key
                                               :on-new #'consult--buffer-action
                                               :group #'consult-omni--group-function)

(consult-omni--make-source-from-consult-source 'consult--source-modified-buffer
                                               :type 'sync
                                               :on-preview #'consult-omni--consult-buffer-preview
                                               :on-return #'identity
                                               :on-callback #'consult--buffer-action
                                               :search-hist 'consult-omni--search-history
                                               :select-hist 'consult-omni--selection-history
                                               :static 'both
                                               :preview-key 'consult-omni-preview-key
                                               :on-new #'consult--buffer-action
                                               :group #'consult-omni--group-function)

(consult-omni--make-source-from-consult-source 'consult--source-hidden-buffer
                                               :type 'sync
                                               :on-preview #'consult-omni--consult-buffer-preview
                                               :on-return #'identity
                                               :on-callback #'consult--buffer-action
                                               :search-hist 'consult-omni--search-history
                                               :select-hist 'consult-omni--selection-history
                                               :static 'both
                                               :preview-key 'consult-omni-preview-key
                                               :on-new #'consult--buffer-action
                                               :group #'consult-omni--group-function)

(consult-omni--make-source-from-consult-source 'consult--source-project-buffer
                                               :type 'sync
                                               :on-preview #'consult-omni--consult-buffer-preview
                                               :on-return #'identity
                                               :on-callback #'consult--buffer-action
                                               :search-hist 'consult-omni--search-history
                                               :select-hist 'consult-omni--selection-history
                                               :static 'both
                                               :preview-key 'consult-omni-preview-key
                                               :on-new #'consult--buffer-action
                                               :group #'consult-omni--group-function)

(consult-omni--make-source-from-consult-source 'consult--source-recent-file
                                               :type 'sync
                                               :on-preview #'consult-omni--consult-buffer-preview
                                               :on-return #'identity
                                               :on-callback #'consult--file-action
                                               :search-hist 'consult-omni--search-history
                                               :select-hist 'consult-omni--selection-history
                                               :static 'both
                                               :preview-key 'consult-omni-preview-key
                                               :on-new #'consult--file-action
                                               :group #'consult-omni--group-function)

(consult-omni--make-source-from-consult-source 'consult--source-project-recent-file
                                               :type 'sync
                                               :on-preview #'consult-omni--consult-buffer-preview
                                               :on-return #'identity
                                               :on-callback #'consult--file-action
                                               :search-hist 'consult-omni--search-history
                                               :select-hist 'consult-omni--selection-history
                                               :static 'both
                                               :preview-key 'consult-omni-preview-key
                                               :on-new #'consult--file-action
                                               :group #'consult-omni--group-function)

(consult-omni--make-source-from-consult-source 'consult--source-bookmark
                                               :type 'sync
                                               :on-preview #'consult-omni--consult-buffer-preview
                                               :on-return #'identity
                                               :on-callback #'consult--bookmark-action
                                               :search-hist 'consult-omni--search-history
                                               :select-hist 'consult-omni--selection-history
                                               :static 'both
                                               :preview-key 'consult-omni-preview-key
                                               :on-new #'bookmark-set
                                               :group #'consult-omni--group-function)

;;; provide `consult-omni-buffer' module

(provide 'consult-omni-buffer)

(add-to-list 'consult-omni-sources-modules-to-load 'consult-omni-buffer)
;;; consult-omni-buffer.el ends here
