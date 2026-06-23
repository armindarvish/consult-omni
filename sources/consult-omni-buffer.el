;;; consult-omni-buffer.el --- Consulting Buffers -*- lexical-binding: t -*-

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

;;; Commentary:
;; consult-omni-buffer provides commands for searching buffer names
;; similar to consult-buffer but using consult-omni.

;;; Code:

(require 'consult-omni)

(defun consult-omni--consult-buffer-preview (cand)
  "Preview function for CAND from `consult-omni--buffer'."
  (if cand
      (let* ((title (get-text-property 0 :title cand)))
        (when-let* ((buff (get-buffer title)))
          (consult--buffer-action buff)))))

;; make a consult-omni source from `consult-source-buffer'
(consult-omni--make-source-from-consult-source 'consult-source-buffer
                                               :type 'sync
                                               :min-input 0
                                               :on-preview #'consult-omni--consult-buffer-preview
                                               :on-return #'identity
                                               :on-callback #'consult--buffer-action
                                               :search-hist 'consult-omni--search-history
                                               :select-hist 'consult-omni--selection-history
                                               :interactive consult-omni-intereactive-commands-type
                                               :preview-key 'consult-omni-preview-key
                                               :on-new #'consult--buffer-action
                                               :group #'consult-omni--group-function
                                               :enabled (lambda () (bound-and-true-p consult-source-buffer)))

;; make a consult-omni source from `consult-source-modified-buffer'
(consult-omni--make-source-from-consult-source 'consult-source-modified-buffer
                                               :type 'sync
                                               :min-input 0
                                               :on-preview #'consult-omni--consult-buffer-preview
                                               :on-return #'identity
                                               :on-callback #'consult--buffer-action
                                               :search-hist 'consult-omni--search-history
                                               :select-hist 'consult-omni--selection-history
                                               :interactive consult-omni-intereactive-commands-type
                                               :preview-key 'consult-omni-preview-key
                                               :on-new #'consult--buffer-action
                                               :group #'consult-omni--group-function
                                               :enabled (lambda () (bound-and-true-p consult-source-modified-buffer)))

;; make a consult-omni source from `consult-source-hidden-buffer'
(consult-omni--make-source-from-consult-source 'consult-source-hidden-buffer
                                               :type 'sync
                                               :min-input 0
                                               :on-preview #'consult-omni--consult-buffer-preview
                                               :on-return #'identity
                                               :on-callback #'consult--buffer-action
                                               :search-hist 'consult-omni--search-history
                                               :select-hist 'consult-omni--selection-history
                                               :interactive consult-omni-intereactive-commands-type
                                               :preview-key 'consult-omni-preview-key
                                               :on-new #'consult--buffer-action
                                               :group #'consult-omni--group-function
                                               :enabled (lambda () (bound-and-true-p consult-source-hidden-buffer)))

;; make a consult-omni source from `consult-source-project-buffer'
(consult-omni--make-source-from-consult-source 'consult-source-project-buffer
                                               :type 'sync
                                               :min-input 0
                                               :on-preview #'consult-omni--consult-buffer-preview
                                               :on-return #'identity
                                               :on-callback #'consult--buffer-action
                                               :search-hist 'consult-omni--search-history
                                               :select-hist 'consult-omni--selection-history
                                               :interactive consult-omni-intereactive-commands-type
                                               :preview-key 'consult-omni-preview-key
                                               :on-new #'consult--buffer-action
                                               :group #'consult-omni--group-function
                                               :enabled (lambda () (bound-and-true-p consult-source-project-buffer)))

;; make a consult-omni source from `consult-source-recent-file'
(consult-omni--make-source-from-consult-source 'consult-source-recent-file
                                               :type 'sync
                                               :min-input 0
                                               :on-preview #'consult-omni--consult-buffer-preview
                                               :on-return #'identity
                                               :on-callback #'consult--file-action
                                               :search-hist 'consult-omni--search-history
                                               :select-hist 'consult-omni--selection-history
                                               :interactive consult-omni-intereactive-commands-type
                                               :preview-key 'consult-omni-preview-key
                                               :on-new #'consult--file-action
                                               :group #'consult-omni--group-function
                                               :enabled (lambda () (bound-and-true-p consult-source-recent-file)))

;; make a consult-omni source from `consult-source-project-recent-file'
(consult-omni--make-source-from-consult-source 'consult-source-project-recent-file
                                               :type 'sync
                                               :min-input 0
                                               :on-preview #'consult-omni--consult-buffer-preview
                                               :on-return #'identity
                                               :on-callback #'consult--file-action
                                               :search-hist 'consult-omni--search-history
                                               :select-hist 'consult-omni--selection-history
                                               :interactive consult-omni-intereactive-commands-type
                                               :preview-key 'consult-omni-preview-key
                                               :on-new #'consult--file-action
                                               :group #'consult-omni--group-function
                                               :enabled (lambda () (bound-and-true-p consult-source-project-recent-file)))

;; make a consult-omni source from `consult-source-bookmark'
(consult-omni--make-source-from-consult-source 'consult-source-bookmark
                                               :type 'sync
                                               :min-input 0
                                               :on-preview #'consult-omni--consult-buffer-preview
                                               :on-return #'identity
                                               :on-callback #'consult--bookmark-action
                                               :search-hist 'consult-omni--search-history
                                               :select-hist 'consult-omni--selection-history
                                               :interactive consult-omni-intereactive-commands-type
                                               :preview-key 'consult-omni-preview-key
                                               :on-new #'bookmark-set
                                               :group #'consult-omni--group-function
                                               :enabled (lambda () (bound-and-true-p consult-source-bookmark)))

;;; provide `consult-omni-buffer' module

(provide 'consult-omni-buffer)

(add-to-list 'consult-omni-sources-modules-to-load 'consult-omni-buffer)
;;; consult-omni-buffer.el ends here
