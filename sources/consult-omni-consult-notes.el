;;; consult-omni-consult-notes.el --- Consulting Consult Notes -*- lexical-binding: t -*-

;; Copyright (C) 2024 Armin Darvish

;; Author: Armin Darvish
;; Maintainer: Armin Darvish
;; Created: 2024
;; Version: 0.1
;; Package-Requires: ((emacs "28.1") (consult "1.4") (consult-omni "0.1") (consult-notes "0.7"))
;; Homepage: https://github.com/armindarvish/consult-omni
;; Keywords: convenience

;;; Commentary:

;;; Code:

(require 'consult-omni)
(require 'consult-notes nil t)

(defun consult-omni--org-roam-note-preview (cand)
  "Preview function for org-roam files."
  (if cand
      (let* ((title (get-text-property 0 :title cand))
             (node (org-roam-node-from-title-or-alias title)))
        (if (org-roam-node-p node)
            (consult--file-action (org-roam-node-file node))))))

(defun consult-omni--org-headings-preview (cand)
  "Preview function for org headings."
  (if cand
      (let* ((title (get-text-property 0 :title cand))
             (marker (get-text-property 0 'consult--candidate title)))
        (if marker
            (consult--jump marker)))))

(defun consult-omni--org-roam-note-callback (cand &rest args)
  "Callback function for org-roam files."
  (let* ((title (get-text-property 0 :title cand))
         (node (org-roam-node-from-title-or-alias title)))
    (org-roam-node-open node)))

(defun consult-omni--org-headings-callback (cand &rest args)
  "Callback function for org headings."
  (if cand
      (let* ((title (get-text-property 0 :title cand))
             (marker (get-text-property 0 'consult--candidate title)))
        (if marker
            (let* ((buff (marker-buffer marker))
                   (pos (marker-position marker)))
              (if buff (with-current-buffer buff
                         (if pos (goto-char pos))
                         (funcall consult--buffer-display buff)
                         (recenter nil t))))))))

;; make consult-omni sources from consult-notes sources
(when consult-notes-org-headings-mode
  (consult-omni--make-source-from-consult-source 'consult-notes-org-headings--source
                                                 :category 'file
                                                 :type 'sync
                                                 :face 'consult-omni-notes-title-face
                                                 :search-hist 'consult-omni--search-history
                                                 :select-hist 'consult-omni--selection-history
                                                 :on-preview #'consult-omni--org-headings-preview
                                                 :on-return #'identity
                                                 :on-callback #'consult-omni--org-headings-callback
                                                 :search-hist 'consult-omni--search-history
                                                 :select-hist 'consult-omni--selection-history
                                                 :preview-key 'consult-preview-key
                                                 :group #'consult-omni--group-function
                                                 :enabled (lambda () consult-notes-org-headings-mode)
                                                 :static 'both))

(when consult-notes-org-roam-mode
  (cl-loop for source in '(consult-notes-org-roam--refs consult-notes-org-roam--nodes)
           do (consult-omni--make-source-from-consult-source source
                                                             :category 'file
                                                             :type 'sync
                                                             :face 'consult-omni-notes-title-face
                                                             :search-hist 'consult-omni--search-history
                                                             :select-hist 'consult-omni--selection-history
                                                             :on-preview #'consult-omni--org-roam-note-preview
                                                             :on-return #'identity
                                                             :on-callback #'consult-omni--org-roam-note-callback
                                                             :preview-key 'consult-preview-key
                                                             :static 'both
                                                             :group #'consult-omni--group-function
                                                             :enabled (lambda () consult-notes-org-roam-mode)
                                                             :annotate nil)))

;;; provide `consult-omni-consult-notes' module

(provide 'consult-omni-consult-notes)

(add-to-list 'consult-omni-sources-modules-to-load 'consult-omni-consult-notes)
;;; consult-omni-consult-notes.el ends here
