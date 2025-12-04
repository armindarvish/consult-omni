;;; consult-omni-consult-notes.el --- Consulting Consult Notes -*- lexical-binding: t -*-

;; Copyright (C) 2024 Armin Darvish

;; Author: Armin Darvish
;; Maintainer: Armin Darvish
;; Created: 2024
;; Version: 0.3
;; Package-Requires: (
;;         (emacs "29.4")
;;         (consult "2.0")
;;         (consult-notes "0.7")
;;         (consult-omni "0.3"))
;;
;; Homepage: https://github.com/armindarvish/consult-omni
;; Keywords: convenience

;;; Commentary:
;; consult-omni-consult-notes enables using consult-notes in consult-omni.
;; It provides commands to search note files using consult-notes the
;; backend.
;;
;; For more info on consult-notes see:
;; URL `https://github.com/mclear-tools/consult-notes'

;;; Code:

(require 'consult-omni)
(require 'consult-notes nil t)

(defun consult-omni--consult-notes-org-roam-note-preview (cand)
  "Preview function for CAND from org-roam files."
  (if cand
      (let* ((title (get-text-property 0 :title cand))
              (node (get-text-property 0 'node title)))
        (if (org-roam-node-p node)
            (consult--file-action (org-roam-node-file node))))))

(defun consult-omni--consult-notes-org-headings-preview (cand)
  "Preview function for CAND from org headings."
  (if cand
      (let* ((title (get-text-property 0 :title cand))
             (marker (get-text-property 0 'org-marker title)))
        (if marker
            (consult--jump marker)))))

(defun consult-omni--consult-notes-denote-preview (cand)
  "Preview function for CAND from denote file."
  (if (and cand
           (not (consult-notes-denote--excluded-p cand)))
      (let* ((title (get-text-property 0 :title cand))
             (file (format "%s" (get-text-property 0 'denote-path title))))
        (if file
            (consult--file-action file)))))

(defun consult-omni--consult-notes-org-roam-note-callback (cand &rest _args)
  "Callback function for CAND from org-roam files."
  (let* ((title (get-text-property 0 :title cand))
         (node (get-text-property 0 'node title)))
    (org-roam-node-open node)))

(defun consult-omni--consult-notes-org-headings-callback (cand &rest _args)
  "Callback function for CAND from org headings."
  (if cand
      (let* ((title (get-text-property 0 :title cand))
             (marker (get-text-property 0 'org-marker title)))
        (if marker
            (let* ((buff (marker-buffer marker))
                   (pos (marker-position marker)))
              (if buff (with-current-buffer buff
                         (if pos (goto-char pos))
                         (funcall consult--buffer-display buff)
                         (recenter nil t))))))))

(defun consult-omni--consult-notes-denote-callback (cand &rest _args)
  "Callback function for CAND from denote files."
  (if (and cand
           (not (consult-notes-denote--excluded-p cand)))
      (let* ((title (get-text-property 0 :title cand))
             (file (format "%s" (get-text-property 0 'denote-path title))))
        (if file
            (consult--file-action file)))))

(defun consult-omni--consult-notes-org-headings-new (cand)
  "Callback function for making “new” org headings from CAND."
  (org-capture-string cand))

(defun consult-omni--consult-notes-org-roam-note-new (cand)
  "Callback function for making “new” org-roam files from CAND."
  (org-roam-node-find nil cand))

(defun consult-omni--consult-notes-denote-new (cand)
  "Callback function for making “new” denote files from CAND."
  (consult-notes-denote--new-note cand))

;; make consult-omni sources from consult-notes `consult-notes-org-headings--source'.
(when (and (bound-and-true-p consult-notes-org-headings-mode)
           (plistp consult-notes-org-headings--source))
  (consult-omni--make-source-from-consult-source (plist-put consult-notes-org-headings--source :name "Consult Notes Org")
                                                 :category 'file
                                                 :type 'sync
                                                 :require-match nil
                                                 :face 'consult-omni-notes-title-face
                                                 :search-hist 'consult-omni--search-history
                                                 :select-hist 'consult-omni--selection-history
                                                 :on-preview #'consult-omni--consult-notes-org-headings-preview
                                                 :on-return #'identity
                                                 :on-callback #'consult-omni--consult-notes-org-headings-callback
                                                 :on-new #'consult-omni--consult-notes-org-headings-new
                                                 :search-hist 'consult-omni--search-history
                                                 :select-hist 'consult-omni--selection-history
                                                 :preview-key 'consult-omni-preview-key
                                                 :group #'consult-omni--group-function
                                                 :enabled (lambda () (bound-and-true-p consult-notes-org-headings-mode))
                                                 :interactive consult-omni-intereactive-commands-type))

;; make consult-omni sources from consult-notes `consult-notes-org-headings--source'.
(when (bound-and-true-p consult-notes-org-roam-mode)
  (cl-loop for source in '(consult-notes-org-roam--refs consult-notes-org-roam--nodes)
           do (let ((name (plist-get (eval source) :name)))
                (plist-put (eval source) :name (concat "Consult Notes " name))
                (consult-omni--make-source-from-consult-source source
                                                               :category 'file
                                                               :type 'sync
                                                               :require-match nil
                                                               :face 'consult-omni-notes-title-face
                                                               :search-hist 'consult-omni--search-history
                                                               :select-hist 'consult-omni--selection-history
                                                               :on-preview #'consult-omni--consult-notes-org-roam-note-preview
                                                               :on-return #'identity
                                                               :on-callback #'consult-omni--consult-notes-org-roam-note-callback
                                                               :on-new #'consult-omni--consult-notes-org-roam-note-new
                                                               :preview-key 'consult-omni-preview-key
                                                               :interactive consult-omni-intereactive-commands-type
                                                               :group #'consult-omni--group-function
                                                               :enabled (lambda () consult-notes-org-roam-mode)
                                                               :annotate nil))))

;; make consult-omni sources from consult-notes `consult-notes-org-headings--source'.
(when (and (bound-and-true-p consult-notes-denote-mode)
           (plistp consult-notes-denote--source))
  (consult-omni--make-source-from-consult-source (plist-put consult-notes-denote--source :name "Consult Notes Denote")
                                                 :category 'file
                                                 :type 'sync
                                                 :require-match nil
                                                 :face 'consult-omni-notes-title-face
                                                 :search-hist 'consult-omni--search-history
                                                 :select-hist 'consult-omni--selection-history
                                                 :on-preview #'consult-omni--consult-notes-denote-preview
                                                 :on-return #'identity
                                                 :on-callback #'consult-omni--consult-notes-denote-callback
                                                 :on-new #'consult-omni--consult-notes-denote-new
                                                 :preview-key 'consult-omni-preview-key
                                                 :interactive consult-omni-intereactive-commands-type
                                                 :group #'consult-omni--group-function
                                                 :enabled (lambda () consult-notes-denote-mode)
                                                 :annotate nil))

;;; provide `consult-omni-consult-notes' module

(provide 'consult-omni-consult-notes)

(add-to-list 'consult-omni-sources-modules-to-load 'consult-omni-consult-notes)
;;; consult-omni-consult-notes.el ends here
