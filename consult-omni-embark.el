;;; consult-omni-embark.el --- Emabrk Actions for `consult-omni' -*- lexical-binding: t -*-

;; Copyright (C) 2024 Armin Darvish


;; Author: Armin Darvish
;; Maintainer: Armin Darvish
;; Created: 2024
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (consult "0.34") (consult-omni 0.2))
;; Homepage: https://github.com/armindarvish/consult-omni
;; Keywords: convenience

;;; Commentary:

;;; Code:

;;; Requirements

(require 'embark)
(require 'consult-omni)

;;; Define Embark Action Functions

(defun consult-omni-embark-default-action (cand)
  "Calls the default action on CAND.

Gets the default callback function from `consult-omni-sources-alist'."
  (let* ((source (and (stringp cand) (get-text-property 0 :source cand))))
    (funcall (consult-omni--get-source-prop source :on-callback) cand))
  )

(add-to-list 'embark-default-action-overrides '(consult-omni . consult-omni-embark-default-action))


(defun consult-omni-embark-insert-title (cand)
  "Insert the title oif the candidate at point"
  (if-let ((title (and (stringp cand) (get-text-property 0 :title cand))))
      (insert (format " %s " title))))

(defun consult-omni-embark-copy-title-as-kill (cand)
  "Copy the title of the candidate to `kill-ring'."
  (if-let ((title (and (stringp cand) (get-text-property 0 :title cand))))
      (kill-new (string-trim title))))

(defun consult-omni-embark-insert-url-link (cand)
  "Insert the title oif the candidate at point."
  (let* ((url (and (stringp cand) (get-text-property 0 :url cand)))
         (url (and (stringp url) (string-trim url)))
         (title (and (stringp cand) (get-text-property 0 :title cand))))
    (when url
      (cond
       ((derived-mode-p 'org-mode)
        (insert (cond
                 ((and url title) (format " [[%s][%s]] " url title))
                 (url (format " [[%s]] " url))
                 (t ""))
                ))
       ((derived-mode-p 'markdown-mode)
        (insert (cond
                 ((and url title) (format " [%s](%s) " url title))
                 (url (format " <%s> " url))
                 (t ""))
                ))
       (t
        (insert (cond
                 ((and url title) (format " %s (%s) " title  url))
                 (url (format " %s " url))
                 (t ""))
                ))))))

(defun consult-omni-embark-copy-url-as-kill (cand)
  "Copy the url of the candidate to `kill-ring'."
  (if-let ((url (and (stringp cand) (get-text-property 0 :url cand))))
      (kill-new (format " %s " (string-trim url)))
    ))

(defun consult-omni-embark-external-browse-link (cand)
  "Open the url with `consult-omni-default-browse-function'"
  (if-let* ((url (and (stringp cand) (get-text-property 0 :url cand))))
      (funcall consult-omni-default-browse-function url)))

(defun consult-omni-embark-alternate-browse-link (cand)
  "Open the url with `consult-omni-alternate-browse-function'"
  (if-let* ((url (and (stringp cand) (get-text-property 0 :url cand))))
      (funcall consult-omni-alternate-browse-function url)))

(defun consult-omni-embark-external-browse-search-link (cand)
  "Open the search url (the search engine page) in the external browser."
  (if-let* ((search-url (and (stringp cand) (get-text-property 0 :search-url cand))))
      (funcall #'browse-url search-url)))

(defun consult-omni-embark-show-preview (cand)
  "Get a preview of CAND.

Gets the preview function from `consult-omni-sources-alist'."
  (let* ((source (and (stringp cand) (get-text-property 0 :source cand))))
    (funcall (consult-omni--get-source-prop source :on-preview) cand))
  )

;;; Define Embark Keymaps

(defvar-keymap consult-omni-embark-general-actions-map
  :doc "Keymap for consult-omni-embark"
  :parent embark-general-map
  "i t"  #'consult-omni-embark-insert-title
  "i u" #'consult-omni-embark-insert-url-link
  "w t" #'consult-omni-embark-copy-title-as-kill
  "w u" #'consult-omni-embark-copy-url-as-kill
  "o o" #'consult-omni-embark-external-browse-link
  "o O" #'consult-omni-embark-alternate-browse-link
  "o s" #'consult-omni-embark-external-browse-search-link
  "o p" #'consult-omni-embark-show-preview
  )


(add-to-list 'embark-keymap-alist '(consult-omni . consult-omni-embark-general-actions-map))

(defun consult-omni-embark-scholar-external-browse-doi (cand)
  "Open the DOI url in external browser"
  (if-let* ((doi (and (stringp cand) (get-text-property 0 :doi cand))))
      (funcall #'browse-url (concat "https://doi.org/" doi))))

(defun consult-omni-embark-scholar-copy-authors-as-kill (cand)
  "Copy the authors of the candidate to `kill-ring'."
  (if-let ((authors (and (stringp cand) (get-text-property 0 :authors cand))))
      (kill-new (string-trim (format " %s " authors)))
    ))

(defun consult-omni-embark-scholar-insert-authors (cand)
  "Insrt the authors of the candidate at point."
  (if-let ((authors (and (stringp cand) (get-text-property 0 :authors cand))))
      (insert (string-trim (mapconcat #'identity authors ", ")))
    ))

(defvar-keymap consult-omni-embark-scholar-actions-map
  :doc "Keymap for consult-omni-embark-scholar"
  :parent consult-omni-embark-general-actions-map
  "o d" #'consult-omni-embark-scholar-external-browse-doi
  "w a" #'consult-omni-embark-scholar-copy-authors-as-kill
  "i a" #'consult-omni-embark-scholar-insert-authors
  )

(add-to-list 'embark-keymap-alist '(consult-omni-scholar . consult-omni-embark-scholar-actions-map))

(add-to-list 'embark-default-action-overrides '(consult-omni-scholar . consult-omni-embark-default-action))



(defvar-keymap consult-omni-embark-video-actions-map
  :doc "Keymap for consult-omni-embark-video"
  :parent consult-omni-embark-general-actions-map
  )

(add-to-list 'embark-keymap-alist '(consult-omni-video . consult-omni-embark-video-actions-map))

(add-to-list 'embark-default-action-overrides '(consult-omni-video . consult-omni-embark-default-action))

;;; Provide `consul-web-embark' module

(provide 'consult-omni-embark)

;;; consult-omni-embark.el ends here
