;;; consult-omni-embark.el --- Embark Actions for `consult-omni' -*- lexical-binding: t -*-

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

;;; Requirements

(require 'embark)
(require 'consult-omni)

;;; Customization Variables
(defcustom consult-omni-embark-default-term  #'eshell
  "Consult-omni default terminal to use in embark actions."
  :type '(choice (function :tag "(Default) eshell" #'eshell)
                 (function :tag "shell" #'shell)
                 (function :tag "term" #'term)
                 (function :tag "ansi-term" #'ansi-term)
                 (function :tag "vterm" #'vterm)
                 (function :tag "vterm" #'eat)
                 (function :tag "Custom Function" function)))

;;; Define Embark Action Functions

(defun consult-omni-embark-default-action (cand)
  "Calls the default action on CAND.

Gets the default callback function from `consult-omni-sources-alist'."
  (let* ((source (and (stringp cand) (get-text-property 0 :source cand))))
    (funcall (consult-omni--get-source-prop source :on-callback) cand)))

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
  "Insert the title of the candidate at point."
  (let* ((url (and (stringp cand) (get-text-property 0 :url cand)))
         (url (and (stringp url) (string-trim url)))
         (title (and (stringp cand) (get-text-property 0 :title cand))))
    (when url
      (cond
       ((derived-mode-p 'org-mode)
        (insert (cond
                 ((and url title) (format " [[%s][%s]] " url title))
                 (url (format " [[%s]] " url))
                 (t ""))))
       ((derived-mode-p 'markdown-mode)
        (insert (cond
                 ((and url title) (format " [%s](%s) " url title))
                 (url (format " <%s> " url))
                 (t ""))))
       (t
        (insert (cond
                 ((and url title) (format " %s (%s) " title  url))
                 (url (format " %s " url))
                 (t ""))))))))

(defun consult-omni-embark-copy-url-as-kill (cand)
  "Copy the url of the candidate to `kill-ring'."
  (if-let ((url (and (stringp cand) (get-text-property 0 :url cand))))
      (kill-new (format " %s " (string-trim url)))))

(defun consult-omni-embark-external-browse-link (cand)
  "Open the url with `consult-omni-default-browse-function'"
  (if-let* ((url (and (stringp cand) (get-text-property 0 :url cand))))
      (funcall consult-omni-default-browse-function url)))

(defun consult-omni-embark-alternate-browse-link (cand)
  "Open the url with `consult-omni-alternate-browse-function'"
  (if-let* ((url (and (stringp cand) (get-text-property 0 :url cand))))
      (funcall consult-omni-alternate-browse-function url)))

(defun consult-omni-embark-external-browse-search-link (cand)
  "Open the search url (the source search page) in the external browser."
  (if-let* ((search-url (and (stringp cand) (get-text-property 0 :search-url cand))))
      (funcall #'browse-url search-url)))

(defun consult-omni-embark-show-preview (cand)
  "Get a preview of CAND.

Gets the preview function from `consult-omni-sources-alist'."
  (let* ((source (and (stringp cand) (get-text-property 0 :source cand))))
    (funcall (consult-omni--get-source-prop source :on-preview) cand)))

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
  "o p" #'consult-omni-embark-show-preview)

(add-to-list 'embark-keymap-alist '(consult-omni . consult-omni-embark-general-actions-map))

(defcustom consult-omni-embark-scholar-make-note-func  #'consult-omni-embark-scholar-default-note
  "Function for making note template for scholar articles."
  :type '(choice (function :tag "(Default) Make Note with Title, Link, Journal, Authors... " consult-omni-embark-scholar-default-note)
                 (function :tag "Custom Function" function)))

(defun consult-omni-embark-scholar-external-browse-doi (cand)
  "Open the DOI url in external browser"
  (if-let* ((doi (and (stringp cand) (get-text-property 0 :doi cand))))
      (funcall #'browse-url (concat "https://doi.org/" doi))))

(defun consult-omni-embark-scholar-copy-authors-as-kill (cand)
  "Copy the authors of CAND to `kill-ring'."
  (if-let ((authors (and (stringp cand) (get-text-property 0 :authors cand))))
      (kill-new (string-trim (format " %s " authors)))))

(defun consult-omni-embark-scholar-insert-authors (cand)
  "Insert the authors of CAND at point."
  (if-let ((authors (and (stringp cand) (get-text-property 0 :authors cand))))
      (insert (string-trim (mapconcat #'identity authors ", ")))))

(defun consult-omni-embark-scholar-default-note (cand)
  "Default function for making note templates.

This can be used for making notes for scholar articles."
  (let* ((url (and (stringp cand) (get-text-property 0 :url cand)))
         (url (and (stringp url) (string-trim url)))
         (doi (and (stringp cand) (get-text-property 0 :doi cand)))
         (doi (if (and doi (stringp doi)) (concat "https://doi.org/" doi)))
         (source (and (stringp cand) (get-text-property 0 :source cand)))
         (url (if (and (equal source "Scopus") doi)
                  doi
                url))
         (title (and (stringp cand) (get-text-property 0 :title cand)))
         (authors (and (stringp cand) (get-text-property 0 :authors cand)))
         (authors (cond
                   ((and (listp authors) (= (length authors) 1))
                    (car authors))
                   ((listp authors)
                    (mapconcat #'identity authors ", "))
                   (t authors)))
         (journal  (and (stringp cand) (get-text-property 0 :journal cand)))
         (date (and (stringp cand) (get-text-property 0 :date cand))))
    (cond
     ((derived-mode-p 'org-mode)
      (concat
       "\n"
       (cond
        ((and url title) (format "** [[%s][%s]]\n" url title))
        (url (format "** [[%s]]\n" url))
        (title (format "** %s\n" title)))
       (if authors (format "\n%s" authors))
       (if journal (format "\nin =%s= " journal))
       (if date (format "published on [%s]\n" date) "\n")
       "\n*** Notes\n"))
     ((derived-mode-p 'markdown-mode)
      (concat
       "\n"
       (cond
        ((and url title) (format "## [%s](%s)\n" url title))
        (url (format "## <%s>\n" url))
        (title (format "## %s\n" title)))
       (if authors (format "\n%s" authors))
       (if journal (format "\nin **%s** " journal))
       (if date (format "published on %s\n" date) "\n")
       "\n### Notes\n"))
     (t
      (concat
       "\n"
       (cond
        ((and url title) (format "** %s (%s)\n" title  url))
        (url (format "** %s\n" url))
        (title (format "** %s\n" title)))
       (if authors (format "\n%s" authors))
       (if journal (format "\nin %s " journal))
       (if date (format "published on %s\n" date) "\n")
       "\n*** Notes\n")))))

(defun consult-omni-embark-scholar-insert-note (cand)
  "Insert note snippet for article.

Uses `consult-omni-embark-scholar-make-note-func' to make template."
  (insert (funcall consult-omni-embark-scholar-make-note-func cand)))

(defvar-keymap consult-omni-embark-scholar-actions-map
  :doc "Keymap for consult-omni-embark-scholar"
  :parent consult-omni-embark-general-actions-map
  "o d" #'consult-omni-embark-scholar-external-browse-doi
  "w a" #'consult-omni-embark-scholar-copy-authors-as-kill
  "i a" #'consult-omni-embark-scholar-insert-authors
  "i n" #'consult-omni-embark-scholar-insert-note)

(add-to-list 'embark-keymap-alist '(consult-omni-scholar . consult-omni-embark-scholar-actions-map))

(add-to-list 'embark-default-action-overrides '(consult-omni-scholar . consult-omni-embark-default-action))

;;; Define Embark Action Functions

(defun consult-omni-embark-apps-open-filemanager (cand)
  "Open CAND's filepath with system's file manager."
  (if-let* ((path (and (stringp cand) (get-text-property 0 :path cand))))
      (pcase system-type
        ('darwin (call-process "open" nil 0 nil path "-R"))
        ('cygwin (call-process "cygstart" nil 0 nil path))
        ('windows-nt (and (fboundp 'w32-shell-execute) (w32-shell-execute "open" path)))
        (_ (call-process "xdg-open" nil 0 nil path)))))

(defun consult-omni-embark-apps-find-file (cand)
  "Open CAND's filepath with `find-file'."
  (if-let* ((path (and (stringp cand) (get-text-property 0 :path cand)))
            (directory (and (file-exists-p (file-truename path)) (file-truename path)))
            (default-directory directory))
      (call-interactively #'find-file)))

(defun consult-omni-embark-apps-open-externally (cand)
  "Open CAND's filepath using system's default application."
  (if-let ((path (and (stringp cand) (get-text-property 0 :path cand))))
      (pcase system-type
        ('darwin (call-process "open" nil 0 nil path))
        ('cygwin (call-process "cygstart" nil 0 nil path))
        ('windows-nt (and (fboundp 'w32-shell-execute) (w32-shell-execute "open" path)))
        (_ (call-process "xdg-open" nil 0 nil path)))
    nil))

(defun consult-omni-embark-apps-open-term (cand)
  "Open CAND's filepath in `consult-omni-embark-default-term'."
  (if-let* ((path (and (stringp cand) (get-text-property 0 :path cand)))
            (directory (and (file-exists-p (file-truename path)) (file-truename path)))
            (default-directory directory))
      (funcall consult-omni-embark-default-term)))

(defun consult-omni-embark-apps-insert-path (cand)
  "Insert the title of CAND at point."
  (if-let ((path (and (stringp cand) (get-text-property 0 :path cand))))
      (insert (format " %s " path))))

(defun consult-omni-embark-apps-copy-path-as-kill (cand)
  "Copy the title of CAND to `kill-ring'."
  (if-let ((path (and (stringp cand) (get-text-property 0 :path cand))))
      (kill-new (format " %s " path))))

;;; Define Embark Keymaps

(defvar-keymap consult-omni-embark-apps-actions-map
  :doc "Keymap for consult-omni-embark"
  :parent consult-omni-embark-general-actions-map
  "x"  #'consult-omni-embark-apps-open-externally
  "f"  #'consult-omni-embark-apps-find-file
  "o f"  #'consult-omni-embark-apps-find-file
  "o o" #'consult-omni-embark-apps-open-filemanager
  "o t" #'consult-omni-embark-apps-open-term
  "w p" #'consult-omni-embark-apps-copy-path-as-kil)

(add-to-list 'embark-keymap-alist '(consult-omni-apps . consult-omni-embark-apps-actions-map))
(add-to-list 'embark-default-action-overrides '(consult-omni-apps . consult-omni-embark-default-action))

;;; Define Embark Action Functions

(defun consult-omni-embark-calc-copy-results-as-kill (cand)
  "Copy the results of the calculator to `kill-ring'."
  (if-let ((results (and (stringp cand) (get-text-property 0 :title cand))))
      (kill-new (format " %s " results))))

(defun consult-omni-embark-calc-insert-results (cand)
  "Insert the results of the calculator at point"
  (if-let (results (and (stringp cand) (get-text-property 0 :title cand)))
      (insert (format " %s " results))))

(defun consult-omni-embark-calc-copy-formula-as-kill (cand)
  "Copy the results of the calculator to `kill-ring'."
  (if-let ((formula (and (stringp cand) (get-text-property 0 :query cand))))
      (kill-new (format " %s " formula))))

(defun consult-omni-embark-calc-insert-formula (cand)
  "Insert the results of the calculator at point"
  (if-let (formula (and (stringp cand) (get-text-property 0 :query cand)))
      (insert (format " %s " formula))))

;;; Define Embark Keymaps

(defvar-keymap consult-omni-embark-calc-actions-map
  :doc "Keymap for consult-omni-embark"
  :parent embark-general-map
  "w r"  #'consult-omni-embark-calc-copy-results-as-kill
  "w f"  #'consult-omni-embark-calc-copy-formula-as-kill
  "i r"  #'consult-omni-embark-calc-insert-results
  "i f"  #'consult-omni-embark-calc-insert-formula)

(add-to-list 'embark-keymap-alist '(consult-omni-calc . consult-omni-embark-calc-actions-map))
(add-to-list 'embark-default-action-overrides '(consult-omni-calc . consult-omni-embark-default-action))

(defcustom consult-omni-embark-video-default-player  (executable-find "mpv")
  "consult-omni external video player.

Can be:
  - an elisp function that takes a URL argument (e.g. mpv-pay-url)
  - a string for external command line program"
  :type '(choice (string :tag "(Default) mpv executable command" (executable-find "mpv"))
                 (function :tag "play with mpv package" mpv-play-url)
                 (function :tag "Custom Function" function)
                 (string :tag "Custom Executable Command" string)))

(defun consult-omni-play-url-with-app (url)
  "Plays video at URL with `consult-omni-embark-video-default-player'."
  (interactive (let* ((cand (consult-omni-youtube nil "Search Youtube:  " t))
                      (link (get-text-property 0 :url cand)))
                 (list link)))
  (cond
   ((stringp consult-omni-embark-video-default-player)
    (if-let ((cmd (executable-find consult-omni-embark-video-default-player)))
        (progn
          (start-process "consult-omni-mpv" nil cmd url)
          (message "Opening with %s ..." consult-omni-embark-video-default-player))
      (message "executable %s not found")))
   ((symbolp consult-omni-embark-video-default-player)
    (if (functionp consult-omni-embark-video-default-player)
        (progn (funcall consult-omni-embark-video-default-player url)
               (message "Opening with %s ..." consult-omni-embark-video-default-player))
      (message "Symbol function definition is void: %s"  consult-omni-embark-video-default-player)))))

(defun consult-omni-embark-video-play-with-app (cand)
  "Open CAND's video URL with `consult-omni-play-url-with-app'."
  (if-let* ((url (and (stringp cand) (get-text-property 0 :url cand))))
      (consult-omni-play-url-with-app url)))

(defvar-keymap consult-omni-embark-video-actions-map
  :doc "Keymap for consult-omni-embark-video"
  :parent consult-omni-embark-general-actions-map
  "o x" #'consult-omni-embark-video-play-with-app)

(add-to-list 'embark-keymap-alist '(consult-omni-video . consult-omni-embark-video-actions-map))

(add-to-list 'embark-default-action-overrides '(consult-omni-video . consult-omni-embark-default-action))

;;; Provide `consul-web-embark' module

(provide 'consult-omni-embark)

;;; consult-omni-embark.el ends here
