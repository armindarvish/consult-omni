;;; consult-omni-embark.el --- Embark Actions for `consult-omni' -*- lexical-binding: t -*-

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
;; provides Embark actions for consult-omni
;;; Code:

;;; Requirements

(require 'consult-omni)
(require 'embark-consult)

;;; User Options (a.k.a. Custom Variables)
;;  The following user options modify the behavior of consult-omni-embark.

(defcustom consult-omni-embark-default-term  #'eshell
  "Consult-omni default terminal to use in embark actions."
  :group 'consult-omni
  :type '(choice (function :tag "(Default) eshell" eshell)
                 (function :tag "shell" shell)
                 (function :tag "term" term)
                 (function :tag "ansi-term" ansi-term)
                 (function :tag "vterm" vterm)
                 (function :tag "eat" eat)
                 (function :tag "Custom Function" function)))

;;; Define Embark Action Functions
;; Embark actions for general sources (e.g. search engines)

(defun consult-omni-embark-default-action (cand)
  "Call the default action on CAND.

Gets the default callback function from `consult-omni--sources-alist'."
  (let* ((source (and (stringp cand) (get-text-property 0 :source cand))))
    (funcall (consult-omni--get-source-prop source :on-callback) cand)))

(add-to-list 'embark-default-action-overrides '(consult-omni . consult-omni-embark-default-action))

(defun consult-omni-embark-insert-title (cand)
  "Insert the title of CAND at point."
  (if-let ((title (and (stringp cand) (get-text-property 0 :title cand))))
      (insert (format " %s " title))))

(defun consult-omni-embark-copy-title-as-kill (cand)
  "Copy the title of CAND to `kill-ring'."
  (if-let ((title (and (stringp cand) (get-text-property 0 :title cand))))
      (kill-new (string-trim title))))

(defun consult-omni-embark-insert-url-link (cand)
  "Insert the url link of CAND at point."
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
  "Copy the url of CAND to `kill-ring'."
  (if-let ((url (and (stringp cand) (get-text-property 0 :url cand))))
      (kill-new (format " %s " (string-trim url)))))

(defun consult-omni-embark-external-browse-link (cand)
  "Open the url of CAND with `consult-omni-default-browse-function'."
  (if-let* ((url (and (stringp cand) (get-text-property 0 :url cand))))
      (funcall consult-omni-default-browse-function url)))

(defun consult-omni-embark-alternate-browse-link (cand)
  "Open the url of CAND with `consult-omni-alternate-browse-function'."
  (if-let* ((url (and (stringp cand) (get-text-property 0 :url cand))))
      (funcall consult-omni-alternate-browse-function url)))

(defun consult-omni-embark-external-browse-search-link (cand)
  "Open the “search url” of CAND in the external browser.

The “search url” is the source search page withthe search term from CAND."
  (if-let* ((search-url (and (stringp cand) (get-text-property 0 :search-url cand))))
      (funcall #'browse-url search-url)))

(defun consult-omni-embark-show-preview (cand)
  "Open a preview of CAND.

Gets the preview function from `consult-omni--sources-alist'."
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

;;; Scholar
;; Embark actions for sources searching academic article

(defcustom consult-omni-embark-scholar-make-note-func  #'consult-omni-embark-scholar-default-note
  "Function for making note template for scholar articles."
  :group 'consult-omni
  :type '(choice (function :tag "(Default) Make Note with Title, Link, Journal, Authors... " consult-omni-embark-scholar-default-note)
                 (function :tag "Custom Function")))

(defun consult-omni-embark-scholar-external-browse-doi (cand)
  "Open the DOI url of CAND in external browser."
  (if-let* ((doi (and (stringp cand) (get-text-property 0 :doi cand))))
      (funcall #'browse-url (concat "https://doi.org/" doi))))

(defun consult-omni-embark-scholar-copy-doi-as-kill (cand)
  "Copy the doi of CAND to `kill-ring'."
(if-let* ((doi (and (stringp cand) (get-text-property 0 :doi cand))))
    (kill-new doi)))

(defun consult-omni-embark-scholar-copy-authors-as-kill (cand)
  "Copy the authors of CAND to `kill-ring'."
  (if-let ((authors (and (stringp cand) (get-text-property 0 :authors cand))))
      (kill-new (string-trim (format " %s " authors)))))

(defun consult-omni-embark-scholar-insert-authors (cand)
  "Insert the authors of CAND at point."
  (if-let ((authors (and (stringp cand) (get-text-property 0 :authors cand))))
      (insert (string-trim (mapconcat #'identity authors ", ")))))

(defun consult-omni-embark-scholar-default-note (cand)
  "Make note on CAND article.

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
       "-----"
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
  "Insert note snippet for CAND article.

Uses `consult-omni-embark-scholar-make-note-func' to make template."
  (insert (funcall consult-omni-embark-scholar-make-note-func cand)))

(defvar-keymap consult-omni-embark-scholar-copy-menu-map
  :doc "Keymap for copy-as-kill menu"
  :parent nil
  "a" '("authors" . consult-omni-embark-scholar-copy-authors-as-kill)
  "d" '("doi" . consult-omni-embark-scholar-copy-doi-as-kill))

(fset 'consult-omni-embark-scholar-copy-menu-map consult-omni-embark-scholar-copy-menu-map)

(defvar-keymap consult-omni-embark-scholar-insert-menu-map
  :doc "Keymap for insert menu"
  :parent nil
  "a" '("authors" . consult-omni-embark-scholar-insert-authors)
  "n" '("note" . consult-omni-embark-scholar-insert-note))

(fset 'consult-omni-embark-scholar-insert-menu-map consult-omni-embark-scholar-insert-menu-map)



(defvar-keymap consult-omni-embark-scholar-actions-map
  :doc "Keymap for consult-omni-embark-scholar"
  :parent consult-omni-embark-general-actions-map
  "o d" #'consult-omni-embark-scholar-external-browse-doi
  "i" '("insert" . consult-omni-embark-scholar-insert-menu-map)
  "w" '("kill" . consult-omni-embark-scholar-copy-menu-map))

(fset 'consult-omni-embark-scholar-actions-map consult-omni-embark-scholar-actions-map)

(add-to-list 'embark-keymap-alist '(consult-omni-scholar . consult-omni-embark-scholar-actions-map))

(add-to-list 'embark-default-action-overrides '(consult-omni-scholar . consult-omni-embark-default-action))

;;; Apps
;; Embark actions for sources searching applications

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
  "w p" #'consult-omni-embark-apps-copy-path-as-kill)

(add-to-list 'embark-keymap-alist '(consult-omni-apps . consult-omni-embark-apps-actions-map))
(add-to-list 'embark-default-action-overrides '(consult-omni-apps . consult-omni-embark-default-action))

;;; Calc
;; Embark actions for calc source

(defun consult-omni-embark-calc-copy-results-as-kill (cand)
  "Copy the results of the calculator, CAND, to `kill-ring'."
  (if-let ((results (and (stringp cand) (get-text-property 0 :title cand))))
      (kill-new (format " %s " results))))

(defun consult-omni-embark-calc-insert-results (cand)
  "Insert the results of the calculator, CAND, at point."
  (if-let (results (and (stringp cand) (get-text-property 0 :title cand)))
      (insert (format " %s " results))))

(defun consult-omni-embark-calc-copy-formula-as-kill (cand)
  "Copy the results of the calculator, CAND, to `kill-ring'."
  (if-let ((formula (and (stringp cand) (get-text-property 0 :query cand))))
      (kill-new (format " %s " formula))))

(defun consult-omni-embark-calc-insert-formula (cand)
  "Insert the results of the calculator, CAND, at point."
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

;;; Video
;; Embark actions for sources searching videos (e.g. Youtube)

(defcustom consult-omni-embark-video-default-player  (executable-find "mpv")
  "External video player for consult-omni.

Can be:
  - an elisp function that takes a URL argument \(e.g. mpv-pay-url\)
  - a string for external command line program"
  :group 'consult-omni
  :type '(choice (string :tag "(Default) mpv executable command" (executable-find "mpv"))
                 (function :tag "play with mpv package" mpv-play-url)
                 (function :tag "Custom Function")
                 (string :tag "Custom Executable Command")))

(defun consult-omni-play-url-with-app (url)
  "Play video at URL with `consult-omni-embark-video-default-player'."
  (interactive (let* ((cand (consult-omni-youtube nil "Search Youtube:  " t))
                      (link (get-text-property 0 :url cand)))
                 (list link)))
  (cond
   ((stringp consult-omni-embark-video-default-player)
    (if-let ((cmd (executable-find consult-omni-embark-video-default-player)))
        (progn
          (start-process "consult-omni-mpv" nil cmd url)
          (message "Opening with %s ..." consult-omni-embark-video-default-player))
      (message "executable %s not found" consult-omni-embark-video-default-player)))
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

;;; OpenAlex
;; Embark actions for openalex source

(defun consult-omni-embark-openalex-external-browse-pdf (cand)
  "Open the pdf url of CAND in external browser."
  (if-let* ((url (and (stringp cand) (get-text-property 0 :pdf-url cand))))
      (funcall #'browse-url (url-encode-url url))
    (message "No PDF link Available!")))

(defun consult-omni-embark-openalex-external-browse-works-by-entity (cand)
  "Open the pdf url of CAND in external browser."
  (let* ((entity (get-text-property 0 :entity cand)))
    (pcase entity
      ("authors"
       (if-let* ((id (and (stringp cand) (get-text-property 0 :id cand)))
                 (url (concat consult-omni-openalex-search-url "works?filter=authorships.author.id:" id)))
      (funcall #'browse-url (url-encode-url url))))
      ("works"
       (if-let* ((authors (and (stringp cand) (get-text-property 0 :authors cand)))
                 (id (consult--read authors
                                    :prompt "Select One of the Authors:"
                                    :lookup (apply-partially #'consult--lookup-prop :id)))
                 (url (concat consult-omni-openalex-search-url "works?filter=authorships.author.id:" id)))
      (funcall #'browse-url (url-encode-url url))))
      ("insitutions"
       (if-let* ((id (and (stringp cand) (get-text-property 0 :id cand)))
                  (url (concat consult-omni-openalex-search-url "works?filter=authorships.institutions.id:" id)))
                  (funcall #'browse-url (url-encode-url url)))))))


(defun consult-omni-embark-openalex-works-by-entity (cand)
  "Open the pdf url of CAND in external browser."
  (let* ((entity (get-text-property 0 :entity cand)))
    (pcase entity
      ("authors"
       (if-let* ((consult-omni-openalex-default-entity "works")
                 (id (and (stringp cand) (get-text-property 0 :id cand)))
                 (consult-omni-openalex-extra-params `(("filter" . ,(format "authorships.author.id:%s" id))))
                 (consult-omni-async-min-input 0)
                 (consult-omni-default-count 25))
           (consult-omni-openalex)))
      ("works"
       (if-let* ((consult-omni-openalex-default-entity "works")
                 (authors (and (stringp cand) (get-text-property 0 :authors cand)))
                 (id (consult--read authors
                                    :prompt "Select One of the Authors:"
                                    :lookup (apply-partially #'consult--lookup-prop :id)))
                  (consult-omni-openalex-extra-params `(("filter" . ,(format "authorships.author.id:%s" id))))
                  (consult-omni-async-min-input 0)
                  (consult-omni-default-count 25))
           (consult-omni-openalex)))
      ("institutions"
       (if-let* ((consult-omni-openalex-default-entity "works")
                 (id (and (stringp cand) (get-text-property 0 :id cand)))
                 (consult-omni-openalex-extra-params `(("filter" . ,(format "authorships.institutions.id:%s" id))))
                 (consult-omni-async-min-input 0)
                 (consult-omni-default-count 50))
           (consult-omni-openalex))))))

;;; Define Embark Keymaps

(defvar-keymap consult-omni-embark-openalex-actions-map
  :doc "Keymap for consult-omni-embark-openalex"
  :parent consult-omni-embark-scholar-actions-map
  "o P" #'consult-omni-embark-openalex-external-browse-pdf
  "o W" #'consult-omni-embark-openalex-external-browse-works-by-entity
  "o w" #'consult-omni-embark-openalex-works-by-entity)

(add-to-list 'embark-keymap-alist '(consult-omni-openalex . consult-omni-embark-openalex-actions-map))

(add-to-list 'embark-default-action-overrides '(consult-omni-openalex . consult-omni-embark-default-action))

;;; OpenAlex
;; Embark actions for openalex source

(defun consult-omni-embark-process-kill-process (cand)
  "Kill the process of CAND."
  (let* ((process (get-text-property 0 :process cand))
         (name (get-text-property 0 :title cand))
         (pid (get-text-property 0 :pid cand)))

    (when (and process
               (yes-or-no-p (format "Are you sure you want to kill \"%s (pis: %s)\"?" name pid)))
      (proced-send-signal "KILL" (list process)))))

(defun consult-omni-embark-process-terminate-process (cand)
  "Terminate the process of CAND."
  (let* ((process (get-text-property 0 :process cand))
         (name (get-text-property 0 :title cand))
         (pid (get-text-property 0 :pid cand)))
    (when (and process
               (yes-or-no-p (format "Are you sure you want to terminate \"%s (pid: %s)\"?" name pid)))
      (proced-send-signal "TERM" (list process)))))

(defun consult-omni-embark-process-send-signal (cand)
  "Send a signal from `proced-signal-list' to the process of CAND."
  (let* ((process (get-text-property 0 :process cand))
         (name (get-text-property 0 :title cand))
         (pid (get-text-property 0 :pid cand))
         (sig (consult--read proced-signal-list
                                :prompt "Select: "
                                :annotate (lambda (cand) (cdr (assoc cand proced-signal-list)))
                                :require-match t
                                :sort nil
                                :default "TERM")))
    (when (and process
               (yes-or-no-p (format "Are you sure you want to send %s to \"%s (pid: %s)\"?" sig name pid)))
  (proced-send-signal sig (list process)))))

;;; Define Embark Keymaps

(defvar-keymap consult-omni-embark-process-actions-map
  :doc "Keymap for consult-omni-embark-process"
  :parent consult-omni-embark-general-actions-map
  "k" #'consult-omni-embark-process-kill-process
  "t" #'consult-omni-embark-process-terminate-process
  "x" #'consult-omni-embark-process-send-signal)

(add-to-list 'embark-keymap-alist '(consult-omni-process . consult-omni-embark-process-actions-map))

(add-to-list 'embark-default-action-overrides '(consult-omni-process . consult-omni-embark-default-action))

;;; Provide `consul-web-embark' module

(provide 'consult-omni-embark)

;;; consult-omni-embark.el ends here
