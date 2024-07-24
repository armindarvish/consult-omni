;;; consult-omni-projects.el --- Consulting project.el -*- lexical-binding: t -*-

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

(require 'project)
(require 'consult-omni)

(defcustom consult-omni--projects-default-preview-func #'consult-omni--projects-default-preview
""
:type '(choice (function :tag "Open project root in dired" consult-omni--projects-default-preview)
               (function :tag "Custom Function")))

(defcustom consult-omni--projects-default-callback-func #'consult-omni--projects-default-callback
""
:type '(choice (function :tag "Call project-switch-project with project root" consult-omni--projects-default-callback)
               (function :tag "Custom Function")))


(defcustom consult-omni--projects-default-new-func #'consult-omni--projects-default-new
""
:type '(choice (function :tag "Make a directory (and initiailze Git if magit-init is available)" consult-omni--projects-default-new)
               (function :tag "Custom Fucntion")))



(cl-defun consult-omni--projects-format-candidate (&rest args &key source query title name nfiles size face &allow-other-keys)
  "Returns a formatted string for candidates of `consult-omni-pubmed'.

Description of Arguments:

  SOURCE the name to use (e.g. “PubMed”)
  QUERY  query input from the user
  TITLE  the title of project
  DIR    the root directory of the project
  FACE   the face to apply to TITLE"
  (let* ((frame-width-percent (floor (* (frame-width) 0.1)))
         (source (if (stringp source) (propertize source 'face 'consult-omni-source-type-face) nil))
         (match-str (if (and (stringp query) (not (equal query ".*"))) (consult--split-escaped query) nil))
         (face (or (consult-omni--get-source-prop source :face) face 'consult-omni-default-face))
         (name (if (stringp name) (propertize name 'face 'consult-omni-keyword-face)))
         (nfiles (if (numberp nfiles) (propertize (format "%s Files" nfiles) 'face 'consult-omni-path-face)))
         (size (if (stringp size) (propertize (format "%s" size) 'face 'consult-omni-domain-face)))
         (title (and (stringp title) (file-exists-p title) title))
         (title-str (and (stringp title) (propertize title 'face face)))
         (title-str (and (stringp title-str) (consult-omni--set-string-width title-str (* 4 frame-width-percent))))
         (str (if title-str (concat title-str
                      (and name (concat "\t" name))
                      (and nfiles (concat "\s\s" nfiles))
                      (and nfiles (concat "\s\s" size))
                      (and source (concat "\t" source))))))
    (if (and str consult-omni-highlight-matches-in-minibuffer)
        (cond
         ((listp match-str)
          (mapcar (lambda (match) (setq str (consult-omni--highlight-match match str t))) match-str))
         ((stringp match-str)
          (setq str (consult-omni--highlight-match match-str str t)))))
    str))

(defun consult-omni--projects-default-preview (cand)
  "Preview function for `consult-omni-projects'."
  (if-let* ((dir (get-text-property 0 :title cand)))
      (dired dir)))

(defun consult-omni--projects-default-callback (cand)
  "Callback function for `consult-omni-projects'."
  (let* ((dir (get-text-property 0 :title cand))
         (project (get-text-property 0 :project cand))
         (root (and project (project-root project))))
      (if root (project-switch-project root)
        (dired dir))))

(defun consult-omni--projects-default-new (cand)
  "Callback function for `consult-omni-projects'."
  (let* ((dir (read-directory-name (concat "Select Parent Directory: " (propertize (format "%s: " cand) 'face 'font-lock-keyword-face)) default-directory nil nil))
        (name (read-string (concat "Project Name for " (propertize (format "%s: " cand) 'face 'font-lock-keyword-face)) cand))
        (target (expand-file-name name dir)))
      (if (and (featurep 'magit) (require 'magit nil nil))
          (progn
            (funcall-interactively #'magit-init target)
            (dired target))
        ((make-directory target t)
         (dired target)))
      (when (file-exists-p target) (project--remember-dir target))))

(cl-defun consult-omni--projects-fetch-results (input &rest args &key callback &allow-other-keys)
"Return list of projects matching INPUT in `project--list'."
(pcase-let* ((`(,query . ,opts) (consult-omni--split-command input (seq-difference args (list :callback callback))))
             (opts (car-safe opts)))
(delq nil (mapcar (lambda (item)
          (let* ((source "Projects")
                 (path (car item))
                 (project (and (stringp path) (file-exists-p path) (project--find-in-directory path)))
                 (title (or (and project (project-root project)) path))
                 (name (or (and project (project-name project)) (file-name-nondirectory path)))
                 (p-files (and project (project-files project)))
                 (nfiles (and (listp p-files) (length p-files)))
                (size (and (stringp title) (file-exists-p title) (file-size-human-readable (file-attribute-size (file-attributes title)))))
                (decorated (consult-omni--projects-format-candidate :source source :title title :project project :name name :nfiles nfiles :size size)))
            (when (stringp decorated) (add-text-properties 0 1 `(:source ,source :title ,title :query ,query :dir ,title :name ,name) decorated))
            decorated))
        (cl-remove-if-not (lambda (item) (string-match (format ".*%s.*" query) (car item))) project--list)))))

;; Define the Projects Source
(consult-omni-define-source "Projects"
                           :narrow-char ?P
                           :type 'sync
                           :require-match nil
                           :category 'project
                           ;; :face 'consult-omni-files-title-face
                           :on-setup #'project--read-project-list
                           :request #'consult-omni--projects-fetch-results
                           :on-preview (lambda (cand) (funcall consult-omni--projects-default-preview-func cand))
                           :on-callback (lambda (cand) (funcall consult-omni--projects-default-callback-func cand))
                           :on-new (lambda (cand) (funcall consult-omni--projects-default-new-func cand))
                           :preview-key consult-omni-preview-key
                           :search-hist 'consult-omni--search-history
                           :select-hist 'consult-omni--selection-history
                           :enabled (lambda () (bound-and-true-p project--list))
                           :group #'consult-omni--group-function
                           :sort t
                           :interactive consult-omni-intereactive-commands-type
                           :annotate nil)

;;; provide `consult-omni-projects' module

(provide 'consult-omni-projects)

(add-to-list 'consult-omni-sources-modules-to-load 'consult-omni-projects)
;;; consult-omni-projects.el ends here
