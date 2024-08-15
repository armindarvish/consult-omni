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

(defcustom consult-omni-projects-vc-backend 'vc
"Backend to use for git operations

Can be either:
  'vc  uses built-in vc library
  'magit  uses magit"
:type '(choice (const :tag "(Default) Use built-in vc" vc)
               (const :tag "Use Magit" magit)))

(defcustom consult-omni-projects-default-projects-folder nil
"Parent directory for making new projects"
:type 'directory)


(defcustom consult-omni-projects-default-fallback-switch-command #'dired
"Function to call when project-switch-project is not available

The function should accept one argument, a project directory"
:type '(choice (function :tag "Open directory in dired" dired)
               (function :tag "Custom Function")))


(defcustom consult-omni-projects-preview-func #'consult-omni--projects-default-preview
"Function to call for previewing projects

The function should accept one argument, a project candidate
(e.g. propertized string from `consult-omni-projects')."
:type '(choice (function :tag "Open project root in dired" consult-omni--projects-default-preview)
               (function :tag "Custom Function")))

(defcustom consult-omni-projects-callback-func #'consult-omni--projects-default-callback
"Function to call when selecting a project

The function should accept one argument, a project candidate
(e.g. propertized string from `consult-omni-projects')."
:type '(choice (function :tag "Call project-switch-project with project root" consult-omni--projects-default-callback)
               (function :tag "Custom Function")))

(defcustom consult-omni-projects-new-func #'consult-omni--projects-make-new-project-dwim
"Function to call when selecting a new non-existinng project

The function should accept one argument, a project candidate
(e.g. propertized string from `consult-omni-projects')."
:type '(choice (function :tag "(Default) Add existing folder or make a new project folder" consult-omni--projects-make-new-project-dwim)
               (function :tag "Find and add existing project folder" consult-omni--projects-add-project-to-list)
               (function :tag "Make a new project folder" consult-omni--projects-make-new-project)
               (function :tag "Custom Fucntion")))

(defcustom consult-omni-projects-create-new-func #'consult-omni--projects-create-new-git-project
"Function to use for maing new projects.

The function should accept one argument, a string with
the directory path where the new project will be created."
:type '(choice (function :tag "(Default) Use magit-init if availbale otherwise fall back to make-directory" consult-omni--projects-create-new-git-project)
               (function :tag "Make a new Git directory with magit" magit-init)
               (function :tag "Make directory" make-directory)
               (function :tag "Custom Fucntion")))

(defvar consult-omni--projects-create-new-project-hook nil
"Function(s) called after `consult-omni-projects-create-new-func'.

The return of `onsult-omni-projects-default-new-func'
 is passed to these functions as input arg.")

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
        (funcall consult-omni-projects-default-fallback-switch-command dir))))

(defun consult-omni--projects-add-project-to-list (cand)
  "Add the missing project CAND to `project--list'."
  (let* ((dir (read-directory-name (format "Find Project Directory for %s: " (propertize cand 'face 'font-lock-keyword-face)) default-directory nil t))
         (project (project--find-in-directory dir))
         (root (and project (project-root project))))
    (when (file-exists-p dir) (project--remember-dir dir)
          (if root (project-switch-project root)
            (funcall consult-omni-projects-default-fallback-switch-command dir)))
dir))

(defun consult-omni--projects-create-new-git-project (dir)
  "Makes a new Git project at DIR"
  (pcase consult-omni-projects-vc-backend
    ('vc
     (if (and (featurep 'vc-git) (require 'vc-git nil nil))
         (progn (make-directory dir t)
                (if-let ((default-directory dir)
                         (cmd (executable-find "git")))
                    (or (and (file-expand-wildcards (expand-file-name ".git" dir))
                             (y-or-n-p "There is already a .git folder there, do you want to re-initialize?")
                             (vc-git-create-repo))
                        (vc-git-create-repo))))
       (message "vc not available. Change `consult-omni-projects-vc-backend' to use a different backend!"))
     (funcall consult-omni-projects-default-fallback-switch-command dir))
    ('magit
     (if (and (featurep 'magit) (require 'magit nil nil))
         (or (and (file-expand-wildcards (expand-file-name ".git" dir))
                  (y-or-n-p "There is already a .git folder there, do you want to re-initialize?")
                  (funcall-interactively #'magit-init dir))
             (funcall-interactively #'magit-init dir))
       (message "Magit not available. Change `consult-omni-projects-vc-backend' to use a different backend!"))
     (funcall consult-omni-projects-default-fallback-switch-command dir)))
  dir)

(defun consult-omni--projects-make-new-project (cand)
  "Make new project for project name CAND."
  (let* ((dir (read-directory-name (concat "Select Parent Directory for %s: " (propertize cand 'face 'font-lock-keyword-face)) (or (and consult-omni-projects-default-projects-folder (file-name-as-directory consult-omni-projects-default-projects-folder)) default-directory) nil nil))
        (name (read-string (concat "Project Name for %s: " (propertize cand 'face 'font-lock-keyword-face)) cand))
        (target (expand-file-name (file-name-as-directory name) dir)))
      (funcall consult-omni-projects-create-new-func target)
      (when (file-exists-p target) (project--remember-dir target))
target))

(defun consult-omni--projects-make-new-project-dwim (cand)
 "Add existing ro make new project for CAND."
  (let* ((dir (read-directory-name (format "Select Directory for %s: " (propertize cand 'face 'font-lock-keyword-face)) (or (and consult-omni-projects-default-projects-folder (file-name-as-directory consult-omni-projects-default-projects-folder)) default-directory) nil t))
         (project (project--find-in-directory dir))
         (root (and project (project-root project)))
         (existing  (if project
                        (y-or-n-p "There is an existing project in that directory. Do you want to open that project?")))
         (new (unless existing (y-or-n-p (format "Do you want to make a new project under %s?" dir)))))
    (cond
      (existing
       (when (file-exists-p dir) (project--remember-dir dir)
             (if root (project-switch-project root)
               (funcall consult-omni-projects-default-fallback-switch-command dir))))
      (new
       (let* ((name (read-string (format "Project Name for %s: " (propertize cand 'face 'font-lock-keyword-face)) cand))
        (target (expand-file-name (file-name-as-directory name) dir)))
       (funcall consult-omni-projects-create-new-func target)
      (when (file-exists-p target)
        (project--remember-dir target)
        (setq dir target)))))
dir))

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
                (decorated (consult-omni--projects-format-candidate :source source :title title  :name name :nfiles nfiles :size size)))
            (when (stringp decorated) (add-text-properties 0 1 `(:source ,source :title ,title :query ,query :project ,project :dir ,title :name ,name) decorated))
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
                           :on-preview (lambda (cand) (funcall consult-omni-projects-preview-func cand))
                           :on-callback (lambda (cand) (funcall consult-omni-projects-callback-func cand))
                           :on-new (lambda (cand) (run-hook-with-args 'consult-omni-create-project-hook (funcall consult-omni-projects-new-func cand)))
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
