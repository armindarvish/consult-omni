;;; consult-omni-apps.el --- Consulting OS applications -*- lexical-binding: t -*-

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
(require 'xdg)

(defcustom consult-omni-apps-paths (list)
  "List of paths to directories containing applications.
"
  :type '(repeat :tag "List of paths" directory))

(defcustom consult-omni-apps-use-cache nil
"Whether to use cache for getting list of apps?"
:type 'boolean)

(defcustom consult-omni-apps-open-command-args nil
  "Command line args to open an application"
  :type 'string)

(defcustom consult-omni-apps-regexp-pattern ""
"Regexp pattern to find system applications"
:type 'regexp)



(defcustom consult-omni-apps-default-launch-function #'consult-omni--apps-lauch-app
  "consult-omni default function to launch an app"
  :type '(choice (function :tag "(Default) Use System Shell" consult-omni--apps-lauch-app)
                 (function :tag "Custom Function")))

(pcase system-type
  ('darwin
   (setq consult-omni-apps-paths (append (file-expand-wildcards "/Applications/Adobe*") (list "/Applications" "/Applications/Utilities/" "/System/Applications/" "/System/Applications/Utilities/" "~/Applications/")))
   (setq consult-omni-apps-regexp-pattern ".*\\.app$")
   (setq consult-omni-apps-open-command-args "open -a")
   )
   ('gnu/linux
    (setq consult-omni-apps-xdg-data-home (if (fboundp 'xdg-data-home) (xdg-data-home)
                                           (let ((path (getenv "XDG_DATA_HOME")))
                                             (if (or (null path) (string= path ""))
                                                 nil
                                               (parse-colon-path path)))))
     (setq consult-omni-apps-xdg-data-dirs (if (fboundp 'xdg-data-dirs) (xdg-data-dirs)
                                           (let ((path (getenv "XDG_DATA_DIRS")))
                                             (if (or (null path) (string= path ""))
    nil
                                               (parse-colon-path path)))))
     (setq consult-omni-apps-paths (remove nil (mapcar (lambda (dir)
                                       (let ((path (and (stringp dir) (file-exists-p dir) (file-truename (expand-file-name "applications" dir)))))
                                              (and (stringp path) path)))
                                          (list consult-omni-apps-xdg-data-home
                                               consult-omni-apps-xdg-data-dirs
                                               "/usr/share"
                                               "/usr/local/share"))))
     (setq consult-omni-apps-regexp-pattern ".*\\.desktop$")
     (setq consult-omni-apps-open-command-args "gtk-launch")
    )
)

(defvar consult-omni-apps-cached-apps nil)

(defvar consult-omni-apps-cached-items nil)

(defun consult-omni--apps-cmd-args (app &optional file)
  (append (consult--build-args consult-omni-apps-open-command-args)
          (list (format "%s" app))
          (if (and file (file-exists-p (file-truename file))) (list (format "%s" file)))))

(defun consult-omni--apps-lauch-app (app &optional file)
  (let* ((name (concat "consult-omni-" (file-name-base app)))
         (cmds (consult-omni--apps-cmd-args app file)))
    (make-process :name name
                :connection-type 'pipe
                :command cmds
                )))

(defun consult-omni--apps-preview (cand)
  "Mdfind preview function."
(funcall (consult--file-preview) 'preview cand))

(defun consult-omni--apps-callback (cand)
  "Mdfind callback function."
  (let ((app (get-text-property 0 :app cand)))
    (funcall consult-omni-apps-default-launch-function app)
  ))

(cl-defun consult-omni--apps-format-candidates (&rest args &key source query title path face snippet visible &allow-other-keys)
"Formats the cnaiddates of `consult-omni-apps.

Files are entries from `consult-omni--apps-list-apps'.
QUERY is the query input from the user"
  (let* ((frame-width-percent (floor (* (frame-width) 0.1)))
         (source (and (stringp source) (propertize source 'face 'consult-omni-source-type-face)))
         (directory (and path (file-name-directory path)))
         (directory (and (stringp directory) (propertize directory 'face 'consult-omni-path-face)))
         ;; (filename (and path (file-name-nondirectory path)))
         ;; (filename (and (stringp filename) (propertize filename 'face 'consult-omni-path-face)))
         (snippet (and (stringp snippet) (consult-omni--set-string-width snippet (* 3 frame-width-percent))))
         (snippet (and (stringp snippet) (propertize snippet 'face 'consult-omni-snippet-face)))
         (match-str (and (stringp query) (consult--split-escaped query) nil))
         (face (or (consult-omni--get-source-prop source :face) face 'consult-omni-files-source-face))
         (title-str (propertize title 'face face))
         (title-str (consult-omni--set-string-width title-str (* 4 frame-width-percent)))
         (str (concat title-str
                      (unless visible "\s[Hidden App]")
                      (when snippet (concat "\t" snippet))
                      (when directory (concat "\t" directory))
                      ;; (when filename (concat "\t" (when directory directory) filename))
                      (when source (concat "\t" source))
                      )))
     (if consult-omni-highlight-matches
        (cond
         ((listp match-str)
          (mapcar (lambda (match) (setq str (consult-omni--highlight-match match str t))) match-str))
         ((stringp match-str)
          (setq str (consult-omni--highlight-match match-str str t)))))
    str))

(defun consult-omni--apps-get-desktop-apps ()
  "Return a list of files for system apps

Finds all files that match `consult-omni-apps-regexp-pattern'
in `consult-omni-apps-paths'.
"
  (if (and consult-omni-apps-use-cache consult-omni-apps-cached-apps)
      consult-omni-apps-cached-apps
(let ((paths (if (stringp consult-omni-apps-paths)
                   (list consult-omni-apps-paths)
                 consult-omni-apps-paths)))
    (when (listp paths)
      (setq consult-omni-apps-cached-apps (cl-remove-duplicates (apply #'append (mapcar (lambda (path)
                          (when (file-exists-p path)
                          (directory-files path t consult-omni-apps-regexp-pattern t))) paths))))))))

(setq consult-omni-apps-cached-apps (consult-omni--apps-get-desktop-apps))

(defun consult-omni--apps-parse-app-file (file)
  (pcase system-type
         ('darwin
          (let ((name (file-name-base file))
                (comment nil)
                (exec (consult-omni--apps-cmd-args (file-name-nondirectory file))))
            (list name comment exec t)))
         ('gnu/linux
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            (let ((start (re-search-forward "^\\[Desktop Entry\\] *$" nil t))
                  (end (re-search-forward "^\\[" nil t))
                  (visible t)
                  name comment exec)
              (catch 'break
                (unless start
                  (throw 'break nil))

                (goto-char start)
                (when (re-search-forward "^\\(Hidden\\|NoDisplay\\) *= *\\(1\\|true\\) *$" end t)
                  (setq visible nil))
                (setq name (match-string 1))

                (goto-char start)
                (unless (re-search-forward "^Type *= *Application *$" end t)
                  (throw 'break nil))
                (setq name (match-string 1))

                (goto-char start)
                (unless (re-search-forward "^Name *= *\\(.+\\)$" end t)
                  (throw 'break nil))
                (setq name (match-string 1))

                (goto-char start)
                (when (re-search-forward "^Comment *= *\\(.+\\)$" end t)
                  (setq comment (match-string 1)))

                (goto-char start)
                (unless (re-search-forward "^Exec *= *\\(.+\\)$" end t)
                  ;; Don't warn because this can technically be a valid desktop file.
                  (throw 'break nil))
                (setq exec (match-string 1))

                (goto-char start)
                (when (re-search-forward "^TryExec *= *\\(.+\\)$" end t)
                  (let ((try-exec (match-string 1)))
                    (unless (locate-file try-exec exec-path nil #'file-executable-p)
                      (throw 'break nil))))

                  (list name comment exec visible)))))))

(defun consult-omni-apps--cached-items (files query)
(if (and consult-omni-apps-use-cache consult-omni-apps--cached-items)
    consult-omni-apps-cached-items
(setq consult-omni-apps-cached-items
 (mapcar (lambda (file)
             (pcase-let* ((source "Apps")
                          (`(,name ,comment ,exec ,visible) (consult-omni--apps-parse-app-file file))
                    (title (or name (file-name-base file) ""))
                    (app (and (stringp file) (file-exists-p file) (file-name-nondirectory file)))
                    (search-url nil)

                    (decorated (funcall #'consult-omni--apps-format-candidates :source source :query query :title title :path file :snippet comment :visible visible)))
               (propertize decorated
                           :source source
                           :title title
                           :url nil
                           :search-url nil
                           :query query
                           :snippet comment
                           :path file
                           :exec exec
                           :app app)))
           (if query
               ;; (seq-filter (lambda (file) (string-match (concat ".*" query ".*") file nil t)) files)
               (cl-remove-if-not (lambda (file) (string-match (concat ".*" query ".*") file nil t)) files)
             files)
   ))))

(setq consult-omni-apps--cached-items  (consult-omni-apps--cached-items consult-omni-apps-cached-apps ".*"))

(cl-defun consult-omni--apps-list-apps (input &rest args &key callback &allow-other-keys)
  "get a list of applications from OS.
"
 (pcase-let* ((`(,query . ,opts) (consult-omni--split-command input (seq-difference args (list :callback callback))))
               (opts (car-safe opts))
               (count (plist-get opts :count))
               (count (or (and count (integerp (read count)) (string-to-number count))
                             consult-omni-default-count))
               (files (consult-omni--apps-get-desktop-apps)))
   (if (and consult-omni-apps-use-cache query)
       (seq-filter (lambda (file) (string-match (concat ".*" query ".*") file nil t)) consult-omni-apps-cached-items)
   (mapcar (lambda (file)
             (pcase-let* ((source "Apps")
                          (`(,name ,comment ,exec ,visible) (consult-omni--apps-parse-app-file file))
                    (title (or name (file-name-base file) ""))
                    (app (and (stringp file) (file-exists-p file) (file-name-nondirectory file)))
                    (search-url nil)
                    (decorated (funcall #'consult-omni--apps-format-candidates :source source :query query :title title :path file :snippet comment :visible visible)))
               (propertize decorated
                           :source source
                           :title title
                           :url nil
                           :search-url nil
                           :query query
                           :snippet comment
                           :path file
                           :exec exec
                           :app app)))
           (if query
               ;; (seq-filter (lambda (file) (string-match (concat ".*" query ".*") file nil t)) files)
             (cl-remove-if-not (lambda (file) (string-match (concat ".*" query ".*") file nil t)) files)
             files)
   )
 )))

(consult-omni-define-source "Apps"
                           :narrow-char ?a
                           :type 'sync
                           :request #'consult-omni--apps-list-apps
                           :on-preview #'ignore
                           :on-return #'identity
                           :on-callback #'consult-omni--apps-callback
                           :preview-key consult-omni-preview-key
                           :search-history 'consult-omni--search-history
                           :selection-history 'consult-omni--selection-history
                           :enabled (lambda () (boundp 'consult-omni-apps-paths))
                           :group #'consult-omni--group-function
                           :sort t
                           :static 'both
                           :annotate nil
                           :category 'file
                           )

;;; provide `consult-omni-apps module

(provide 'consult-omni-apps)

(add-to-list 'consult-omni-sources-modules-to-load 'consult-omni-apss)
;;; consult-omni-apps.el ends here
