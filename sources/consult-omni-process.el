;;; consult-omni-process.el --- Consulting Processes -*- lexical-binding: t -*-

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

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; consult-omni-projects enables searching projects directly in Emacs
;; minibuffer using consult-omni.  It uses projects.el as the backend
;; and provides commands to find projects, switch to them or create new
;; ones using consult-omni as the frontend.

;;; Code:

(require 'proced)
(require 'consult-omni)

(defcustom consult-omni-process-backend "proced"
  "Backend to use for listing processes.

Can be either:
  \='proced  uses `proced'
  \='ps  uses command line program ps"
  :group 'consult-omni
  :type '(choice (string :tag "(Default) proced" "proced")
                 (string :tag "Use ps" "ps")))

(defcustom consult-omni-process-grep-ignore-case t
  "Whether to ignore case when using “grep” with “ps”."
  :group 'consult-omni
  :type 'boolean)

(defcustom consult-omni-process-cpu-indicator "%C"
  "String to show in annotation for %CPU."
  :group 'consult-omni
  :type '(string :tag "(Default) %C" "%C"))

(defcustom consult-omni-process-mem-indicator "%M"
  "String to show in annotation for %MEM."
  :group 'consult-omni
  :type '(string :tag "(Default) %M" "%M"))


(defcustom consult-omni-process-medium-cpu-threshold 20
  "Percent of cpu usage to use warning face.

Above this percent, the cpu annotaiton in `consult-omni-process' is
formatted with `consult-omni-warning-face'."
  :group 'consult-omni
  :type '(integer :tag "(Default) 20" 20))

(defcustom consult-omni-process-high-cpu-threshold 60
  "Percent of cpu usage to use error face.

Above this percent, the cpu annotaiton in `consult-omni-process' is
formatted with `consult-omni-error-face'."
  :group 'consult-omni
  :type '(integer :tag "(Default) 60" 60))

(defcustom consult-omni-process-medium-memory-threshold 20
  "Percent of memory usage to use warning face.

Above this percent, the memory annotaiton in `consult-omni-process' is
formatted with `consult-omni-warning-face'."
  :group 'consult-omni
  :type '(integer :tag "(Default) 20" 20))

(defcustom consult-omni-process-high-memory-threshold 60
  "Percent of memory usage to use error face.

Above this percent, the memory annotaiton in `consult-omni-process' is
formatted with `consult-omni-error-face'."
  :group 'consult-omni
  :type '(integer :tag "(Default) 60" 60))

(cl-defun consult-omni--process-format-candidate (&rest args &key source query pid title user cmd state cpu mem rss face
&allow-other-keys)
  "Format candidates from `consult-omni-proced' with ARGS.

Description of Arguments:

  SOURCE a string; the source name (e.g. “Proced”)
  QUERY  a string; the query input from the user
  PID    an ineger; pid of the process
  TITLE  a string; command name of the process
  USER   a string; owener of the process
  CMD    a string; comamnd arguments of the process
  STATE  a string; process state code, such as S, R, or T
  CPU    a float; percents of CPU time used by the process
  MEM    a float; percents of total physical memory used
         by process's resident set
  RSS    a number; resident set size of the process in KB's
  FACE   the face to apply to TITLE"

  (let* ((frame-width-percent (floor (* (frame-width) 0.1)))
         (source (if (stringp source) (propertize source 'face 'consult-omni-source-type-face) nil))
         (match-str (if (and (stringp query) (not (equal query ".*"))) (consult--split-escaped query) nil))
         (face (or (consult-omni--get-source-prop source :face) face 'consult-omni-default-face))
         (pid (if (numberp pid) (number-to-string pid) pid))
         (pid (if (stringp pid) (propertize pid 'face 'consult-omni-keyword-face)))
         (cpuface (if (numberp cpu)
                      (cond
                       ((> cpu consult-omni-process-high-cpu-threshold) 'consult-omni-error-face)
                       ((> cpu consult-omni-process-medium-cpu-threshold) 'consult-omni-warning-face)
                       (t 'consult-omni-success-face))
                    'default))
         (cpu (if (numberp cpu) (format "%.0f %s" cpu consult-omni-process-cpu-indicator) cpu))
         (cpu (if (stringp cpu) (propertize cpu 'face cpuface)))
         (memface (if (numberp mem)
                      (cond
                       ((> mem consult-omni-process-high-memory-threshold) 'consult-omni-error-face)
                       ((> mem consult-omni-process-medium-memory-threshold) 'consult-omni-warning-face)
                       (t 'consult-omni-success-face))
                    'default))
         (mem (if (numberp mem) (format "%.0f %s" mem consult-omni-process-mem-indicator) mem))
         (mem (if (stringp mem) (propertize mem 'face memface)))
         (rss (if (numberp rss) (file-size-human-readable (* rss 1024)) rss))
         (rss (if (stringp rss) (propertize rss 'face 'consult-omni-annotation-1-face)))
         (user (if (stringp user) (propertize user 'face 'consult-omni-date-face)))
         (cmd (if (stringp cmd) (propertize (string-replace "\n" " " cmd) 'face 'consult-omni-path-face)))
         (title-str (and (stringp title) (propertize title 'face face)))
         (title-str (and (stringp title-str) (consult-omni--set-string-width title-str (* 3 frame-width-percent))))
         (cmd-str (and (stringp cmd) (consult-omni--set-string-width cmd (* 5 frame-width-percent) 0)))
         (state  (and (stringp state) (propertize state 'face 'consult-omni-annotation-3-face)))
         (str (if title-str (concat title-str
                                    (and pid (concat "\t" pid))
                                    (and user (concat "\s\s" user))
                                    (and cpu (format "\s\s%s" cpu))
                                    (and mem (format "\s%s" mem))
                                    (and rss (format "\s%s" rss))
                                    (and state (concat "\s\s" state))
                                    (and cmd-str (concat "\s\s" cmd-str))
                                    (and source (concat "\t" source))))))
    (if (and str consult-omni-highlight-matches-in-minibuffer)
        (cond
         ((listp match-str)
          (mapc (lambda (match) (setq str (consult-omni--highlight-match match str t))) match-str))
         ((stringp match-str)
          (setq str (consult-omni--highlight-match match-str str t)))))
    str))

(defun consult-omni--process-default-preview (cand)
  "Preview function for CAND from `consult-omni-proced'."
  (let* ((pid (get-text-property 0 :pid cand))
         (buff (get-buffer-create "*consult-omni-proced*")))
      (with-current-buffer buff
        (proced-mode)
        (if (assoc 'omni proced-filter-alist)
            (setf (alist-get 'omni proced-filter-alist)
                  (list (cons 'pid `(lambda (attr)
 (equal attr ,pid)))))
          (add-to-list 'proced-filter-alist (cons 'omni (list (cons 'pid `(lambda (attr)
 (equal attr ,pid)))))))
        (setq-local proced-process-alist nil)
        (setq-local proced-filter 'omni)
        (setq-local proced-format 'verbose)
        (proced-update t)
        (funcall (consult--buffer-preview) 'preview
                 buff))))

(defun consult-omni-process-ps-list-processes (query &optional user state sort)
  "Return a list of attributes for system processes matching QUERY."
  (let* ((ps-cmd (concat "ps -a -x"
                          (pcase sort
                            ("cpu" " -r")
                            ("mem" " -m")
                            (_ " -r"))
                          (if user (format "-u %s" user))
                          (if state (format "--state %s" state))
                          " -o user -o pid -o pcpu -o pmem -o vsize -o rss -o state -o stime -o utime -o flags -o ucomm -o command"))
         (grep-exec (if (executable-find "rg") "rg" "grep"))
         (grep-cmd (and query (not (string-empty-p query)) (concat (format "%s --color=never" grep-exec) (if consult-omni-process-grep-ignore-case " --ignore-case") (format " \"%s\"" query))))
         (command (concat ps-cmd (if grep-cmd (concat " | " grep-cmd))))
         (output (split-string (string-trim (shell-command-to-string command)) "[\r\n]" t "[ \f\t\n\r\v]+")))
    (when (listp output)
      (cl-loop for row in (if query output (cdr output))
               collect
               (let* ((parts (split-string row " " t))
                      (user (car parts))
                      (pid (string-to-number (cadr parts)))
                      (pcpu (string-to-number (cadr (cdr parts))))
                      (pmem (string-to-number (cadr (cddr parts))))
                      (vsize (string-to-number (cadr (cdddr parts))))
                      (rss (string-to-number (cadr (cddddr parts))))
                      (state (cadr (cdr (cddddr parts))))
                      (stime (cadr (cddr (cddddr parts))))
                      (utime (cadr (cdddr (cddddr parts))))
                      (flags (cadr (cddddr (cddddr parts))))
                      (comm (cadr (cdr (cddddr (cddddr parts)))))
                      (args (mapconcat #'identity (cddr (cdr (cddddr (cddddr parts)))) " ")))
                 (unless (or (equal args command)
                             (equal args ps-cmd)
                             (equal args grep-cmd)
                             (equal args (concat (format "%s --color=never" grep-exec) (if consult-omni-process-grep-ignore-case " --ignore-case") (format " %s" query)))
                             (equal args (concat (format "%s -c %s" (getenv "SHELL")
                                                              command)))
                             (equal args (concat (format "%s -c %s" explicit-shell-file-name
                                                              command))))
                   (cons pid (list (cons 'user user)
                                   (cons 'pid pid)
                                   (cons 'comm comm)
                                   (cons 'pcpu pcpu)
                                   (cons 'pmem pmem)
                                   (cons 'vsize vsize)
                                   (cons 'rss rss)
                                   (cons 'state state)
                                   (cons 'stime stime)
                                   (cons 'utime utime)
                                   (cons 'flags flags)
                                   (cons 'args args)))))))))

(defun consult-omni-process-proced-list-processes (query &optional user state sort)
  "Return alist of attributes for system processes matching QUERY."
  (let* ((filter-list (append (if (and query (not (string-empty-p query)))
                                    (list (cons 'comm (format ".*%s.*" query))))
                                (if user (list (cons 'user (format ".*%s.*" user))))
                                (if state (list (cons 'state (format  "\\`[%s]\\'" state)))
                                  (list (cons 'state "\\`[Rr]\\'")))))
         (sorter (pcase sort
                   ("cpu" 'pcpu)
                   ("mem" 'pmem)
                   (_ 'pcpu))))
    (proced-sort (proced-filter (proced-process-attributes) filter-list) sorter t)))

(cl-defun consult-omni--process-fetch-results (input &rest args &key callback &allow-other-keys)
  "Fetch list of processes matching INPUT with ARGS.

CALLBACK is a function used internally to update the list of candidates in
the minibuffer asynchronously.  It is called with a list of strings, which
are new annotated candidates \(e.g. as they arrive from an asynchronous
process\) to be added to the minibuffer completion cnadidates.  See the
section on REQUEST in documentation for `consult-omni-define-source' as
well as the function
`consult-omni--multi-update-dynamic-candidates' for how CALLBACK is used."
  (pcase-let* ((`(,query . ,opts) (consult-omni--split-command input (seq-difference args (list :callback callback))))
               (completion-ignore-case t)
               (opts (car-safe opts))
               (user (plist-get opts :user))
               (state (plist-get opts :state))
               (sort (plist-get opts :sort))
               (processes (pcase consult-omni-process-backend
                            ("ps" (consult-omni-process-ps-list-processes query user state sort))
                            ("proced" (consult-omni-process-proced-list-processes query user state sort)))))
    (when (listp processes)
    (delq nil (cl-loop for proc in processes
                       collect (let* ((source "Process")
                                      (attributes (cdr proc))
                                      (pid (map-elt attributes 'pid))
                                      (comm (map-elt attributes 'comm))
                                      (args (map-elt attributes 'args))
                                      (user (map-elt attributes 'user))
                                      (state (map-elt attributes 'state))
                                      (cpu (map-elt attributes 'pcpu))
                                      (mem (map-elt attributes 'pmem))
                                      (rss (map-elt attributes 'rss))
                                      (decorated (consult-omni--process-format-candidate :source source :query query :pid pid :title comm :user user :cmd args :state state :cpu cpu :mem mem :rss rss)))
                                 (when (stringp decorated) (add-text-properties 0 1 `(:source ,source :query ,query :pid ,pid :title ,comm :user ,user :cmd ,args :state ,state :cpu ,cpu :mem ,mem :rss ,rss :process ,proc) decorated))
                                 decorated))))))

;; Define the Proced source
(consult-omni-define-source "Process"
                            :narrow-char ?x
                            :type 'sync
                            :require-match nil
                            :category 'consult-omni-process
                            ;; :face 'consult-omni-default-face
                            :request #'consult-omni--process-fetch-results
                            :min-input 0
                            :on-preview #'consult-omni--process-default-preview
                            :on-callback #'consult-omni--process-default-preview
                            :preview-key consult-omni-preview-key
                            :search-hist 'consult-omni--search-history
                            :select-hist 'consult-omni--selection-history
                            :enabled (lambda () (fboundp 'proced-process-attributes))
                            :group #'consult-omni--group-function
                            :sort nil
                            :interactive consult-omni-intereactive-commands-type
                            :annotate nil)

;;; provide `consult-omni-process' module

(provide 'consult-omni-process)

(add-to-list 'consult-omni-sources-modules-to-load 'consult-omni-process)
;;; consult-omni-process.el ends here
