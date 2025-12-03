;;; consult-omni-mu4e.el --- Consulting Mu4e -*- lexical-binding: t -*-

;; Copyright (C) 2024 Armin Darvish

;; Author: Armin Darvish
;; Maintainer: Armin Darvish
;; Created: 2024
;; Version: 0.3
;; Package-Requires: (
;;         (emacs "29.4")
;;         (consult "2.0")
;;         (consult-mu "1.0")
;;         (consult-omni "0.3"))
;;
;; Homepage: https://github.com/armindarvish/consult-omni
;; Keywords: convenience

;;; Commentary:
;; consult-omni-mu4e enables searching emails in consult-omni.
;; It provides commands to search emails using consult-mu as the backend.
;;
;; For more info on consult-mu see:
;; URL `https://github.com/armindarvish/consult-mu'

;;; Code:

(require 'consult-omni)
(require 'consult-mu)

(defun consult-omni-mu--format-candidate (cand highlight)
  "Format CAND from `consult-omni-mu4e'.

Description of Arguments:

  CAND      a string; candidate from consult-mu
  HIGHLIGHT a boolean; when non-nil highlights the query term in
            the minibuffer"
  (let* ((string (car cand))
         (info (cadr cand))
         (msg (plist-get info :msg))
         (query (plist-get info :query))
         (match-str (if (and (stringp query) (not (equal query ".*"))) (consult--split-escaped (car (consult--command-split query))) nil))
         (headers-template (consult-mu--headers-template))
         (str (if headers-template
                  (consult-mu--expand-headers-template msg headers-template)
                string))
         (str (propertize str :msg msg :query query :type :dynamic :source "mu4e" :title string)))
    (if (and consult-mu-highlight-matches highlight)
        (cond
         ((listp match-str)
          (mapc (lambda (match) (setq str (consult-mu--highlight-match match str t))) match-str))
         ((stringp match-str)
          (setq str (consult-mu--highlight-match match-str str t))))
      str)
    (when msg
      (cons str (list :msg msg :query query :type :dynamic)))))

(defun consult-omni--mu-preview (cand)
  "Preview function for CAND from `consult-omni-mu4e'."
  (when-let* ((info (cdr (get-text-property 0 'multi-category cand)))
              (msg (plist-get info :msg))
              (query (plist-get info :query))
              (msgid (substring-no-properties (plist-get msg :message-id)))
              (match-str (car (consult--command-split query)))
              (match-str (car (consult--command-split query)))
              (mu4e-headers-buffer-name consult-mu-headers-buffer-name)
              (buffer consult-mu-view-buffer-name))
    (add-to-list 'consult-mu--view-buffers-list buffer)
    (funcall (consult--buffer-preview) 'preview
             (consult-mu--view msg t consult-mu-mark-previewed-as-read match-str))
    (with-current-buffer consult-mu-view-buffer-name
      (unless (one-window-p) (delete-other-windows)))))

(defun consult-omni--mu-return (cand)
  "Return function for CAND from `consult-omni-mu4e'."
  (save-mark-and-excursion
    (consult-mu--execute-all-marks))
  (setq consult-mu--override-group nil)
  cand)

(defun consult-omni--mu-callback (cand)
  "Callback function for CAND from `consult-omni-mu4e'."
  (let* ((info (cdr (get-text-property 0 'multi-category cand)))
         (msg (plist-get info :msg))
         (query (plist-get info :query))
         (match-str (car (consult--command-split query))))
    (consult-mu--view msg nil consult-mu-mark-viewed-as-read match-str)
    (consult-mu-overlays-toggle consult-mu-view-buffer-name)))

(cl-defun consult-omni--mu-fetch-results (input &rest args &key callback &allow-other-keys)
  "Fetch results for searching INPUT in “mu4e” with ARGS.

CALLBACK is a function used internally to update the list of candidates in
the minibuffer asynchronously.  It is called with a list of strings, which
are new annotated candidates \(e.g. as they arrive from an asynchronous
process\) to be added to the minibuffer completion cnadidates.  See the
section on REQUEST in documentation for `consult-omni-define-source' as
well as the function
`consult-omni--multi-update-dynamic-candidates' for how CALLBACK is used."
  (save-window-excursion
    (consult-mu--execute-all-marks)
    (pcase-let* ((`(,query . ,opts) (consult-omni--split-command input (seq-difference args (list :callback callback))))
                 (opts (car-safe opts))
                 (count (plist-get opts :count))
                 (count (or (and count (integerp (read count)) (string-to-number count))
                            consult-omni-default-count))
                 (mu-input (format "%s -- --maxnum %s" query count))
                 (messages))
      (consult-mu--update-headers mu-input nil nil :dynamic)
      (with-current-buffer consult-mu-headers-buffer-name
        (goto-char (point-min))
        (setq messages (remove nil
                               (cl-loop until (eobp)
                                        collect (let* ((msg (ignore-errors (mu4e-message-at-point))))
                                                  (consult-omni-mu--format-candidate `(,(buffer-substring (point) (line-end-position)) (:msg ,(ignore-errors (mu4e-message-at-point)) :query ,input)) t))
                                        do (forward-line 1)))))
      (when (and messages callback)
        (funcall callback messages)))))

;; Define the Mu4e source
(consult-omni-define-source "mu4e"
                            :narrow-char ?m
                            :type 'dynamic
                            :require-match nil
                            :category 'consult-mu-messages
                            :face 'consult-omni-engine-title-face
                            :request #'consult-omni--mu-fetch-results
                            :lookup #'consult-mu--lookup
                            :on-preview #'consult-omni--mu-preview
                            :on-return #'consult-omni--mu-return
                            :on-callback #'consult-omni--mu-callback
                            :preview-key consult-omni-preview-key
                            :search-hist 'consult-omni--search-history
                            :select-hist 'consult-omni--email-select-history
                            :enabled (lambda () (and (executable-find "mu")
                                                     (fboundp 'consult-mu)))
                            :group #'consult-omni--group-function
                            :sort t
                            :interactive consult-omni-intereactive-commands-type
                            :annotate nil)

;;; provide `consult-omni-mu4e' module

(provide 'consult-omni-mu4e)

(add-to-list 'consult-omni-sources-modules-to-load 'consult-omni-gh)
;;; consult-omni-mu4e.el ends here
