;;; consult-omni-mu4e.el --- Consulting Mu4e -*- lexical-binding: t -*-

;; Copyright (C) 2024 Armin Darvish

;; Author: Armin Darvish
;; Maintainer: Armin Darvish
;; Created: 2024
;; Version: 0.1
;; Package-Requires: ((emacs "28.1") (consult "1.4") (consult-mu "1.0") (consult-omni "0.1"))
;; Homepage: https://github.com/armindarvish/consult-omni
;; Keywords: convenience

;;; Commentary:

;;; Code:

(require 'consult-omni)
(require 'consult-mu)

(defun consult-omni-mu--format-candidate (cand highlight)
  "Formats candidates for `consult-omni-mu4e'

Description of Arguments:

  CAND      a candidate from consult-mu
  HIGHLIGHT when non-nil highlights the query term in minibuffer"
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
                       (mapcar (lambda (match) (setq str (consult-mu--highlight-match match str t))) match-str))
                      ((stringp match-str)
                       (setq str (consult-mu--highlight-match match-str str t))))
           str)
(when msg
  (cons str (list :msg msg :query query :type :dynamic)))))

(defun consult-omni--mu-preview (cand)
  "Preview for `consult-omni-mu4e'."
  (when-let* ((info (text-properties-at 0 (cdr (get-text-property 0 'multi-category cand))))
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
  "Return function for `consult-omni-mu4e'."
  (save-mark-and-excursion
    (consult-mu--execute-all-marks))
  (setq consult-mu--override-group nil)
  cand)

(defun consult-omni--mu-callback (cand)
  "Callback function for `consult-omni-mu4e'."
  (let* ((info (text-properties-at 0 (cdr (get-text-property 0 'multi-category cand))))
         (msg (plist-get info :msg))
         (query (plist-get info :query))
         (match-str (car (consult--command-split query))))
    (consult-mu--view msg nil consult-mu-mark-viewed-as-read match-str)
    (consult-mu-overlays-toggle consult-mu-view-buffer-name)))

(cl-defun consult-omni--mu-fetch-results (input &rest args &key callback &allow-other-keys)
  "Makes builder command line args for “mu4e”."
  (save-mark-and-excursion
    (consult-mu--execute-all-marks))
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
                                      collect (let ((msg (ignore-errors (mu4e-message-at-point))))
                                                (consult-omni-mu--format-candidate `(,(buffer-substring (point) (point-at-eol)) (:msg ,(ignore-errors (mu4e-message-at-point)) :query ,input)) t))
                                      do (forward-line 1)))))
    (when (and messages callback)
      (funcall callback messages))))

;; Define the Mu4e Source
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
                                                (mu4e-running-p)
                                                (fboundp 'consult-mu)))
                            :group #'consult-omni--group-function
                            :sort t
                            :interactive consult-omni-intereactive-commands-type
                            :annotate nil)

;;; provide `consult-omni-mu4e' module

(provide 'consult-omni-mu4e)

(add-to-list 'consult-omni-sources-modules-to-load 'consult-omni-gh)
;;; consult-omni-mu4e.el ends here
