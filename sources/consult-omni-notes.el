;;; consult-omni-notes.el --- Consulting Note Files -*- lexical-binding: t -*-

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

(require 'consult-omni)
(require 'consult-omni-grep)

(defcustom consult-omni-notes-files (apply #'append
                                           (when (bound-and-true-p consult-notes-file-dir-sources)
                                             ;; dir sources
                                             (apply #'append (mapcar #'cddr consult-notes-file-dir-sources)))
                                           ;; org roam
                                           (when (bound-and-true-p org-roam-directory)
                                             (list (expand-file-name org-roam-directory)))
                                           ;; denote
                                           (when (bound-and-true-p denote-directory)
                                             (list (expand-file-name denote-directory)))
                                           ;; org agenda files
                                           (when (bound-and-true-p consult-notes-org-headings-mode)
                                             (list (mapcar #'expand-file-name consult-notes-org-headings-files))))
  "List of all note files for consult-omni-notes."
  :type '(repeat :tag "list of files" string))

(defcustom consult-omni-notes-use-rg t
  "Whether to use ripgrep when searching ntoes?"
  :type 'boolean)

(defun consult-omni--notes-transform (candidates &optional query)
  "Formats `consult-omni-notes' candidates.

Adopted from `consult--grep-format'.
"
  (let* ((frame-width-percent (floor (* (frame-width) 0.1)))
         (file "")
         (file-len 0)
         (file-str "")
         result)
    (save-match-data
      (dolist (str candidates)
        (when (and (string-match consult--grep-match-regexp str)
                   ;; Filter out empty context lines
                   (or (/= (aref str (match-beginning 3)) ?-)
                       (/= (match-end 0) (length str))))
          ;; We share the file name across candidates to reduce
          ;; the amount of allocated memory.
          (unless (and (= file-len (- (match-end 1) (match-beginning 1)))
                       (eq t (compare-strings
                              file 0 file-len
                              str (match-beginning 1) (match-end 1) nil)))
            (setq file (match-string 1 str))
            (setq file-len (length file))
            )
          (let* ((line (propertize (match-string 2 str) 'face 'consult-line-number))
                 (ctx (= (aref str (match-beginning 3)) ?-))
                 (sep (if ctx "-" ":"))
                 (content (substring str (match-end 0)))
                 (line-len (length line)))
            (when (length> content consult-grep-max-columns)
              (setq content  (consult-omni--set-string-width content consult-grep-max-columns))
              )
            (setq str (concat file sep line sep content))

            ;; Store file name in order to avoid allocations in `consult--prefix-group'
            (add-text-properties 0 file-len `(face consult-file consult--prefix-group ,file) str)
            (put-text-property (1+ file-len) (+ 1 file-len line-len) 'face 'consult-line-number str)

            (push (propertize str :source "Notes Search" :title query :file file) result)))))
    result))

(cl-defun consult-omni--notes-builder (input &rest args &key callback &allow-other-keys)
  "Makes builder command line args for `consult-omni-notes'.
"
  (pcase-let* ((`(,query . ,opts) (consult-omni--split-command input args))
               (opts (car-safe opts))
               (count (plist-get opts :count))
               (dir (plist-get opts :dir))
               (dir (if dir (file-truename (format "%s" dir))))
               (dir (or dir consult-omni-notes-files))
               (count (or (and count (integerp (read count)) (string-to-number count))
                          consult-omni-default-count))
               )
    (funcall (consult-omni--grep-make-builder (if (and consult-omni-notes-use-rg (executable-find "rg")) #'consult--ripgrep-make-builder #'consult--grep-make-builder) dir) query)
    ))

;; Define the Notes Search Source
(consult-omni-define-source "Notes Search"
                            :narrow-char ?n
                            :type 'async
                            :require-match nil
                            :face 'consult-omni-engine-title-face
                            :request #'consult-omni--notes-builder
                            :transform #'consult-omni--notes-transform
                            :on-preview #'consult-omni--grep-preview
                            :on-return #'identity
                            :on-callback #'consult-omni--grep-callback
                            :preview-key 'any
                            :search-hist 'consult-omni--search-history
                            :select-hist 'consult-omni--selection-history
                            :group #'consult-omni--group-function
                            ;;:group #'consult--prefix-group
                            :sort t
                            :static 'both
                            :annotate nil
                            )

;;; provide `consult-omni-notes' module

(provide 'consult-omni-notes)

(add-to-list 'consult-omni-sources-modules-to-load 'consult-omni-notes)
;;; consult-omni-notes.el ends here
