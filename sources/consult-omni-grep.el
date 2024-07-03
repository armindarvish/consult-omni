;;; consult-omni-grep.el --- Consulting Grep Command -*- lexical-binding: t -*-

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

(defun consult-omni--grep-make-builder (make-builder &optional dir)
  "General builder for grep and similar process."
  (pcase-let* ((`(_ ,paths ,dir) (consult--directory-prompt "" dir))
               (paths (if dir
                          (mapcar (lambda (path) (file-truename (concat dir path))) paths)
                        paths)))
    (funcall make-builder paths)))

(defun consult-omni--grep-transform (candidates &optional query)
  "Formats candidates pf `consult-omni-grep'.

Adopted from `consult--grep-format'."
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
            (setq file (or (and (buffer-file-name)
                                (file-relative-name (match-string 1 str) (file-name-directory (buffer-file-name))))
                           (file-relative-name (match-string 1 str))))
            (when (and file (stringp file) (> file-len (* frame-width-percent 2)))
              (setq file (consult-omni--set-string-width file (* frame-width-percent 2) (* frame-width-percent 1))))
            (setq file-len (length file)))
          (let* ((line (propertize (match-string 2 str) 'face 'consult-line-number))
                 (ctx (= (aref str (match-beginning 3)) ?-))
                 (sep (if ctx "-" ":"))
                 (content (substring str (match-end 0)))
                 (line-len (length line)))
            (when (length> content consult-grep-max-columns)
              ;; (setq content (substring content 0 consult-grep-max-columns))
              (setq content  (consult-omni--set-string-width content consult-grep-max-columns)))
            (setq str (concat file sep line sep content))
            ;; Store file name in order to avoid allocations in `consult--prefix-group'
            (add-text-properties 0 file-len `(face consult-file consult--prefix-group ,file) str)
            ;; (put-text-property (1+ file-len) (+ 1 file-len line-len) 'face 'consult-line-number str)
            (when ctx
              (add-face-text-property (+ 2 file-len line-len) (length str) 'consult-grep-context 'append str))
            (push (propertize str :source "grep" :title query) result)))))
    result))

(defun consult-omni--grep-preview (cand)
  "Grep preview function."
  (funcall  (consult--jump-state) 'preview (consult--grep-position (cdr (get-text-property 0 'multi-category cand)))))

(defun consult-omni--grep-callback (cand)
  "Grep callback function."
  (funcall  (consult--jump-state) 'return (consult--grep-position (cdr (get-text-property 0 'multi-category cand)))))

(cl-defun consult-omni--grep-builder (input &rest args &key callback &allow-other-keys)
  "Makes builder command line args for “grep”."
  (pcase-let* ((`(,query . ,opts) (consult-omni--split-command input (seq-difference args (list :callback callback))))
               (opts (car-safe opts))
               (count (plist-get opts :count))
               (dir (plist-get opts :dir))
               (dir (if dir (file-truename (format "%s" dir))))
               (count (or (and count (integerp (read count)) (string-to-number count))
                          consult-omni-default-count))
               (default-directory (or dir default-directory)))
    (funcall (consult-omni--grep-make-builder #'consult--grep-make-builder dir) query)))

;; Define the Grep Source
(consult-omni-define-source "grep"
                            :narrow-char ?r
                            :type 'async
                            :require-match t
                            :face 'consult-omni-engine-title-face
                            :request #'consult-omni--grep-builder
                            :transform #'consult-omni--grep-transform
                            :on-preview #'consult-omni--grep-preview
                            :on-return #'identity
                            :on-callback #'consult-omni--grep-callback
                            :preview-key consult-omni-preview-key
                            :search-hist 'consult-omni--search-history
                            :select-hist 'consult-omni--selection-history
                            :group #'consult-omni--group-function
                            :sort t
                            :static 'both
                            :transform #'consult-omni--ripgrep-transform
                            :enabled (lambda () (if (and (executable-find "grep")
                                                         (fboundp 'consult-grep))
                                                    t nil))
                            :annotate nil)

;;; provide `consult-omni-grep' module

(provide 'consult-omni-grep)

(add-to-list 'consult-omni-sources-modules-to-load 'consult-omni-grep)
;;; consult-omni-grep.el ends here
