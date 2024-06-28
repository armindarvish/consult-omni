;;; consult-omni-ripgrep.el --- Consulting Ripgrep Command -*- lexical-binding: t -*-

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

(defun consult-omni--ripgrep-transform (candidates &optional query)
  "Formats candidates of `consult-omni-ripgrep'.
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
                  (setq file (or (and (buffer-file-name)
                           (file-relative-name (match-string 1 str) (file-name-directory (buffer-file-name))))
                          (file-relative-name (match-string 1 str))))

                  (when (and file (stringp file) (> file-len (* frame-width-percent 2)))
                    (setq file (consult-omni--set-string-width file (* frame-width-percent 2) (* frame-width-percent 1))))
                  (setq file-len (length file))

                  )
                (let* ((line (propertize (match-string 2 str) 'face 'consult-line-number))
                       (ctx (= (aref str (match-beginning 3)) ?-))
                       (sep (if ctx "-" ":"))
                       (content (substring str (match-end 0)))
                       (line-len (length line)))
                  (when (length> content consult-grep-max-columns)
                    ;; (setq content (substring content 0 consult-grep-max-columns))
                    (setq content  (consult-omni--set-string-width content consult-grep-max-columns))
                    )
                  (setq str (concat file sep line sep content))
                  ;; Store file name in order to avoid allocations in `consult--prefix-group'
                  (add-text-properties 0 file-len `(face consult-file consult--prefix-group ,file) str)
                  ;; (put-text-property (1+ file-len) (+ 1 file-len line-len) 'face 'consult-line-number str)
                  (when ctx
                    (add-face-text-property (+ 2 file-len line-len) (length str) 'consult-grep-context 'append str))
                  (push (propertize str :source "ripgrep" :title query) result)))))
          result))

(file-relative-name "/Users/armin/projects/consult-omni/README.org" (buffer-file-name))

(cl-defun consult-omni--ripgrep-builder (input &rest args &key callback &allow-other-keys)
  "makes builder command line args for “ripgrep”.
"
  (pcase-let* ((`(,query . ,opts) (consult-omni--split-command input (seq-difference args (list :callback callback))))
               (opts (car-safe opts))
               (count (plist-get opts :count))
               (dir (plist-get opts :dir))
               (dir (if dir (file-truename (format "%s" dir))))
               (count (or (and count (integerp (read count)) (string-to-number count))
                          consult-omni-default-count))
               (default-directory (or dir default-directory))
               )
   (funcall (consult-omni--grep-make-builder #'consult--ripgrep-make-builder dir) query)
            ))

(consult-omni-define-source "ripgrep"
                           :narrow-char ?r
                           :type 'async
                           :require-match t
                           :face 'consult-omni-engine-title-face
                           :request #'consult-omni--ripgrep-builder
                           :transform #'consult-omni--ripgrep-transform
                           :on-preview #'consult-omni--grep-preview
                           :on-return #'identity
                           :on-callback #'consult-omni--grep-callback
                           :preview-key consult-omni-preview-key
                           :search-hist 'consult-omni--search-history
                           :select-hist 'consult-omni--selection-history
                           :group #'consult-omni--group-function
                           :enabled (lambda () (if (and (executable-find "rg")
                                                   (fboundp 'consult-ripgrep))
                                                   t nil))
                           :sort t
                           :static 'both
                           :transform #'consult-omni--ripgrep-transform
                           :annotate nil
                           )

;;; provide `consult-omni-ripgrep' module

(provide 'consult-omni-ripgrep)

(add-to-list 'consult-omni-sources-modules-to-load 'consult-omni-ripgrep)
;;; consult-omni-ripgrep.el ends here
