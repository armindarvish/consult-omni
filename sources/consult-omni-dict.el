;;; consult-omni-dict.el --- Consulting Dictionary -*- lexical-binding: t -*-

;; Copyright (C) 2024 Armin Darvish

;; Author: Armin Darvish
;; Maintainer: Armin Darvish
;; Created: 2024
;; Version: 0.1
;; Package-Requires: ((emacs "28.1") (consult "1.4") (consult-omni "0.1") (gptel "0.7.0"))
;; Homepage: https://github.com/armindarvish/consult-omni
;; Keywords: convenience

;;; Commentary:

;;; Code:

(require 'dictionary)
(require 'consult-omni)
(ignore-errors (with-temp-buffer (dictionary-mode)))

;;; Customization Variables

(defcustom consult-omni-dict-short-definition-wordcount 1000
  "Number of words to use in a short definition"
  :type 'integer)

(defcustom consult-omni-dict-server (or (and (bound-and-true-p dictionary-server) dictionary-server) "dict.org")
"This server is contacted for searching the dictionary.

For details see `dictionary-server'."
  :type '(choice (const :tag "Automatic" nil)
                 (const :tag "localhost" "localhost")
                 (const :tag "dict.org" "dict.org")
                 (string :tag "User-defined"))
)


(defcustom consult-omni-dict-search-interface (or (and (bound-and-true-p dictionary-search-interface) dictionary-search-interface) 'help)
  "Controls how `dictionary-search' prompts for words and displays definitions.

See `dictionary-search-interface' for details."
  :type '(choice (const :tag "Dictionary buffer" nil)
                 (const :tag "Help buffer" help)))

(defcustom consult-omni-dict-number-of-lines nil
"How many lines of definition to show in minibuffer?

Truncate the definition to this many lines in minibuffer."
  :type '(choice (const :tag "(Default) Do not truncate" nil)
                 (const :tag "Just use the first line" 1)
                 (int :tag "Custom Number of Lines")))


(defcustom consult-omni-dict-use-single-buffer (and (bound-and-true-p dictionary-use-single-buffer) dictionary-use-single-buffer)
  "Should the dictionary command reuse previous dictionary buffers?

See `dictionary-use-single-buffer' for referenc
  "Should the dictionary command reuse previous dictionary buffers?

See `dictionary-use-single-buffer' for reference"
  :type 'boolean)

(cl-defun consult-omni--dict-format-candidates (&rest args &key source query dict def buffer pos  idx face &allow-other-keys)
  "Returns a formatted string for Dictionary candidates

Description of Arguments:

  SOURCE the name to use (e.g. “Dictionary”)
  QUERY  query input from the user
  DICT   name of dictionary for current item
  DEF    definition of current item
  BUFFER the current buffer for dictionary
  POS    position of definition in BUFFER
  IDX    index of definition in current definitions
  FACE   the face to apply to DEFINITION"
  (let* ((frame-width-percent (floor (* (frame-width) 0.1)))
         (source (if (stringp source) (propertize source 'face 'consult-omni-source-type-face)))
         (match-str (and (stringp query) (consult--split-escaped query)))
         (dict (and (stringp dict) (propertize dict 'face 'consult-omni-date-face)))
         (face (or (consult-omni--get-source-prop source :face) face 'consult-omni-default-face))
         (answer (and (stringp def) (if (length> def consult-omni-dict-short-definition-wordcount) (substring def 0 consult-omni-dict-short-definition-wordcount) def)))
         (items (and (stringp answer) (split-string answer "\n")))
         (items (and items (if (integerp consult-omni-dict-number-of-lines) (seq-take items consult-omni-dict-number-of-lines) items)))
         (first-item t))
    (mapcar (lambda (item)
              (if-let ((str (propertize item 'face face)))
                  (progn
                    (if consult-omni-highlight-matches
                        (cond
                         ((listp match-str)
                          (mapcar (lambda (match)
                                    (setq str (consult-omni--highlight-match match str t)))
                                  match-str))
                         ((stringp match-str)
                          (setq str (consult-omni--highlight-match match-str str t)))))
                    (setq str (if first-item
                                  (concat dict "\t" str)
                                (concat (make-string (length dict) ?\s) "\t" str)))
                    (setq first-item nil)
                    (propertize str
                                :source source
                                :title def
                                :query query
                                :pos pos
                                :dict dict
                                :buffer buffer))))
            items)))

(defun consult-omni--dict-preview (cand)
  "Shows a preview buffer of CAND for `consult-omni-dict'."
  (if (listp cand) (setq cand (or (car-safe cand) cand)))
  (let*  ((query (get-text-property 0 :query cand))
          (buffer (get-text-property 0 :buffer cand))
          (pos (get-text-property 0 :pos cand)))
      (when buffer
        (with-current-buffer buffer (when pos (goto-char pos))))
      (funcall (consult--buffer-preview) 'preview
                 buffer)
      (save-excursion
        (with-selected-window (get-buffer-window buffer)
          (recenter 1 t)))
      (consult-omni--pulse-line 0.15)))

(defun consult-omni--dict-return (cand)
"Returns definition string of CAND for `consult-omni-dict'."
(if-let  ((def (get-text-property 0 :def cand)))
    def
cand))

(defun consult-omni--dict-search-query (query &optional maxcount)
"Finds definitions for QUERY from `dictionary'.

if MAXCOUNT is non-nil, only find top MAXCOUNT definitions."
  (let* ((dictionary-server consult-omni-dict-server)
         (dictionary-search-interface consult-omni-dict-search-interface)
         (dictionary-default-strategy consult-omni-dict-default-strategy)
         (dictionary-use-single-buffer consult-omni-dict-use-single-buffer)
         (dictionary-server consult-omni-dict-server)
         (buffer (save-mark-and-excursion (dictionary) (current-buffer)))
         (annotated-results))
    (when (and buffer (buffer-live-p buffer))
      (with-current-buffer buffer
        (dictionary-new-search-internal query "*"
                                        (lambda (result)
                                          (let ((inhibit-read-only t)
                                                (source "Dictionary")
                                                (idx 0)
                                                (reply (dictionary-read-reply-and-split)))
                                            (while (dictionary-check-reply reply 151)
                                              (let* ((reply-list (dictionary-reply-list reply))
                                                     (dictionary (nth 2 reply-list))
	                                             (description (nth 3 reply-list))
	                                             (word (nth 1 reply-list))
                                                     (def)
                                                     (dict)
                                                     (line))
                                                (dictionary-display-word-entry dictionary description)
	                                        (setq reply (dictionary-read-answer))
	                                        (setq def (dictionary-decode-charset reply dictionary))
                                                (setq line (point-max))
                                                (dictionary-display-word-definition reply word dictionary)
                                                (setq reply (dictionary-read-reply-and-split))
                                                (when (or (not maxcount) (and maxcount (< idx maxcount))) (setq annotated-results (append annotated-results (consult-omni--dict-format-candidates :source source :query query :dict dictionary :def def :pos line :buffer buffer :idx idx) )))
                                                (cl-incf idx)))
                                            (dictionary-post-buffer))))
        (consult-omni--overlay-match query nil consult-omni-highlight-match-ignore-case)
        (quit-window))
        annotated-results)))

(cl-defun consult-omni--dict-fetch-results (input &rest args &key callback &allow-other-keys)
  "Fetches word definitions for INPUT from `dictionary'."
  (pcase-let* ((`(,query . ,opts) (consult-omni--split-command input (seq-difference args (list :callback callback))))
               (opts (car-safe opts))
               (count (plist-get opts :count))
               (count (or (and count (integerp (read count)) (string-to-number count))
                          consult-omni-default-count))
               (annotated-results (consult-omni--dict-search-query query (if count count))))
    (when (and annotated-results (functionp callback))
      (funcall callback (nreverse annotated-results))
    annotated-results)))

;; Define the Dictionary Source
(consult-omni-define-source "Dictionary"
                            :narrow-char ?D
                            :category 'consult-omni-dictionary
                            :type 'dynamic
                            :require-match t
                            :face 'consult-omni-snippet-face
                            :request #'consult-omni--dict-fetch-results
                            :on-preview #'consult-omni--dict-preview
                            :on-return #'consult-omni--dict-return
                            :on-callback #'consult-omni--dict-preview
                            :preview-key consult-omni-preview-key
                            :search-hist 'consult-omni--search-history
                            :select-hist 'consult-omni--selection-history
                            :enabled (lambda () (fboundp 'dictionary))
                            :group #'consult-omni--group-function
                            :sort nil
                            :static 'both
                            :annotate nil)

;;; provide `consult-omni-dict' module

(provide 'consult-omni-dict)

(add-to-list 'consult-omni-sources-modules-to-load 'consult-omni-dict)
;;; consult-omni-dict.el ends here
