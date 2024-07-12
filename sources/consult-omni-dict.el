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
(with-temp-buffer (dictionary-mode))

;;; Customization Variables

(defcustom consult-omni-dict-buffer-name " *consult-omni-dict*"
  "Name for consult-omni-dict buffer."
  :type '(choice (:tag "A string for buffer name" string)
                 (:tag "A custom function taking prompt (and other args) as input and returning buffer name string" function)))

(defcustom consult-omni-dict-short-definition-wordcount 800
  "Number of words to use in a short definition"
  :type 'integer)

(defcustom consult-omni-dict-server (or dictionary-server "dict.org")
"This server is contacted for searching the dictionary.

For details see `dictionary-server'."
  :type '(choice (const :tag "Automatic" nil)
                 (const :tag "localhost" "localhost")
                 (const :tag "dict.org" "dict.org")
                 (string :tag "User-defined"))
)

(defcustom consult-omni-dict-number-of-lines nil
"How many lines of definition to show in minibuffer?

Truncate the definition to this many lines in minibuffer."
  :type '(choice (const :tag "(Default) Do not truncate" nil)
                 (const :tag "Just use the first line" 1)
                 (int :tag "Custom Number of Lines")))

(cl-defun consult-omni--dict-format-candidate (&rest args &key source query line face &allow-other-keys)
  "Returns a formatted string for Dictionary candidates

Description of Arguments:

  SOURCE the name to use (e.g. “Dictionary”)
  QUERY  query input from the user
  LINE   a single line of the definition for QUERY.
  FACE   the face to apply to DEFINITION"
  (let* ((frame-width-percent (floor (* (frame-width) 0.1)))
         (match-str (and (stringp query) (consult--split-escaped query)))
         (face (or (consult-omni--get-source-prop source :face) face 'consult-omni-default-face))
         (title-str (and line (stringp line) (propertize line 'face face)))
         (str title-str))
    (if consult-omni-highlight-matches
        (cond
         ((listp match-str)
          (mapcar (lambda (match) (setq str (consult-omni--highlight-match match str t))) match-str))
         ((stringp match-str)
          (setq str (consult-omni--highlight-match match-str str t)))))
    str))

(defun consult-omni--dict-buffer-name (&optional query &rest args)
  "Returns a string for `consult-omni-gptel' buffer name"
  (cond
   ((functionp consult-omni-dict-buffer-name)
    (funcall consult-omni-dict-buffer-name query args))
   ((stringp consult-omni-dict-buffer-name)
    consult-omni-dict-buffer-name)
   (t " *consult-omni-dict*")))

(defun consult-omni--dict-preview (cand)
  "Shows a preview buffer of CAND for `consult-omni-dict'.

The preview buffer is from `consult-omni--dict-definition-preview'."
  (if (listp cand) (setq cand (or (car-safe cand) cand)))
  (let*  ((query (get-text-property 0 :query cand))
          (buffer (get-text-property 0 :buffer cand))
          (pos (get-text-property 0 :pos cand)))
    (when buffer
      (with-current-buffer buffer (when pos (goto-line pos)))
      (funcall (consult--buffer-preview) 'preview
                 buffer))))

(defun consult-omni--dict-return (cand)
"Returns definition string of CAND for `consult-omni-dict'."
(if-let  ((buffer (get-text-property 0 :buffer cand)))
    (with-current-buffer buffer (buffer-string))
cand))

(cl-defun consult-omni--dict-fetch-results (input &rest args &key callback &allow-other-keys)
  "Fetches word definitions for INPUT from `dictionary'."
  (pcase-let* ((`(,query . ,opts) (consult-omni--split-command input (seq-difference args (list :callback callback))))
               (opts (car-safe opts))
               (source "Dictionary")
               (dictionary-server consult-omni-dict-server)
               (buffer (get-buffer-create (consult-omni--dict-buffer-name query args)))
               (def (dictionary-definition query))
               (_ (with-current-buffer buffer
                    (erase-buffer)
                    (insert def)
                    (add-to-history 'dictionary-word-history query)
                    (consult-omni--overlay-match query nil consult-omni-highlight-match-ignore-case)
                    ))
               (frame-width-percent (floor (* (frame-width) 0.1)))
               (answer (if (length> def consult-omni-dict-short-definition-wordcount) (substring def 0 consult-omni-dict-short-definition-wordcount) def))
               (items (split-string answer "\n"))
               (items (if (integerp consult-omni-dict-number-of-lines) (seq-take items consult-omni-dict-number-of-lines) items))
               (idx 0)
               (annotated-results (nreverse (mapcar (lambda (item)
                                            (propertize (consult-omni--dict-format-candidate :source source :query query :line item)
                                                        :source "Dictionary"
                                                        :buffer buffer
                                                        :pos (cl-incf idx)))
                                         items))))
              (when (and annotated-results (functionp callback))
                (funcall callback annotated-results))
              annotated-results))

;; Define the Dictionary Source
(consult-omni-define-source "Dictionary"
                            :narrow-char ?a
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
                            :enabled (lambda () (fboundp 'dictionary-definition))
                            :group #'consult-omni--group-function
                            :sort nil
                            :static 'both
                            :annotate nil)

;;; provide `consult-omni-dict' module

(provide 'consult-omni-dict)

(add-to-list 'consult-omni-sources-modules-to-load 'consult-omni-dict)
;;; consult-omni-dict.el ends here
