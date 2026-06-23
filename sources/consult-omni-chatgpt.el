;;; consult-omni-chatgpt.el --- Consulting ChatGPT -*- lexical-binding: t -*-

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
;; consult-omni-chatgpt provides commands for getting ChatGPT results
;; directly in the minibuffer using consult-omni.

;;; Code:

(require 'consult-omni)

;;; User Options (a.k.a. Custom Variables)

(defcustom consult-omni-openai-api-key nil
  "Key for OpeAI API.

Can be a key string or a function that returns a key string.

Refer to URL `https://openai.com/product' and
URL `https://platform.openai.com/docs/introduction' for details on getting
an API key."
  :group 'consult-omni
  :type '(choice (string :tag "API Key")
                 (function :tag "Custom Function")))

(defvar consult-omni-chatgpt-api-url "https://api.openai.com/v1/chat/completions"
"API URL for OpenAI chatgpt service.")

(defun consult-omni-dynamic--chatgpt-format-candidate (source query title &optional model face)
  "Format candidates of `consult-omni-chatgpt'.

Description of Arguments:

  SOURCE     a string; the source name to use \(e.g. “ChatGPT”\)
  QUERY      a string; query input from the user
  TITLE      a string; the title of the candidate
             \(e.g. response from ChatGPT\)
  MODEL      a string; the OpenAI model used
  FACE       a string; the face to apply to TITLE"
  (let* ((source (if (stringp source) (propertize source 'face 'consult-omni-source-type-face)))
         (title-str (consult-omni--set-string-width title (floor (* (frame-width) 0.4))))
         (title-str (propertize title-str 'face (or face 'consult-omni-ai-title-face)))
         (str (concat title-str "\t"
                      (propertize " " 'display '(space :align-to center))
                      (if model (propertize (format "model: %s" model) 'face 'consult-omni-path-face))
                      (if source (concat "\t" source))))
         (match-str (if (and (stringp query) (not (equal query ".*"))) (consult--split-escaped query) nil)))
    (if consult-omni-highlight-matches-in-minibuffer
        (cond
         ((listp match-str)
          (mapc (lambda (match) (setq str (consult-omni--highlight-match match str t))) match-str))
         ((stringp match-str)
          (setq str (consult-omni--highlight-match match-str str t)))))
    str))

(defun consult-omni--chatgpt-response-preview (response &optional query)
  "Return a buffer with formatted RESPONSE to QUERY from ChatGPT."
  (save-excursion
    (let ((buff (get-buffer-create "*consult-omni-chatgpt-response*")))
      (with-current-buffer buff
        (erase-buffer)
        (if query (insert (format "# User:\n\n %s\n\n" query)))
        (if response (insert (format "# ChatGPT:\n\n %s\n\n" response)))
        (when (featurep 'mardown-mode)
          (require 'markdown-mode nil t)
          (markdown-mode))
        (point-marker)))))

(defun consult-omni--chatgpt-preview (cand)
  "Show a preview buffer with ChatGPT response in CAND."
  (when-let* ((buff (get-buffer "*consult-omni-chatgpt-response*")))
    (kill-buffer buff))
  (if (listp cand) (setq cand (or (car-safe cand) cand)))
  (when-let*  ((query  (get-text-property 0 :query cand))
               (response (or (get-text-property 0 :title cand) cand))
               (marker (consult-omni--chatgpt-response-preview response query)))
    (consult--jump marker)))

(cl-defun consult-omni--chatgpt-fetch-results (input &rest args &key callback &allow-other-keys)
  "Fetch chat response for INPUT from ChatGPT with ARGS.

CALLBACK is a function used internally to update the list of candidates in
the minibuffer asynchronously.  It is called with a list of strings, which
are new annotated candidates \(e.g. as they arrive from an asynchronous
process\) to be added to the minibuffer completion cnadidates.  See the
section on REQUEST in documentation for `consult-omni-define-source' as
well as the function
`consult-omni--multi-update-dynamic-candidates' for how CALLBACK is used."
  (pcase-let* ((`(,query . ,opts) (consult-omni--split-command input (seq-difference args (list :callback callback))))
               (opts (car-safe opts))
               (model (or (plist-get opts :model) "gpt-3.5-turbo"))
               (headers `(("Content-Type" . "application/json")
                          ("Authorization" . ,(concat "Bearer " (consult-omni-expand-variable-function consult-omni-openai-api-key))))))
    (consult-omni--fetch-url consult-omni-chatgpt-api-url
                             consult-omni-http-retrieve-backend
                             :type "POST"
                             :encoding 'utf-8
                             :headers headers
                             :data  (json-encode `((model . ,model)
                                                   (messages . [((role . "user")
                                                                 (content . ,query))])))
                             :parser #'consult-omni--json-parse-buffer
                             :callback
                             (lambda (attrs)
                               (let* ((source "ChatGPT")
                                      (url nil)
                                      (search-url nil)
                                      (choices (car-safe (gethash "choices" attrs)))
                                      (title (and choices (map-nested-elt choices '("message" "content"))))
                                      (model model)
                                      (decorated (consult-omni-dynamic--chatgpt-format-candidate source query title model))
                                      (annotated-results (and decorated
                                                              (propertize decorated
                                                                          :source source
                                                                          :title title
                                                                          :url url
                                                                          :model model
                                                                          :search-url search-url
                                                                          :query query))))
                                 (when (and annotated-results (functionp callback))
                                   (funcall callback (list annotated-results)))
                                 (list annotated-results))))))

;; Define the ChatGPT Source
(consult-omni-define-source "ChatGPT"
                            :narrow-char ?a
                            :type 'dynamic
                            :require-match t
                            :face 'consult-omni-ai-title-face
                            :request #'consult-omni--chatgpt-fetch-results
                            :min-input 5
                            :preview-key consult-omni-preview-key
                            :on-preview #'consult-omni--chatgpt-preview
                            :on-return #'identity
                            :on-callback #'consult-omni--chatgpt-preview
                            :search-hist 'consult-omni--search-history
                            :select-hist 'consult-omni--selection-history
                            :enabled (lambda () (bound-and-true-p consult-omni-openai-api-key))
                            :group #'consult-omni--group-function
                            :sort t
                            :interactive consult-omni-intereactive-commands-type
                            :annotate nil)

;;; provide `consult-omni-chatgpt' module

(provide 'consult-omni-chatgpt)

(add-to-list 'consult-omni-sources-modules-to-load 'consult-omni-chatgpt)
;;; consult-omni-chatgpt.el ends here
