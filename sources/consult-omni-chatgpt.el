;;; consult-omni-chatgpt.el --- Consulting chatGPT -*- lexical-binding: t -*-

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

(defcustom consult-omni-openai-api-key nil
"Key for OpeAI API

See URL `https://openai.com/product' and
URL `https://platform.openai.com/docs/introduction'
for details."
:group 'consult-omni
:type '(choice (const :tag "API Key" string)
               (function :tag "Custom Function")))

(defvar consult-omni-chatgpt-api-url "https://api.openai.com/v1/chat/completions"
"API URL for OpenAI chatgpt service")

(defun consult-omni-dynamic--chatgpt-format-candidate (source query title &optional model face)
  "Formats candidates of `consult-omni-chatgpt'.

Description of Arguments:

  SOURCE     the source name to use (e.g. “chatgPT”)
  QUERY      query input from the user
  URL        a string pointing to url of the candidate
  SEARCH-URL a string pointing to the url for
             the search results of QUERY on the SOURCE website
  TITLE      the title of the candidate (e.g. response from chatgpt)
  MODEL      the OpenAI model used
  FACE       the face to apply to TITLE"
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
          (mapcar (lambda (match) (setq str (consult-omni--highlight-match match str t))) match-str))
         ((stringp match-str)
          (setq str (consult-omni--highlight-match match-str str t)))))
    str))

(defun consult-omni--chatgpt-response-preview (response &optional query)
  "Returns a buffer with formatted RESPONSE from chatGPT"
  (save-excursion
    (let ((buff (get-buffer-create "*consult-omni-chatgpt-response*")))
      (with-current-buffer buff
        (erase-buffer)
        (if query (insert (format "# User:\n\n %s\n\n" query)))
        (if response (insert (format "# chatGPT:\n\n %s\n\n" response)))
        (if (featurep 'mardown-mode)
            (require 'markdown-mode)
          (markdown-mode))
        (point-marker)))))

(defun consult-omni--chatgpt-preview (cand)
  "Shows a preview buffer with chatGPT response from CAND"
  (when-let ((buff (get-buffer "*consult-omni-chatgpt-response*")))
    (kill-buffer buff))
  (if (listp cand) (setq cand (or (car-safe cand) cand)))
  (when-let*  ((query  (get-text-property 0 :query cand))
               (response (or (get-text-property 0 :title cand) cand))
               (marker (consult-omni--chatgpt-response-preview response query)))
    (consult--jump marker)))

(cl-defun consult-omni--chatgpt-fetch-results (input &rest args &key callback &allow-other-keys)
  "Fetches chat response for INPUT from chatGPT."
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
                               (let* ((source "chatGPT")
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

;; Define the chatGPT Source
(consult-omni-define-source "chatGPT"
                            :narrow-char ?a
                            :type 'dynamic
                            :require-match t
                            :face 'consult-omni-ai-title-face
                            :request #'consult-omni--chatgpt-fetch-results
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
