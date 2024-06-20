;;; consult-omni-gptel.el --- Consulting gptel -*- lexical-binding: t -*-

;; Copyright (C) 2024 Armin Darvish

;; Author: Armin Darvish
;; Maintainer: Armin Darvish
;; Created: 2024
;; Version: 0.1
;; Package-Requires: ((emacs "28.1") (consult "1.1") (consult-omni "0.2") (gptel "0.7.0"))
;; Homepage: https://github.com/armindarvish/consult-omni
;; Keywords: convenience

;;; Commentary:

;;; Code:

(require 'gptel)
(require 'consult-omni)

;;; Customization Variables
(defcustom consult-omni-gptel-buffer-name  "*consult-omni-gptel*"
  "Name for consult-omni-gptel buffer."
  :type '(choice (:tag "A string for buffer name" string)
                 (:tag "A custom function taking prompt (and other args) as input and returning buffer name string" function)))

(cl-defun consult-omni--gptel-format-candidate (&rest args &key source query model backend stream face &allow-other-keys)
  "Returns a formatted string for gptel's candidates

SOURCE is the name string of the source for candidate

QUERY is the query string used for searching

URL is a string pointing to url of the candidate

SEARCH-URL is a string pointing to the url for
the search results of QUERY on the SOURCE website

TITLE is the title of the candidate

SNIPPET is a string containing a snippet/description of candidate
"
  (let* ((frame-width-percent (floor (* (frame-width) 0.1)))
         (source (and (stringp source) (propertize source 'face 'consult-omni-source-type-face)))
         (title (format "ask gptel: %s" (if query (string-trim-right query) "")))
         (match-str (and (stringp query) (consult--split-escaped query) nil))
         (backend (and (stringp backend) (propertize backend 'face 'consult-omni-domain-face)))
         (model (and (stringp model) (propertize model 'face 'consult-omni-path-face)))
         (stream (and stream (propertize "~stream~" 'face 'consult-omni-snippet-face)))
         (face (or (consult-omni--get-source-prop source :face) face 'consult-omni-default-face))
         (title-str (propertize title 'face face))
         (title-str (consult-omni--set-string-width title-str (* 5 frame-width-percent)))
         (str (concat title-str
                      (when backend (concat "\t" backend))
                      (when model (concat ":" model))
                      (when stream (concat "\s" stream "\s"))
                      )))
    (if consult-omni-highlight-matches
        (cond
         ((listp match-str)
          (mapcar (lambda (match) (setq str (consult-omni--highlight-match match str t))) match-str))
         ((stringp match-str)
          (setq str (consult-omni--highlight-match match-str str t)))))
    str))

(defun consult-omni--gptel-buffer-name (&optional query &rest args)
  "Returns a string for `consult-omni-gptel' buffer name"
  (cond
   ((functionp consult-omni-gptel-buffer-name)
    (funcall consult-omni-gptel-buffer-name query args))
   ((stringp consult-omni-gptel-buffer-name)
    consult-omni-gptel-buffer-name)
   (t
    "*consult-omni-gptel*")))

(cl-defun consult-omni--gptel-response-preview (query &rest args &key backend model stream &allow-other-keys)
  "Returns a `gptel' buffer.

QUERY is sent to BACKEND using MODEL.
If STREAM is non-nil, the response is streamed."
  (save-excursion
    (with-current-buffer (gptel (consult-omni--gptel-buffer-name query args) nil nil nil)
      (let* ((query-sent)
             (backend (and backend (car (seq-filter (lambda (item) (when (string-match (format "%s" backend) item) item)) (mapcar #'car gptel--known-backends)))))
             (backend (or backend (gptel-backend-name gptel-backend)))
             (backend (cdr (assoc (format "%s" backend) gptel--known-backends)))
             (model (or (and model (format "%s" model))
                        (and backend (car (cl-struct-slot-value (type-of backend) 'models backend)))
                        gptel-model))
             (stream (if stream t nil))
             )
        (setq-local gptel-backend backend)
        (setq-local gptel-model model)
        (setq-local gptel-stream stream)
        (erase-buffer)
        (insert (gptel-prompt-prefix-string))
        (insert (format "%s" query))
        (unless query-sent
          (erase-buffer)
          (insert (gptel-prompt-prefix-string) query)
          (setq query-sent t)
          (gptel-send)))
      (current-buffer))))



(defun consult-omni--gptelbuffer-preview (cand)
  "Shows a preview buffer of CAND for `consult-omni-gptel'.

The preview buffer is from `consult-omni--gptel-response-preview'."
  (if (listp cand) (setq cand (or (car-safe cand) cand)))
  (let*  ((query (get-text-property 0 :query cand))
          (backend (get-text-property 0 :backend cand))
          (model (get-text-property 0 :model cand))
          (stream (get-text-property 0 :stream cand))
          (buff (consult-omni--gptel-response-preview query :model model :backend backend :stream stream)))
    (if buff
        (funcall (consult--buffer-preview) 'preview
                 buff
                 ))))

(cl-defun consult-omni--gptel-fetch-results (input &rest args &key callback &allow-other-keys)
  "Makes cnaidate with INPUT as placeholder for `consult-omni-gptel'.

This makes a placeholder string “ask gptel: %s” %s=INPUT with
metadata so it can be send to `gptel'."
  (unless (featurep 'gptel)
    (error "consult-omni: gptel is not available. Make sure to install and load `gptel'."))
  (pcase-let* ((`(,query . ,opts) (consult-omni--split-command input (seq-difference args (list :callback callback))))
               (opts (car-safe opts))
               (source "gptel")
               (backend (and (plist-member opts :backend) (format "%s" (plist-get opts :backend))))
               (backend (and backend (car (seq-filter (lambda (item) (when (string-match (format "%s" backend) item) item)) (mapcar #'car gptel--known-backends)))))
               (backend (or backend (gptel-backend-name gptel-backend)))
               (backend-struct  (cdr (assoc (format "%s" backend) gptel--known-backends)))
               (model (and (plist-member opts :model) (format "%s" (plist-get opts :model))))
               (model (or (and model backend-struct (member model (cl-struct-slot-value (type-of backend-struct) 'models backend-struct)) model)
                          (and backend-struct (car (cl-struct-slot-value (type-of backend-struct) 'models backend-struct)))))
               (stream (or (and (plist-member opts :stream) (plist-get opts :stream)) gptel-stream))
               (placeholder (consult-omni--gptel-format-candidate :source source :query query :model model :backend backend :stream stream))
               (annotated-results (propertize placeholder
                                              :source source
                                              :title query
                                              :url nil
                                              :query query
                                              :model model
                                              :stream stream
                                              :backend backend)))
    (list annotated-results)
    ))

(consult-omni-define-source "gptel"
                           :narrow-char ?G
                           :type 'sync
                           :face 'consult-omni-ai-source-face
                           :request #'consult-omni--gptel-fetch-results
                           :on-preview #'consult-omni--gptelbuffer-preview
                           :on-return #'identity
                           :on-callback #'consult-omni--gptelbuffer-preview
                           :preview-key consult-omni-preview-key
                           :search-history 'consult-omni--search-history
                           :selection-history 'consult-omni--selection-history
                           :enabled (lambda () (fboundp 'gptel))
                           :group #'consult-omni--group-function
                           :sort t
                           :static 'both
                           :annotate nil
                           )

;;; provide `consult-omni-gptel' module

(provide 'consult-omni-gptel)

(add-to-list 'consult-omni-sources-modules-to-load 'consult-omni-gptel)
;;; consult-omni-gptel.el ends here
