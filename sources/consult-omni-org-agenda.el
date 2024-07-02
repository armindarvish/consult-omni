;;; consult-omni-org-agenda.el --- Consulting Org Agenda -*- lexical-binding: t -*-

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

(defun consult-omni--org-agenda-next-day (date)
  (if (stringp date) (setq date  (date-to-time date)))
  (format-time-string "%Y-%m-%d" (encode-time (decoded-time-add (decode-time date) (make-decoded-time :day 1)))))

(defun consult-omni--org-agenda-week-of (date)
  (if (stringp date) (setq date  (date-to-time date)))
(cl-loop for d from 0 to 7
         collect (format-time-string "%Y-%m-%d" (encode-time (decoded-time-add (decode-time date) (make-decoded-time :day d))))
))

(cl-defun consult-omni--org-agenda-format-candidate (&rest args &key source query title buffer todo prio tags filepath snippet sched dead face &allow-other-keys)
  "Formats a candidate for `consult-omni-youtube' commands.

Description of Arguments:

  SOURCE       the name to use (e.g. “Org Agenda”)
  QUERY        the query input from the user
  TITLE        the title of the agenda item
  SNIPPET      string containing a snippet/description of the agenda item
  DATE         the due date of the agenda item
  FACE         the face to apply to TITLE"
  (let* ((frame-width-percent (floor (* (frame-width) 0.1)))
         (source (propertize source 'face 'consult-omni-source-type-face))
         (match-str (if (stringp query) (consult--split-escaped query) nil))
         (buffer (and buffer (propertize (format "%s" buffer) 'face 'consult-omni-domain-face)))
         (prio (and (stringp prio) (propertize (format "[#%s]" prio) 'face 'consult-omni-prompt-face)))
         (todo (and (stringp todo) (propertize todo 'face (or (and org-todo-keyword-faces (cdr (assoc todo org-todo-keyword-faces)))
                                                              (and (member todo org-done-keywords) 'org-done)
 'org-todo))))

         (tags (and tags (stringp tags) (propertize tags 'face 'consult-omni-keyword-face)))
         (snippet (and snippet (stringp snippet) (propertize snippet 'face 'consult-omni-snippet-face)))
         (snippet (if (stringp snippet) (consult-omni--set-string-width (replace-regexp-in-string "\n" "  " snippet) (* 2 frame-width-percent))))
         (sched (or (and sched (stringp sched) (propertize sched 'face (or 'org-agenda-date 'consult-omni-date-face))) (make-string 15 ?\s)))
         (fraction (and dead (- 1 (min (/ (float (- (org-agenda--timestamp-to-absolute dead) (org-today))) (max (org-get-wdays dead) 1)) 1.0))))
         (dead-face (and dead
                         (org-agenda-deadline-face
			     fraction)))
         (dead (or (and dead (stringp dead) (propertize dead 'face (or dead-face 'consult-omni-warning-face))) (make-string 15 ?\s)))
         (date (concat (and (stringp sched) sched) (and (stringp sched) " ") (and (stringp dead) dead)))
         (face (or (consult-omni--get-source-prop source :face) face))
         (todo-str (concat (or prio "    ") " " todo))
         (title (if (and face (stringp title)) (propertize title 'face face) title))
         (title-str (if (and (stringp tags) (stringp title)) (concat title " " tags) title))
         (title-str (and (stringp title-str)
 (consult-omni--set-string-width title-str (* 5 frame-width-percent))))
         (str (concat title-str
                      (and todo-str "\t") todo-str
                      (and buffer "\s") buffer
                      (and date "\s\s") date
                      (and snippet "\s\s") snippet
                      (and source "\t") source)))
    (if consult-omni-highlight-matches
        (cond
         ((listp match-str)
          (mapcar (lambda (match) (setq str (consult-omni--highlight-match match str t))) match-str))
         ((stringp match-str)
          (setq str (consult-omni--highlight-match match-str str t)))))
    str))

(defun consult-omni--org-agenda-items (query &optional match &rest skip)
  "Return a list of Org heading candidates.

If PREFIX is non-nil, prefix the candidates with the buffer name.
MATCH, SCOPE and SKIP are as in `org-map-entries'."
  (let (buffer
        (source "Org Agenda"))
    (apply
     #'org-map-entries
     (lambda ()
       ;; Reset the cache when the buffer changes, since `org-get-outline-path' uses the cache
       (unless (eq buffer (buffer-name))
         (setq buffer (buffer-name)
               org-outline-path-cache nil))
       (pcase-let* ((`(_ ,level ,todo ,prio ,_hl ,tags) (org-heading-components))
                    (filename (buffer-file-name))
                    (filepath (file-truename filename))
                    (tags (if org-use-tag-inheritance
                              (when-let ((tags (org-get-tags)))
                                (concat ":" (string-join tags ":") ":"))
                            tags))
                    (title (org-format-outline-path
                           (org-get-outline-path 'with-self 'use-cache)
                           most-positive-fixnum))
                    (prio (and (characterp prio) (char-to-string prio)))
                    (marker (point-marker))
                    (props (org-entry-properties))
                    (sched (cdr (assoc "TIMESTAMP" props)))
                    (dead (cdr (assoc "DEADLINE" props)))
                    (snippet nil)
                    (query (cond
                            ((equal query "today") (format-time-string "%Y-%m-%d" (current-time)))
                            ((equal query "tomorrow") (consult-omni--org-agenda-next-day (current-time)))
                            ((equal query "this week") (mapconcat #'identity (consult-omni--org-agenda-week-of (current-time)) "\\|"))
                            ((equal query "this month") (format-time-string "%Y-%m" (current-time)))
                            ((equal query "this year") (format-time-string "%Y" (current-time)))
                            (t query))))
 (org-format-timestamp (org-timestamp-from-time (org-today)) "%Y")
         (if (string-match-p (concat ".*" query ".*") (concat todo " " prio " " _hl " " sched " " dead " " tags))
           (propertize (consult-omni--org-agenda-format-candidate :source source :query query :title title :buffer buffer :todo todo :prio prio :tags tags :filepath filepath :snippet snippet :sched sched :dead dead) :source source :title title :query query :url nil :search-url nil :tags tags :filepath filepath :marker marker))))
     match 'agenda skip)))

(defun consult-omni--org-agenda-preview (cand)
  "Preview function for `consult-omni-org-agenda'."
 (if-let ((marker (get-text-property 0 :marker cand)))
            (consult--jump marker)
          )
)

(defun consult-omni--org-agenda-callback (cand)
  "Callback function for `consult-omni-org-agenda'."
  (if-let ((marker (get-text-property 0 :marker cand)))
            (consult--jump marker)
          ))

(defun consult-omni--org-agenda-new (cand)
  "New function for `consult-omni-org-agenda'."
  (let ((title (substring-no-properties cand))
        (old-marker org-capture-last-stored-marker))
  (org-capture-string title)
  (consult-omni-propertize-by-plist title `(:title ,title :source "Org Agenda" :url nil :search-url nil :query ,title :sched nil :dead nil :tags nil :filepath ,(cadr (org-capture-get :target)) :marker ,(unless (equal ,old-marker ,org-capture-last-stored-marker) org-capture-last-stored-marker)) 0 1))
  )

(cl-defun consult-omni--org-agenda-fetch-results (input &rest args &key callback &allow-other-keys)
  "Fetches chat response for INPUT from gptel."
  (pcase-let* ((`(,query . ,opts) (consult-omni--split-command input (seq-difference args (list :callback callback))))
               (opts (car-safe opts))
               (match (or (and (plist-member opts :match) (plist-get opts :match))
                          (and (plist-member opts :filter) (plist-get opts :filter))))
               (source "Org Headings")
               (annotated-results (delq nil (consult-omni--org-agenda-items query match))))
    (when annotated-results
      (when (functionp callback)
        (funcall callback annotated-results))
      annotated-results
      )
    ))

(consult-omni-define-source "Org Agenda"
                            :narrow-char ?o
                            :category 'org-heading
                            :type 'dynamic
                            :require-match nil
                            :request #'consult-omni--org-agenda-fetch-results
                            :on-preview #'consult-omni--org-agenda-preview
                            :on-return #'identity
                            :on-callback #'consult-omni--org-agenda-callback
                            :on-new #'consult-omni--org-agenda-new
                            :preview-key consult-omni-preview-key
                            :search-hist 'consult-omni--search-history
                            :select-hist 'consult-omni--selection-history
                            :enabled (lambda () (bound-and-true-p org-agenda-files))
                            :group #'consult-omni--group-function
                            :sort t
                            :static 'both
                            )

;;; provide `consult-omni-org-agenda' module

(provide 'consult-omni-org-agenda)

(add-to-list 'consult-omni-sources-modules-to-load 'consult-omni-org-agenda)
;;; consult-omni-org-agenda.el ends here
