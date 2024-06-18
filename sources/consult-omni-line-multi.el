;;; consult-omni-line-multi.el --- Search Lines in All Buffers  -*- lexical-binding: t -*-

;; Copyright (C) 2024 Armin Darvish

;; Author: Armin Darvish
;; Maintainer: Armin Darvish
;; Created: 2024
;; Version: 0.1
;; Package-Requires: ((emacs "28.1") (consult "1.1") (consult-omni "0.2"))
;; Homepage: https://github.com/armindarvish/consult-omni
;; Keywords: convenience

;;; Commentary:

;;; Code:

(require 'consult)
(require 'consult-omni)

(defun consult-omni--line-multi-candidates (input &optional buffers)
  "Wrapper around consult--line-multi-candidates for consult-omni."
  (let  ((buffers (or buffers (consult--buffer-query :directory (consult--normalize-directory default-directory) :sort 'alpha-current))))
    (consult--line-multi-candidates buffers input)))

(cl-defun consult-omni--line-multi-fetch-results (input &rest args &key callback &allow-other-keys)
  "Fetches search results for INPUT from `consult-line-multi'."
(unless (functionp 'consult-omni--line-multi-candidates)
  (error "consult-omni: consult-omni-line-multi not available. Make sure `consult' is loaded properly"))
(pcase-let* ((`(,query . ,opts) (consult-omni--split-command input args))
               (opts (car-safe opts))
               (items (consult-omni--line-multi-candidates query))
               (annotated-results (mapcar (lambda (item)
                                            (let* ((source "buffers text search")
                                                   (marker  (consult--get-location item))
                                                   (title (substring-no-properties item 0 -1))
                                                   (decorated (consult-omni--line-multi-format-candidate :source source :query query :marker marker :title title)))
                                           (propertize decorated
                                                       :source source
                                                       :title title
                                                       :url nil
                                                       :marker marker
                                                       :query query
                                                       ))) items)))
    annotated-results))

(cl-defun consult-omni--line-multi-format-candidate (&rest args &key source query marker title face &allow-other-keys)
  "Formats the cnaiddates of `consult-omni-line-multi'.

SOURCE is the name to use (e.g. “Line MUlti”)

QUERY is the query input from the user

MARKER is the marker pointing to results of line multi search

TITLE is the title of the candidate (e.g. line text)

FACE is the face to apply to TITLE"
  (let* ((frame-width-percent (floor (* (frame-width) 0.1)))
         (source (if (stringp source) (propertize source 'face 'consult-omni-source-type-face)))
         (marker (car marker))
         (buff (marker-buffer marker))
         (pos (marker-position marker))
         (buff (and buff (propertize (format "%s" buff) 'face 'consult-omni-domain-face)))
         (pos (and pos (propertize (format "%s" pos) 'face 'consult-omni-path-face)))
         (match-str (if (stringp query) (consult--split-escaped (car (consult--command-split query))) nil))
         (face (or (consult-omni--get-source-prop source :face) face 'consult-omni-default-face))
         (title-str (propertize title 'face face))
         (title-str (consult-omni--set-string-width title-str (* 6 frame-width-percent)))
         (str (concat title-str
                      (when buff (concat "\t" buff))
                      (when pos (concat "\s\s" pos ))
                      (when source (concat "\t" source))))
         )
    (if consult-omni-highlight-matches
        (cond
         ((listp match-str)
          (mapcar (lambda (match) (setq str (consult-omni--highlight-match match str t))) match-str))
         ((stringp match-str)
          (setq str (consult-omni--highlight-match match-str str t)))))
    str))

(defun consult-omni--line-multi-preview (cand)
"Preview function for consult-omni-line-multi."
  (let* ((marker (car (get-text-property 0 :marker cand)))
         (query (get-text-property 0 :query cand)))
    (consult--jump marker)
       ))

(consult-omni-define-source "buffers text search"
                           :narrow-char ?L
                           :type 'sync
                           :category 'consult-location
                           :face 'default
                           :request #'consult-omni--line-multi-fetch-results
                           :preview-key consult-preview-key
                           :search-history 'consult-omni--search-history
                           :selection-history 'consult-omni--selection-history
                           :on-preview #'consult-omni--line-multi-preview
                           :on-return #'identity
                           :on-callback #'consult-omni--line-multi-preview
                           :enabled (lambda () (fboundp 'consult-omni--line-multi-candidates))
                           :group #'consult-omni--group-function
                           :sort t
                           :static 'both
                           :annotate nil
                           )

;;; provide `consult-omni-line-multi' module

(provide 'consult-omni-line-multi)

(add-to-list 'consult-omni-sources-modules-to-load 'consult-omni-line-multi)
;;; consult-omni-line-multi.el ends here
