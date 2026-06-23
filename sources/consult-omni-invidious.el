;;; consult-omni-invidious.el --- Consulting Invidious -*- lexical-binding: t -*-

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
;; consult-omni-invidious provides commands for searching Invidious in Emacs
;; using consult-omni.

;;; Code:

(require 'consult-omni)

(defvar consult-omni-invidious-servers nil
  "List of “Invidious” API servers.")

(defvar consult-omni-invidious-server-url "https://api.invidious.io/instances.json"
  "URL to fetch “Invidious” API servers.")

(defvar consult-omni-invidious-watch-url "https://www.youtube.com/watch"
  "Watch URL of Invidous.")

(defvar consult-omni-invidious-channel-url "https://www.youtube.com/channel/"
  "Channel URL of YouTube.")

(defun consult-omni--invidious-get-servers (&optional rotate)
  "Get list of Invidious API servers.

When ROTATE is no-nil roate the order of items in
`consult-omni-invidious-server'."
  (when (and consult-omni-invidious-servers rotate)
    (setq consult-omni-invidious-servers
          (nconc (cdr consult-omni-invidious-servers)
                 (list (car consult-omni-invidious-servers)))))
  (or consult-omni-invidious-servers
      (setq consult-omni-invidious-servers
            (let ((params '(("pretty" . "1")
                            ("sort_by" . "type"))))
              (consult-omni--fetch-url
               consult-omni-invidious-server-url
               consult-omni-http-retrieve-backend
               :params params
               :sync t
               :parser #'consult-omni--json-parse-buffer
               :callback (lambda (attrs)
                           (delq nil (mapcar (lambda (item)
                                               (when (equal (gethash "api" (cadr item)) t)
                                                 (gethash "uri" (cadr item))))
                                             attrs))))))))

(cl-defun consult-omni--invidious-format-candidate (&rest args &key source type query title snippet channeltitle date subcount videocount viewcount length face &allow-other-keys)
  "Format a candidate for `consult-omni-invidious' commands with ARGS.

Description of Arguments:

  SOURCE       a string; the name to use (e.g. “Invidious”)
  TYPE         a string; the type of candidate
               (e.g. video, channel, playlist)
  QUERY        a string; query input from the user
  TITLE        a string; the title of the video
  SNIPPET      a string; a snippet/description of the video
  CHANNELTITLE a string; the name of the channel for the video
  DATE         a string; the publish date of the video
  SUBCOUNT     an integer; the subscriber count fpr a channel
  VIDEOCOUNT   an integer; the number of videos in a playlist
  VIEWCOUNT    an integer; the number of times a video is viewed
  LENGTH       a number; the duration of a video in seconds
  FACE         a symbol; the face to apply to TITLE"
  (let* ((frame-width-percent (floor (* (frame-width) 0.1)))
         (source (propertize source 'face 'consult-omni-source-type-face))
         (match-str (if (and (stringp query) (not (equal query ".*"))) (consult--split-escaped query) nil))
         (videocount-str (and videocount (consult-omni--numbers-human-readable (or videocount 0) "videos")))
         (viewcount-str (and viewcount (consult-omni--numbers-human-readable (or viewcount 0) "views")))
         (subcount-str (and subcount (consult-omni--numbers-human-readable (or subcount 0) "subs")))
         (stats (and type
                     (stringp type)
                     (propertize
                      (consult-omni--set-string-width
                       (pcase type
                         ("video" (format "%s" (or viewcount-str "0 views")))
                         ("playlist" (format "%s" (or videocount-str "0 videos")))
                         ("channel" (format "%s" (or subcount-str "0 subscriptions")))
                         (_ ""))
                       10)
                      'face 'consult-omni-domain-face)))
         (length (or
                  (and (numberp length) (seconds-to-string length))
                  (and (equal type "playlist") "[PLAYLIST]")
                  (and (equal type "channel") "(CHANNEL)")))
         (length (and (stringp length) (consult-omni--set-string-width (propertize length 'face 'consult-omni-comment-face) 10)))
         (date (propertize (or (and (stringp date) date) (make-string 10 ?\s)) 'face 'consult-omni-date-face))
         (channeltitle (and channeltitle (stringp channeltitle) (propertize channeltitle 'face 'consult-omni-path-face)))
         (channeltitle (consult-omni--set-string-width channeltitle (* 2 frame-width-percent)))
         (snippet (if (stringp snippet) (consult-omni--set-string-width (replace-regexp-in-string "\n" "  " snippet) (* 2 frame-width-percent))))
         (snippet (and snippet (stringp snippet) (propertize snippet 'face 'consult-omni-snippet-face)))
         (face (or (consult-omni--get-source-prop source :face) face 'consult-omni-default-face))
         (title-str (and title (propertize title 'face face)))
         (title-str (consult-omni--set-string-width title-str (* 5 frame-width-percent)))
         (str (concat title-str
                      (when date (concat "\s" date))
                      (when channeltitle (concat " " channeltitle))
                      (propertize " " 'display `(space :align-to ,(+ (* 5 frame-width-percent)                                                                      11)))
                      (when length (concat "\s" length))
                      (unless (string-empty-p stats) (concat "\s" stats))
                      (when snippet (concat "\s\s" snippet))
                      (concat "\t" source))))
    (if consult-omni-highlight-matches-in-minibuffer
        (cond
         ((listp match-str)
          (mapc (lambda (match) (setq str (consult-omni--highlight-match match str t))) match-str))
         ((stringp match-str)
          (setq str (consult-omni--highlight-match match-str str t)))))
    str))

(cl-defun consult-omni--invidious-fetch-results (input &rest args &key callback &allow-other-keys)
  "Fetch search results for INPUT from “Invidious” service with ARGS.

CALLBACK is a function used internally to update the list of candidates in
the minibuffer asynchronously.  It is called with a list of strings, which
are new annotated candidates \(e.g. as they arrive from an asynchronous
process\) to be added to the minibuffer completion cnadidates.  See the
section on REQUEST in documentation for `consult-omni-define-source' as
well as the function
`consult-omni--multi-update-dynamic-candidates' for how CALLBACK is used."
  (pcase-let* ((`(,query . ,opts) (consult-omni--split-command input (seq-difference args (list :callback callback))))
               (opts (car-safe opts))
               (count (plist-get opts :count))
               (type (plist-get opts :type))
               (order (or (plist-get opts :order) (plist-get opts :sort)))
               (subs (or (plist-get opts :subs) (plist-get opts :subscriptions)))
               (searchdate (plist-get opts :date))
               (searchdate (if searchdate (format "%s" searchdate)))
               (features (plist-get opts :features))
               (features (if features (format "%s" features)))
               (duration (plist-get opts :dur))
               (duration (if duration (format "%s" duration)))
               (count (or (and count (integerp (read count)) (string-to-number count))
                          consult-omni-default-count))
               (order  (if (and order (member (format "%s" order) '("date" "rating" "relevance" "upload_date" "views" "view_count"))) (format "%s" order) "relevance"))
               (type (if (and type (member (format "%s" type) '("channel" "playlist" "video" "movie" "show" "all"))) (format "%s" type) "video"))
               (params (delq nil `(("q" . ,(replace-regexp-in-string " " "+" query))
                                   ("sort_by" . ,order)
                                   ("type" . ,type)
                                   ("max_results" . ,(format "%s" count))
                                   ,(when searchdate `("date" . ,searchdate))
                                   ,(when features `("features" . ,features))
                                   ,(when duration `("duration" . ,duration))
                                   ,(when subs `("subscriptions" . ,(if subs "true" "false"))))))
               (server-url (car (consult-omni--invidious-get-servers)))
               (api-url (concat server-url "/api/v1/search?")))
    (consult-omni--fetch-url api-url consult-omni-http-retrieve-backend
                             :encoding 'utf-8
                             :params params
                             :parser #'consult-omni--json-parse-buffer
                             :callback
                             (lambda (attrs)
                               (let* ((raw-results attrs)
                                      (annotated-results
                                       (mapcar (lambda (item)
                                                 (let*
                                                     ((source "Invidious")
                                                      (item-type (gethash "type" item))
                                                      (channelhandle (gethash "channelHandle" item))
                                                      (title (or (gethash "title" item)
                                                                 (unless (eq channelhandle :null) channelhandle)
                                                                 (gethash "author" item)))
                                                      (videos  (gethash "videos" item))
                                                      (videoid (or (gethash "videoId" item)
                                                                   (and videos (gethash "videoId" (car videos)))))
                                                      (channeltitle (gethash "author" item))
                                                      (channelid (gethash "authorId" item))
                                                      (playlistid (gethash "playlistId" item))
                                                      (videocount (gethash "videoCount" item))
                                                      (subcount (gethash "subCount" item))
                                                      (viewcount (gethash "viewCount" item))
                                                      (videolength (gethash "lengthSeconds" item))
                                                      (date (gethash "published" item))
                                                      (date (when date (format-time-string "%Y-%m-%d" (seconds-to-time date))))
                                                      (url (cond
                                                            ((and playlistid videoid)
                                                             (consult-omni--make-url-string
                                                              consult-omni-invidious-watch-url
                                                              `(("v" . ,videoid)
                                                                ("list" . ,playlistid))))
                                                            (playlistid (consult-omni--make-url-string consult-omni-invidious-watch-url `(("list" . ,playlistid))))
                                                            (videoid (consult-omni--make-url-string consult-omni-invidious-watch-url `(("v" . ,videoid))))
                                                            (channelid (concat consult-omni-invidious-channel-url channelid))))
                                                      (search-url (consult-omni--make-url-string server-url params))
                                                      (description (gethash "description" item))
                                                      (decorated (consult-omni--invidious-format-candidate :source source :type item-type :query query :title title :snippet description :channeltitle channeltitle :date date :subcount subcount :videocount videocount :viewcount viewcount :length videolength)))
                                                   (propertize decorated
                                                               :source source
                                                               :title title
                                                               :url url
                                                               :search-url search-url
                                                               :query query
                                                               :snippet description
                                                               :videoid videoid
                                                               :channeltitle channeltitle
                                                               :channelid channelid
                                                               :duration duration
                                                               :views viewcount
                                                               :videocount videocount
                                                               :subscriptions subcount)))
                                               raw-results)))
                                 (when (and annotated-results (functionp callback))
                                   (funcall callback annotated-results))
                                 annotated-results)))))

;; Define the Invidious source
(consult-omni-define-source "Invidious"
                            :narrow-char ?y
                            :type 'dynamic
                            :require-match t
                            :category 'consult-omni-video
                            :face 'consult-omni-engine-title-face
                            :request #'consult-omni--invidious-fetch-results
                            :preview-key consult-omni-preview-key
                            :search-hist 'consult-omni--search-history
                            :select-hist 'consult-omni--selection-history
                            :enabled (lambda () (bound-and-true-p consult-omni-invidious-server-url))
                            :group #'consult-omni--group-function
                            :sort t
                            :interactive consult-omni-intereactive-commands-type
                            :annotate nil)

;;; provide `consult-omni-invidious' module

(provide 'consult-omni-invidious)

(add-to-list 'consult-omni-sources-modules-to-load 'consult-omni-invidious)
;;; consult-omni-invidious.el ends here
