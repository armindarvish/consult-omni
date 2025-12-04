;;; consult-omni-youtube.el --- Consulting YouTube -*- lexical-binding: t -*-

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

;;; Commentary:
;; consult-omni-youtube provides commands for searching YouTube in Emacs
;; using consult-omni as the frontend.

;;; Code:

(require 'consult-omni)
(require 'iso8601)

(defcustom consult-omni-youtube-search-key nil
  "Key for “YouTube Data API”.

Can be a key string or a function that returns a key string.

See URL `https://developers.google.com/youtube/v3/getting-started'
for details"
  :group 'consult-omni
  :type '(choice (const :tag "API Key" string)
                 (function :tag "Custom Function")))

(defcustom consult-omni-youtube-search-command #'consult-omni--youtube-fetch-results-details
  "Command to use to get results from “YouTube Data API”."
  :group 'consult-omni
  :type '(choice (function :tag "(Default) Detailed Results with stats" #'consult-omni--youtube-fetch-results-details)
                 (function :tag "Simple Results without stats"  #'consult-omni--youtube-fetch-results-simple)
                 (function :tag "Custom Function")))

(defvar consult-omni-youtube-base-url "https://www.youtube.com/"
  "Base URL of YouTube.")

(defvar consult-omni-youtube-watch-url "https://www.youtube.com/watch"
  "Watch URL of YouTube.")

(defvar consult-omni-youtube-channel-url "https://www.youtube.com/channel/"
  "Channel URL of YouTube.")

(defvar consult-omni-youtube-search-results-url "https://www.youtube.com/results"
  "Search Results URL of YouTube.")

(defvar consult-omni-youtube-search-api-url "https://www.googleapis.com/youtube/v3/search"
  "API URL of YouTube Search.")

(defvar consult-omni-youtube-videos-api-url "https://www.googleapis.com/youtube/v3/videos"
  "API URL for YouTube Videos.")

(defvar consult-omni-youtube-playlists-api-url "https://www.googleapis.com/youtube/v3/playlists"
  "API URL for YouTube Playlists.")

(defvar consult-omni-youtube-channels-api-url "https://www.googleapis.com/youtube/v3/channels"
  "API URL for YouTube Channels.")

(cl-defun consult-omni--youtube-format-candidate (&rest args &key source type query title snippet channeltitle date length subcount videocount viewcount face &allow-other-keys)
  "Format a candidate from `consult-omni-youtube' with ARGS.

Description of Arguments:

  SOURCE       a string; the name to use (e.g. “YouTube”)
  TYPE         a string; the type of candidate
               \(e.g. video, channel, playlist\)
  QUERY        a string; the query input from the user
  TITLE        a string; the title of the video
  SNIPPET      a string; a snippet/description of the video
  CHANNELTITLE a string; the name of the channel for the video
  DATE         a string; the publish date of the video
  LENGTH       a number or string; the duration of a  video in seconds
  SUBCOUNT     an integer; the subscriber count for a channel
  VIDEOCOUNT   an integer; the number of videos in a playlist
  VIEWCOUNT    an integer; the number of times a video is viewed
  FACE         a symbol; the face to apply to TITLE"
  (let* ((frame-width-percent (floor (* (frame-width) 0.1)))
         (source (propertize source 'face 'consult-omni-source-type-face))
         (match-str (if (and (stringp query) (not (equal query ".*"))) (consult--split-escaped query) nil))
         (date (and (stringp date) (propertize date 'face 'consult-omni-date-face)))
         (channeltitle (and (stringp channeltitle) (propertize channeltitle 'face 'consult-omni-path-face)))
         (channeltitle (consult-omni--set-string-width channeltitle (* 2 frame-width-percent)))
         (snippet (if (stringp snippet) (consult-omni--set-string-width (replace-regexp-in-string "\n" "  " snippet) (* 2 frame-width-percent))))
         (snippet (and snippet (stringp snippet) (propertize snippet 'face 'consult-omni-snippet-face)))
         (videocount-str (and videocount (consult-omni--numbers-human-readable (or videocount 0) "videos")))
         (viewcount-str (and viewcount (consult-omni--numbers-human-readable (or viewcount 0) "views")))
         (subcount-str (and subcount (consult-omni--numbers-human-readable (or subcount 0) "subs")))
         (stats (and type
                     (stringp type)
                     (propertize
                      (consult-omni--set-string-width (pcase type
                                                        ("video" (format "%s" (or viewcount-str "0 views")))
                                                        ("playlist" (format "%s" (or videocount-str "0 videos")))
                                                        ("channel" (format "%s" (or subcount-str "0 subscriptions")))
                                                        (_ "")) 10)
                      'face 'consult-omni-domain-face)))
         (length (or
                  (and (equal type "playlist") "[PLAYLIST]")
                  (and (equal type "channel") "(CHANNEL)")
                  (and (numberp length) (seconds-to-string length))))
         (length (and (stringp length) (consult-omni--set-string-width (propertize length 'face 'consult-omni-comment-face) 10)))
         (face (or (consult-omni--get-source-prop source :face) face 'consult-omni-default-face))
         (title-str (propertize title 'face face))
         (title-str (consult-omni--set-string-width title-str (* 5 frame-width-percent)))
         (str (concat title-str
                      (when date (concat "\s" date))
                      (when channeltitle (concat " " channeltitle))
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

(cl-defun consult-omni--youtube-fetch-results-simple (input &rest args &key callback &allow-other-keys)
  "Fetch search results for INPUT and ARGS from “YouTube Data API” service.

This is a simpler version that does not show details
such as view counts and duration, ... of videos/playlists, etc.

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
               (def (plist-get opts :def))
               (type (plist-get opts :type))
               (vidtype (plist-get opts :vidtype))
               (order (or (plist-get opts :order) (plist-get opts :sort)))
               (count (or (and count (integerp (read count)) (string-to-number count))
                          consult-omni-default-count))
               (def (if (and def (member (format "%s" def) '("any" "standard" "high"))) (format "%s" def) "any"))
               (type (if (and type (member (format "%s" type) '("channel" "playlist" "video"))) (format "%s" type) "video"))
               (vidtype (if (and vidtype (member (format "%s" vidtype) '("any" "episode" "movie"))) (format "%s" vidtype) "any"))
               (count (min count 10))
               (order  (if (and order (member (format "%s" order) '("date" "rating" "relevance" "title" "videoCount" "viewCount"))) (format "%s" order) "relevance"))
               (params `(("q" . ,(replace-regexp-in-string " " "+" query))
                         ("part" . "snippet")
                         ("order" . ,order)
                         ("type" . ,type)
                         ("maxResults" . ,(format "%s" count))
                         ("videoDefinition" . ,def)
                         ("videoType" . ,vidtype)
                         ("key" . ,(consult-omni-expand-variable-function consult-omni-youtube-search-key))))
               (headers `(("Accept" . "application/json")
                          ("Accept-Encoding" . "gzip")
                          ("User-Agent" . "consult-omni (gzip)")
                          ("X-Goog-Api-Key" . ,(consult-omni-expand-variable-function consult-omni-youtube-search-key)))))
    (consult-omni--fetch-url consult-omni-youtube-search-api-url consult-omni-http-retrieve-backend
                             :encoding 'utf-8
                             :params params
                             :headers headers
                             :parser #'consult-omni--json-parse-buffer
                             :callback
                             (lambda (attrs)
                               (let* ((raw-results (gethash "items" attrs))
                                      (annotated-results
                                       (mapcar (lambda (item)
                                                 (let*
                                                     ((source "YouTube")
                                                      (videoid (gethash "videoId" (gethash "id" item)))
                                                      (snippet (gethash "snippet" item))
                                                      (channeltitle (gethash "channelTitle" snippet))
                                                      (channelid (gethash "channelId" snippet))
                                                      (title (gethash "title" snippet))
                                                      (date (gethash "publishedAt" snippet))
                                                      (date (format-time-string "%Y-%m-%d" (date-to-time date)))
                                                      (url (cond
                                                            (videoid (consult-omni--make-url-string consult-omni-youtube-watch-url `(("v" . ,videoid))))
                                                            (channelid (concat consult-omni-youtube-channel-url channelid))))
                                                      (search-url (consult-omni--make-url-string consult-omni-youtube-search-results-url `(("search_query" . ,query))))
                                                      (description (gethash "description" snippet))
                                                      (decorated (consult-omni--youtube-format-candidate :source source :query query :title title :snippet description :channeltitle channeltitle :date date)))
                                                   (propertize decorated
                                                               :source source
                                                               :title title
                                                               :url url
                                                               :search-url search-url
                                                               :query query
                                                               :snippet description
                                                               :videoid videoid
                                                               :channeltitle channeltitle
                                                               :channelid channelid)))
                                               raw-results)))
                                 (when (and annotated-results (functionp callback))
                                   (funcall callback annotated-results))
                                 annotated-results)))))

(cl-defun consult-omni--youtube-fetch-video-details (videoids &rest args &key callback query &allow-other-keys)
  "Fetch details with VIDEOIDS and ARGS from “YouTube Data API” service.

QUERY is the user input string.

CALLBACK is a function used internally to update the list of candidates in
the minibuffer asynchronously.  It is called with a list of strings, which
are new annotated candidates \(e.g. as they arrive from an asynchronous
process\) to be added to the minibuffer completion cnadidates.  See the
section on REQUEST in documentation for `consult-omni-define-source' as
well as the function
`consult-omni--multi-update-dynamic-candidates' for how CALLBACK is used."
  (pcase-let* ((params `(("part" . "snippet,statistics,contentDetails")
                         ("key" . ,(consult-omni-expand-variable-function consult-omni-youtube-search-key))
                         ("id" . ,(string-join videoids ","))))
               (headers `(("Accept" . "application/json")
                          ("Accept-Encoding" . "gzip")
                          ("User-Agent" . "consult-omni (gzip)")
                          ("X-Goog-Api-Key" . ,(consult-omni-expand-variable-function consult-omni-youtube-search-key)))))
    (consult-omni--fetch-url consult-omni-youtube-videos-api-url consult-omni-http-retrieve-backend
                             :encoding 'utf-8
                             :params params
                             :headers headers
                             :parser #'consult-omni--json-parse-buffer
                             :callback
                             (lambda (attrs)
                               (let* ((raw-results (gethash "items" attrs))
                                      (annotated-results
                                       (mapcar (lambda (item)
                                                 (let*
                                                     ((source "YouTube")
                                                      (videoid (gethash "id" item))
                                                      (snippet (gethash "snippet" item))
                                                      (contentdetails (gethash "contentDetails" item))
                                                      (statistics (gethash "statistics" item))
                                                      (channeltitle (gethash "channelTitle" snippet))
                                                      (channelid (gethash "channelId" snippet))
                                                      (title (gethash "title" snippet))
                                                      (date (gethash "publishedAt" snippet))
                                                      (date (format-time-string "%Y-%m-%d" (date-to-time date)))
                                                      (url (consult-omni--make-url-string consult-omni-youtube-watch-url `(("v" . ,videoid))))
                                                      (search-url (consult-omni--make-url-string consult-omni-youtube-search-results-url `(("search_query" . ,query))))
                                                      (description (gethash "description" snippet))
                                                      (duration (gethash "duration" contentdetails))
                                                      (duration (if duration (iso8601-parse-duration duration)))
                                                      (duration (if duration (+ (* (caddr duration) 2600) (* (cadr duration) 60) (car duration))))
                                                      (viewcount (string-to-number (gethash "viewCount" statistics)))
                                                      (decorated (consult-omni--youtube-format-candidate :source source :type "video" :query query :title title :snippet description :channeltitle channeltitle :date date :length duration :viewcount viewcount)))
                                                   (propertize decorated
                                                               :source source
                                                               :title title
                                                               :url url
                                                               :search-url search-url
                                                               :query query
                                                               :snippet description
                                                               :id videoid
                                                               :channeltitle channeltitle
                                                               :channelid channelid)))
                                               raw-results)))
                                 (when (and annotated-results (functionp callback))
                                   (funcall callback annotated-results))
                                 annotated-results)))))

(cl-defun consult-omni--youtube-fetch-playlist-details (playlistids &rest args &key callback query &allow-other-keys)
  "Fetch details with PLAYLISTIDS and ARGS from “YouTube Data API”.

QUERY is th euser input string and CANDIDATES
CALLBACK is a function used internally to update the list of candidates in
the minibuffer asynchronously.  It is called with a list of strings, which
are new annotated candidates \(e.g. as they arrive from an asynchronous
process\) to be added to the minibuffer completion cnadidates.  See the
section on REQUEST in documentation for `consult-omni-define-source' as
well as the function
`consult-omni--multi-update-dynamic-candidates' for how CALLBACK is used."
  (pcase-let* ((params `(("part" . "snippet,contentDetails")
                         ("key" . ,(consult-omni-expand-variable-function consult-omni-youtube-search-key))
                         ("id" . ,(string-join playlistids ","))))
               (headers `(("Accept" . "application/json")
                          ("Accept-Encoding" . "gzip")
                          ("User-Agent" . "consult-omni (gzip)")
                          ("X-Goog-Api-Key" . ,(consult-omni-expand-variable-function consult-omni-youtube-search-key)))))
    (consult-omni--fetch-url consult-omni-youtube-playlists-api-url consult-omni-http-retrieve-backend
                             :encoding 'utf-8
                             :params params
                             :headers headers
                             :parser #'consult-omni--json-parse-buffer
                             :callback
                             (lambda (attrs)
                               (let* ((raw-results (gethash "items" attrs))
                                      (annotated-results
                                       (mapcar (lambda (item)
                                                 (let*
                                                     ((source "YouTube")
                                                      (playlistid (gethash "id" item))
                                                      (snippet (gethash "snippet" item))
                                                      (contentdetails (gethash "contentDetails" item))
                                                      (videocount (gethash "itemCount" contentdetails))
                                                      (channeltitle (gethash "channelTitle" snippet))
                                                      (channelid (gethash "channelId" snippet))
                                                      (title (gethash "title" snippet))
                                                      (date (gethash "publishedAt" snippet))
                                                      (date (format-time-string "%Y-%m-%d" (date-to-time date)))
                                                      (url (consult-omni--make-url-string consult-omni-youtube-watch-url `(("list" . ,playlistid))))
                                                      (search-url (consult-omni--make-url-string consult-omni-youtube-search-results-url `(("search_query" . ,query))))
                                                      (description (gethash "description" snippet))
                                                      (decorated (consult-omni--youtube-format-candidate :source source :type "playlist" :query query :title title :snippet description :channeltitle channeltitle :date date :videocount videocount)))
                                                   (propertize decorated
                                                               :source source
                                                               :title title
                                                               :url url
                                                               :search-url search-url
                                                               :query query
                                                               :snippet description
                                                               :id playlistid
                                                               :channeltitle channeltitle
                                                               :channelid channelid)))
                                               raw-results)))
                                 (when (and annotated-results (functionp callback))
                                   (funcall callback annotated-results))
                                 annotated-results)))))

(cl-defun consult-omni--youtube-fetch-channel-details (channelids &rest args &key callback query &allow-other-keys)
  "Fetch details with CHANNELIDS and ARGS from “YouTube Data API”.

QUERY is the user's input string.

CALLBACK is a function used internally to update the list of candidates in
the minibuffer asynchronously.  It is called with a list of strings, which
are new annotated candidates \(e.g. as they arrive from an asynchronous
process\) to be added to the minibuffer completion cnadidates.  See the
section on REQUEST in documentation for `consult-omni-define-source' as
well as the function
`consult-omni--multi-update-dynamic-candidates' for how CALLBACK is used."
  (pcase-let* ((params `(("part" . "snippet,statistics")
                         ("key" . ,(consult-omni-expand-variable-function consult-omni-youtube-search-key))
                         ("id" . ,(string-join channelids ","))))
               (headers `(("Accept" . "application/json")
                          ("Accept-Encoding" . "gzip")
                          ("User-Agent" . "consult-omni (gzip)")
                          ("X-Goog-Api-Key" . ,(consult-omni-expand-variable-function consult-omni-youtube-search-key)))))
    (consult-omni--fetch-url consult-omni-youtube-channels-api-url consult-omni-http-retrieve-backend
                             :encoding 'utf-8
                             :params params
                             :headers headers
                             :parser #'consult-omni--json-parse-buffer
                             :callback
                             (lambda (attrs)
                               (let* ((raw-results (gethash "items" attrs))
                                      (annotated-results
                                       (mapcar (lambda (item)
                                                 (let*
                                                     ((source "YouTube")
                                                      (channelid (gethash "id" item))
                                                      (snippet (gethash "snippet" item))
                                                      (statistics (gethash "statistics" item))
                                                      (title (gethash "title" snippet))
                                                      (customurl (gethash "customUrl" snippet))
                                                      (date (gethash "publishedAt" snippet))
                                                      (date (format-time-string "%Y-%m-%d" (date-to-time date)))
                                                      (subcount (gethash "subscriberCount" statistics))
                                                      (url (if customurl
                                                               (concat consult-omni-youtube-base-url customurl)
                                                             (concat consult-omni-youtube-channel-url channelid)))
                                                      (search-url (consult-omni--make-url-string consult-omni-youtube-search-results-url `(("search_query" . ,query))))
                                                      (description (gethash "description" snippet))
                                                      (decorated (consult-omni--youtube-format-candidate :source source :type "channel" :query query :title title :snippet description :channeltitle title :date date :subcount subcount)))
                                                   (propertize decorated
                                                               :source source
                                                               :title title
                                                               :url url
                                                               :search-url search-url
                                                               :query query
                                                               :snippet description
                                                               :id channelid
                                                               :channeltitle title
                                                               :channelid channelid)))
                                               raw-results)))
                                 (when (and annotated-results (functionp callback))
                                   (funcall callback annotated-results))
                                 annotated-results)))))

(cl-defun consult-omni--youtube-fetch-results-details (input &rest args &key callback &allow-other-keys)
  "Fetch search results for INPUT and ARGS from “YouTube Data API”.

This is a version with  statistics \(e.g. view counts\) and more details
on videos, playlsits, etc.

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
               (def (plist-get opts :def))
               (search-type (plist-get opts :type))
               (vidtype (plist-get opts :vidtype))
               (order (or (plist-get opts :order) (plist-get opts :sort)))
               (count (or (and count (integerp (read count)) (string-to-number count))
                          consult-omni-default-count))
               (def (if (and def (member (format "%s" def) '("any" "standard" "high"))) (format "%s" def)))
               (vidtype (if (and vidtype (member (format "%s" vidtype) '("any" "episode" "movie"))) (format "%s" vidtype)))
               (search-type (cond
                             ((or def vidtype) "video")
                             ((and search-type (member (format "%s" search-type) '("channel" "playlist" "video"))) (format "%s" search-type))))
               (count (min count 100))
               (order  (if (and order (member (format "%s" order) '("date" "rating" "relevance" "title" "videoCount" "viewCount"))) (format "%s" order) "relevance"))
               (params (delq nil `(("q" . ,(replace-regexp-in-string " " "+" query))
                                   ("part" . "snippet")
                                   ("order" . ,order)
                                   ("maxResults" . ,(format "%s" count))
                                   ,(when search-type `("type" . ,(format "%s" search-type)))
                                   ,(when def `("videoDefinition" . ,def))
                                   ,(when vidtype `("videoType" . ,vidtype))
                                   ("key" . ,(consult-omni-expand-variable-function consult-omni-youtube-search-key)))))
               (headers `(("Accept" . "application/json")
                          ("Accept-Encoding" . "gzip")
                          ("User-Agent" . "consult-omni (gzip)")
                          ("X-Goog-Api-Key" . ,(consult-omni-expand-variable-function consult-omni-youtube-search-key)))))
    (consult-omni--fetch-url consult-omni-youtube-search-api-url consult-omni-http-retrieve-backend
                             :encoding 'utf-8
                             :params params
                             :headers headers
                             :parser #'consult-omni--json-parse-buffer
                             :callback
                             (lambda (attrs)
                               (let* ((raw-results (gethash "items" attrs))
                                      (videoids (list))
                                      (playlistids (list))
                                      (channelids (list)))
                                 (mapc (lambda (item)
                                         (let* ((kind (gethash "kind" (gethash "id" item)))
                                                (type (string-trim-left kind "youtube#")))
                                           (pcase type
                                             ("video"
                                              (push (gethash "videoId" (gethash "id" item)) videoids))
                                             ("playlist"
                                              (push (gethash "playlistId" (gethash "id" item)) playlistids))
                                             ("channel"
                                              (push (gethash "channelId" (gethash "id" item)) channelids)))))
                                       raw-results)
                                 (when videoids
                                   (consult-omni--youtube-fetch-video-details videoids :callback callback :query query))
                                 (when playlistids
                                   (consult-omni--youtube-fetch-playlist-details playlistids :callback callback :query query))
                                 (when channelids
                                   (consult-omni--youtube-fetch-channel-details channelids :callback callback :query query)))))))

;; Define the YouTube source
(consult-omni-define-source "YouTube"
                            :narrow-char ?y
                            :type 'dynamic
                            :require-match t
                            :category 'consult-omni-video
                            :face 'consult-omni-engine-title-face
                            :request consult-omni-youtube-search-command
                            :on-new (apply-partially #'consult-omni-external-search-with-engine "YouTube")
                            :preview-key consult-omni-preview-key
                            :search-hist 'consult-omni--search-history
                            :select-hist 'consult-omni--selection-history
                            :enabled (lambda () (bound-and-true-p consult-omni-youtube-search-key))
                            :group #'consult-omni--group-function
                            :sort t
                            :interactive consult-omni-intereactive-commands-type
                            :annotate nil)

;;; provide `consult-omni-youtube' module

(provide 'consult-omni-youtube)

(add-to-list 'consult-omni-sources-modules-to-load 'consult-omni-youtube)
;;; consult-omni-youtube.el ends here
