;;; consult-omni.el --- Emacs Omni Search Package -*- lexical-binding: t -*-

;; Copyright (C) 2024 Armin Darvish

;; Author: Armin Darvish
;; Maintainer: Armin Darvish
;; Created: 2024
;; Version: 0.3
;; Package-Requires: (
;;         (emacs "29.4")
;;         (consult "2.0"))
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
;; consult-omni is a package for getting search results from one or
;; several custom sources (web search engines, AI assistants,
;; elfeed database, org notes, local files, desktop applications,
;; mail servers, ...) directly in Emacs minibuffer.

;; consult-omni provides wrappers and macros around
;; consult (https://github.com/minad/consult), to make it easier
;; for users to get results from different sources and combine
;; local and web sources in an omni-style search.
;; In other words, consult-omni enables getting consult-style
;; multi-source or dynamically completed results in minibuffer for
;; a wide range of sources including Emacs functions/packages
;; (e.g. Emacs buffers, org files, elfeed,...), command-line programs
;; (e.g. grep, find, gh, ...), or web search engines (Google, Bing, ...).

;;; Code:

;;; Requirements
(eval-when-compile
  (require 'json)
  (require 'request nil t)
  (require 'plz nil t))
(require 'consult)
(require 'url)
(require 'url-queue)
(require 'pulse)

;;; Group
(defgroup consult-omni nil
  "Consulting omni sources."
  :group 'convenience
  :group 'minibuffer
  :group 'consult
  :group 'web
  :group 'search
  :prefix "consult-omni-")

;;; User Options (a.k.a. Custom Variables)
;;  The following user options modify the behavior of consult-omni.

(defcustom consult-omni-sources-modules-to-load  (list)
  "List of source modules/features to load.

This variable is a list of symbols;
each symbol being a source featue (e.g. consult-omni-brave)"
  :group 'consult-omni
  :type '(repeat :tag "list of source modules/features to load" symbol))

(defcustom consult-omni-intereactive-commands-type 'both
  "Type of interactive commands to make?

This variable can be a  symbol:
  \='static  only make static commands
  \='dynamic only make dynamic commands
  otherwise make both commands

dynamic commands are dynamically completed  in the minibuffer.
static commands fetch search results once without dynamic completion"
  :group 'consult-omni
  :type '(choice (const :tag "(Default) Make both static and dynamic commands" both)
                 (const :tag "Only make DYNAMIC interactive commands" dynamic)
                 (const :tag "Only make STATIC interactive commands" static)))

(defcustom consult-omni-default-browse-function #'browse-url
  "Default function when selecting a link."
  :group 'consult-omni
  :type '(choice (function :tag "(Default) Browse URL" browse-url)
                 (function :tag "Custom Function")))

(defcustom consult-omni-default-new-function #'consult-omni-external-search
  "Default function when selecting a non-existing new candidate."
  :group 'consult-omni
  :type '(choice (function :tag "(Default) Search in External Browser" consult-omni-external-search)
                 (function :tag "Custom Function")))

(defcustom consult-omni-alternate-browse-function #'eww-browse-url
  "Default function when selecting a link."
  :group 'consult-omni
  :type '(choice (function :tag "(Default) EWW" eww-browse-url)
                 (function :tag "Custom Function")))

(defcustom consult-omni-default-search-engine nil
  "Default search engine name."
  :group 'consult-omni
  :type '(choice (string :tag "Bing" "Bing")
                 (string :tag "Brave" "Brave")
                 (string :tag "DuckDuckGo" "DuckDuckGo")
                 (string :tag "Google" "Google")
                 (string :tag "Perplexity" "Perplexity")
                 (string :tag "PubMed" "PubMed")
                 (string :tag "Wikipedia" "Wikipedia")
                 (string :tag "YouTube" "YouTube")))

(defcustom consult-omni-default-preview-function #'eww-browse-url
  "Default function when previewing a link."
  :group 'consult-omni
  :type '(choice (function :tag "(Default) EWW" eww-browse-url)
                 (function :tag "Custom Function")))


(defcustom consult-omni-show-preview nil
  "Should consult-omni show previews?

This turns previews on/off globally for all consult-omni sources."
  :group 'consult-omni
  :type 'boolean)

(defcustom consult-omni-preview-key consult-preview-key
  "Preview key for consult-omni.

This is similar to `consult-preview-key' but explicitly For consult-omni."
  :group 'consult-omni
  :type '(choice (const :tag "Any Key" Any)
                 (List :tag "Debounced"
                       (const :Debounce)
                       (Float :tag "Seconds" 0.1)
                       (const Any))
                 (const :tag "No Preview" nil)
                 (Key :tag "Key")
                 (repeat :tag "List Of Keys" Key)))


(defcustom consult-omni-default-format-candidate #'consult-omni--highlight-format-candidate
  "Default function when selecting a link."
  :group 'consult-omni
  :type '(choice (function :tag "(Default) Adds Metadata and Highlights Query" #'consult-omni--highlight-format-candidate)
                 (function :tag "Simple and Fast Foramting (No Metadata)" #'consult-omni--simple-format-candidate)
                 (function :tag "Custom Function")))

(defcustom consult-omni-default-count 5
  "Number Of search results to retrieve."
  :group 'consult-omni
  :type 'integer)

(defcustom consult-omni-default-page 0
  "Offset of search results to retrieve.

If this is set to N, the first N “pages” \(or other first N items,
depending on the source search capabilities\) of the search results are
omitted and the rest are shown."
  :group 'consult-omni
  :type 'integer)

(defcustom consult-omni-default-timeout 30
  "Default timeout in seconds for synchronous requests."
  :group 'consult-omni
  :type 'integer)

(defcustom consult-omni-url-use-queue nil
  "Use `url-queue-retrieve'?"
  :group 'consult-omni
  :type 'boolean)

(defcustom consult-omni-url-queue-parallel-processes 15
  "The number of concurrent `url-queue-retrieve' processes."
  :group 'consult-omni
  :type 'integer)

(defcustom consult-omni-url-queue-timeout 120
  "How long to let a job live once it's started (in seconds)."
  :group 'consult-omni
  :type '(integer :tag "Timeout in seconds"))

(defcustom consult-omni-log-buffer-name " *consult-omni-log*"
  "String for consult-omni-log buffer name."
  :group 'consult-omni
  :type 'string)

(defcustom consult-omni-log-level nil
  "How to make logs for consult-omni requests?

This can be set to:
  \='nil:   Does not log anything
  \='info:  Logs URLs and http response header.
  \='debug: Logs URLs and the entire http response.

When non-nil, information is logged to `consult-omni-log-buffer-name'."
  :group 'consult-omni
  :type '(choice
          (const :tag "No Logging" nil)
          (const :tag "Just HTTP Header" info)
          (const :tag "Full Response" debug)))

(defcustom consult-omni-group-by :source
  "What field to use to group the results in the minibuffer?

By default it is set to :source.  but can be any of:

  nil       Do not group
  :title    group by candidate's string
  :url      group by URL
  :domain   group by the domain of the URL
  :source   group by source name
  symbol    group by another property of the candidate"
  :group 'consult-omni
  :type '(radio (const :tag "URL path" :url)
                (const :tag "Domain of URL path":domain)
                (const :tag "Name of the search engine or source" :source)
                (const :tag "Custom other field (constant)" :any)
                (const :tag "Do not group" nil)))

(defcustom consult-omni-multi-sources nil
  "List of sources used by `consult-omni-multi'.

This variable is a list of strings or symbols;
 - strings can be name of a source, a key from `consult-omni--sources-alist',
   which can be made with the convinient macro `consult-omni-define-source'
   or by using `consult-omni--make-source-from-consult-source'.
 - symbols can be other consult sources
   (see `consult-buffer-sources' for example.)"
  :group 'consult-omni
  :type '(choice (repeat :tag "list of source names" string)))

(defcustom consult-omni-highlight-matches-in-minibuffer t
  "Should `consult-omni' highlight search queries in the minibuffer?"
  :group 'consult-omni
  :type 'boolean)

(defcustom consult-omni-highlight-matches-in-file t
  "Should `consult-omni' highlight search queries in files (preview or return)?"
  :group 'consult-omni
  :type 'boolean)

(defcustom consult-omni-highlight-match-ignore-case t
  "Should `consult-omni' ignore case when highlighting matches?"
  :group 'consult-omni
  :type 'boolean)

(defcustom consult-omni-default-interactive-command #'consult-omni-multi
  "Which command should `consult-omni' call?"
  :group 'consult-omni
  :type '(choice (function :tag "(Default) multi-source dynamic search"  consult-omni-multi)
                 (function :tag "multi-source static search" consult-omni-multi-static)
                 (function :tag "Other custom interactive command")))

(defcustom consult-omni-http-retrieve-backend 'url
  "Which backend should `consult-omni' use for http requests?"
  :group 'consult-omni
  :type   '(choice
            (const :tag "(Default) Built-in Emacs's url-retrive" url)
            (const :tag "`emacs-request' backend" request)
            (const :tag "`plz' backend" plz)))

(defcustom consult-omni-default-autosuggest-command nil
  "Which command should `consult-omni' use for auto suggestion on search input?"
  :group 'consult-omni
  :type '(choice (const :tag "(Default) no autosuggestion" nil)
                 (function :tag "Brave autosuggestion (i.e. `consult-omni-brave-autosuggest')" consult-omni-brave-autosuggest)
                 (function :tag "Google autosuggestion (i.e. `consult-omni-dynamic-google-autosuggest')" consult-omni-dynamic-google-autosuggest)
                 (function :tag "Other custom interactive command")))

(defcustom consult-omni-async-min-input consult-async-min-input
  "Minimum number of characters needed, before async process is called.

This applies to dynamic collection commands, e.g., `consult-omni-google'.
This is similar to `consult-async-min-input' but specifically for
consult-omni dynamic commands.

By default inherits from `consult-async-min-input'."
  :group 'consult-omni
  :type '(natnum :tag "Number of characters"))


(defcustom consult-omni-dynamic-input-debounce consult-async-input-debounce
  "Input debounce for dynamic commands.

The dynamic collection process is started only when
there has not been new input for consult-omni-dynamic-input-debounce seconds.
This is similar to `consult-async-input-debounce' but
specifically for consult-omni dynamic commands.

By default inherits from `consult-async-input-debounce'."
  :group 'consult-omni
  :type '(float :tag "delay in seconds"))

(defcustom consult-omni-dynamic-input-throttle consult-async-input-throttle
  "Input throttle for dynamic commands.

The dynamic collection process is started only every
`consult-omni-dynamic-input-throttle' seconds.  This is similar
to `consult-async-input-throttle' but specifically for
consult-omni dynamic commands.

By default inherits from `consult-async-input-throttle'."
  :group 'consult-omni
  :type '(float :tag "delay in seconds"))

(defcustom consult-omni-dynamic-refresh-delay consult-async-refresh-delay
  "Refreshing delay of the completion UI or dynamic commands.

The completion UI is only updated every
`consult-omni-dynamic-refresh-delay' seconds.
This is similar to `consult-async-refresh-delay' but specifically
for consult-omni dynamic commands.

By default inherits from `consult-async-refresh-delay'."
  :group 'consult-omni
  :type '(float :tag "delay in seconds"))

(defcustom consult-omni-search-engine-alist '(("Bing" . "https://www.bing.com/search?q=%s")
                                              ("Brave" .  "https://search.brave.com/search?q=%s")
                                              ("DuckDuckGo" . "https://duckduckgo.com/?q=%s")
                                              ("Google" . "https://www.google.com/search?q=%s")
                                              ("Perplexity" .  "https://www.perplexity.ai/search?q=%s")
                                              ("PubMed" . "https://pubmed.ncbi.nlm.nih.gov/?q=%s")
                                              ("Wikipedia" . "https://en.wikipedia.org/wiki/Special:Search/%s")
                                              ("YouTube" . "https://www.youtube.com/search?q=%s")
                                              ("gptel" . #'consult-omni--gptel-preview)
                                              ("Other" . #'consult-omni--choose-other-source-for-new))
  "Alist of search engine names and URLs.

car of each item is the name of the engine
cdr of items must be either:
- a search url string with %s for the query
- an elisp funciton that takes a single string input for query"
  :group 'consult-omni
  :type '(alist :key-type string :value-type (choice (string :tag "a search url string with %s for the query")
                                                     (function :tag " an elisp funciton that takes a single string input for query"))))

;;; Other Variables

;;  The following variables define search categories.

(defvar consult-omni-sources--all-modules-list (list)
  "List of all source modules.")

(defvar consult-omni-category 'consult-omni
  "Category symbol for the consult-omni seach.")

(defvar consult-omni-scholar-category 'consult-omni-scholar
  "Category symbol for scholar search.")

(defvar consult-omni-apps-category 'consult-omni-apps
  "Category symbol for app launcher.")

(defvar consult-omni-calc-category 'consult-omni-calc
  "Category symbol for calculators.")

(defvar consult-omni-video-category 'consult-omni-video
  "Category symbol for video search.")

(defvar consult-omni-dictionary-category 'consult-omni-dictionary
  "Category symbol for dictionary search.")

(defvar consult-omni-process-category 'consult-omni-process
  "Category symbol for process search.")

;;  The following history variables store search histories for
;;  different categories.

(defvar consult-omni--selection-history (list)
  "History variable that keeps selected items.")

(defvar consult-omni--search-history (list)
  "History variable that keeps search terms.")

(defvar consult-omni--email-select-history (list)
  "History variable that keeps selected email result.")

(defvar consult-omni--calc-select-history (list)
  "History variable that keeps selected calculator result.")

(defvar consult-omni--apps-select-history (list)
  "History variable that keeps list of apps launched.")

;;  The following variables are generally for internal use

(defvar consult-omni--sources-alist (list)
  "Alist of all sources.

This is an alist mapping source names to source property lists.
This alist is used to define how to process data form
a source (e.g. format data) or find what commands to run on
selecting candidates from a source, etc.

You can use the convinient macro `consult-omni-define-source'
or the command `consult-omni--make-source-from-consult-source'
to add to this alist.")

(defvar consult-omni--hidden-buffers-list (list)
  "List of currently open hidden buffers.")

(defvar consult-omni--override-group-by nil
  "Override grouping in `consult-group' based on user input.

This is used in dynamic collection to change grouping.")

(defconst consult-omni--http-end-of-headers-regexp
  (rx (or "\r\n\r\n" "\n\n"))
  "Regular expression matching the end of HTTP headers.")

(defvar consult-omni--async-processes (list)
  "List of processes for async candidates colleciton.")

(defvar consult-omni--dynamic-timers (list)
  "List of timers for dynamic candidates colleciton.")

(defvar consult-omni--async-log-buffer " *consult-omni--async-log*"
  "Name of buffer for logging async processes info.")

(defvar consult-omni--async-log-buffer " *consult-omni--async-log*"
  "Name of buffer for logging async processes info.")

(defvar consult-omni--min-timeout 2
  "Minimum timeout in seconds for `consult-omni--multi-static'.")

(defvar consult-omni--max-timeout 120
  "Maximum timeout in seconds for `consult-omni--multi-static'.")

(defvar consult-omni--slow-warning-message "Give me a few seconds to sort it out in this big mess!"
  "The message to show when collection takes a long time.")

(defvar url-http-end-of-headers)

;;; Faces

(defface consult-omni-default-face
  `((t :inherit 'default))
  "Face used for items in minibuffer.")

(defface consult-omni-prompt-face
  `((t :inherit 'font-lock-variable-use-face))
  "Face used for prompts in minibuffer.")

(defface consult-omni-annotation-1-face
  `((t :inherit 'font-lock-builtin-face))
  "Face used for prompts in minibuffer.")

(defface consult-omni-annotation-2-face
  `((t :inherit 'font-lock-constant-face))
  "Face used for prompts in minibuffer.")

(defface consult-omni-annotation-3-face
  `((t :inherit 'font-lock-type-face))
  "Face used for prompts in minibuffer.")

(defface consult-omni-success-face
  `((t :inherit 'success))
  "Face used for sucess indication.")

(defface consult-omni-warning-face
  `((t :inherit 'font-lock-warning-face))
  "Face used for warning indication.")

(defface consult-omni-error-face
  `((t :inherit 'error))
  "Face used for error indication.")

(defface consult-omni-engine-title-face
  `((t :inherit 'font-lock-variable-use-face))
  "Face used for search engine source types in minibuffer.")

(defface consult-omni-ai-title-face
  `((t :inherit 'font-lock-operator-face))
  "Face used for AI assistant source types in minibuffer.")

(defface consult-omni-files-title-face
  `((t :inherit 'consult-file))
  "Face used for file source types in minibuffer.")

(defface consult-omni-notes-title-face
  `((t :inherit 'font-lock-bracket-face))
  "Face used for notes source types in minibuffer.")

(defface consult-omni-scholar-title-face
  `((t :inherit 'font-lock-function-call-face))
  "Face used for academic literature source types in minibuffer.")

(defface consult-omni-source-type-face
  `((t :inherit 'font-lock-comment-face))
  "Face used for source annotation in minibuffer.")

(defface consult-omni-date-face
  `((t :inherit 'font-lock-preprocessor-face))
  "Face used for date annotation in minibuffer.")

(defface consult-omni-domain-face
  `((t :inherit 'font-lock-string-face))
  "Face used for domain annotation in minibuffer.")

(defface consult-omni-path-face
  `((t :inherit 'font-lock-string-face))
  "Face used for path annotation in minibuffer.")

(defface consult-omni-snippet-face
  `((t :inherit 'font-lock-doc-face))
  "Face used for snippet annotation in minibuffer.")

(defface consult-omni-keyword-face
  `((t :inherit 'font-lock-keyword-face))
  "Face used for keyword annotation in minibuffer.")

(defface consult-omni-comment-face
  `((t :inherit 'font-lock-comment-face))
  "Face used for comment annotation in minibuffer.")

(defface consult-omni-highlight-match-face
  `((t :inherit 'consult-highlight-match))
  "Face used for highlighting matches in minibuffer.")

(defface consult-omni-preview-match-face
  `((t :inherit 'consult-preview-match))
  "Face used for hilighlighting matches in preview buffer.")

;;; Backend Functions
;;  These functions are meant for internal use and/or programmers

(defun consult-omni-properties-to-plist (string &optional ignore-keys)
  "Return a plist of the text properties of STRING.

Ommits keys in IGNORE-KEYS."
  (let ((properties (text-properties-at 0 string))
        (pl nil))
    (cl-loop for k in properties
             when (keywordp k)
             do (unless (member k ignore-keys) (push (list k (plist-get properties k)) pl)))
    (apply #'append pl)))

(defun consult-omni-propertize-by-plist (item props &optional beg end)
  "Propertize ITEM by PROPS plist.

When BEG and or END are non-nil, adds properties to positions BEG to END."
  (if (stringp item)
      (if (or beg end)
          (let ((beg (or beg 0))
                (end (if (and end (< end 0))
                         (+ (length item) end)
                       (and end (min end (length item))))))
            (add-text-properties beg end props item)
            item)
        (apply #'propertize item props))
    nil))

(defun consult-omni--set-string-width (string width &optional truncate-pos add-pos)
  "Set the STRING width to a fixed value, WIDTH.

Set the string with depedning on the following conditions:
- If the STRING is longer than WIDTH, truncate the STRING and add
ellipsis, \"...\".
- If the STRING is shorter than WIDTH, add whitespace to the STRING.
- If TRUNCATE-POS is non-nil, truncate from position TRUNCATE-POS in the
STRING.
- If ADD-POS is non-nil, add whitespace to psition ADD-POS in the STRING."
  (let* ((string (format "%s" string))
         (w (length string)))
    (when (< w width)
      (if (and add-pos (< add-pos w))
          (setq string (format "%s%s%s" (substring string 0 add-pos) (consult-omni-propertize-by-plist (make-string (- width w) ?\s) (text-properties-at add-pos string)) (substring string add-pos)))
        (setq string (format "%s%s" (substring string) (make-string (- width w) ?\s)))))
    (when (> w width)
      (if (and truncate-pos (< truncate-pos (- width 3)) (>= truncate-pos 0))
          (setq string (format "%s%s%s" (substring string 0 truncate-pos) (propertize (substring string truncate-pos (+ truncate-pos 3)) 'display "...") (substring string (- 0 (- width truncate-pos 3)))))
        (setq string (format "%s%s%s"
                             (substring string 0 (- width 3))
                             (propertize  (substring string (- width 3) width) 'display "...")
                             (propertize (substring string width) 'invisible t)))))
    string))

(defun consult-omni--justify-left (string prefix maxwidth)
  "Set the width of STRING+PREFIX justified from left.

It uses `consult-omni--set-string-width' and sets the width
of the concatenate of STRING+PREFIX (e.g. `(concat PREFIX STRING)`)
within MAXWIDTH.

This can be used for aligning marginalia info in minibuffer."
  (let ((s (length string))
        (w (length prefix)))
    (if (< (+ s w) maxwidth)
        (consult-omni--set-string-width string (- maxwidth w) 0)
          string)))

(defun consult-omni--set-url-width (domain path width)
  "Set the length of DOMAIN+PATH to fit within WIDTH."
  (when (stringp domain)
    (let* ((result)
           (path-target-width (- width (length domain))))
      (cond
       ((<= path-target-width 0)
        (setq result (consult-omni--set-string-width domain width)))
       ((and (integerp path-target-width) (> path-target-width 10))
        (setq result (concat domain (consult-omni--set-string-width path path-target-width (floor (/ path-target-width 2))))))
       (t
        (setq result (consult-omni--set-string-width (concat domain path) width))))
      result)))

(defun consult-omni--highlight-match (regexp str ignore-case)
  "Highlight REGEXP in STR.

Case is ignored, if IGNORE-CASE is non-nil.
If a regular expression contains capturing groups, only these are
highlighted.  If no capturing groups are used, highlight the whole match.

\(This is adapted from `consult--highlight-regexps'.\)"
  (save-match-data
    (let ((i 0))
      (while (and (let ((case-fold-search ignore-case))
                    (string-match regexp str i))
                  (> (match-end 0) i))
        (let ((m (match-data)))
          (setq i (cadr m)
                m (or (cddr m) m))
          (while m
            (when (car m)
              (add-face-text-property (car m) (cadr m)
                                      'consult-omni-highlight-match-face nil str))
            (setq m (cddr m)))))))
  str)

(defun consult-omni--overlay-match (match-str buffer ignore-case)
  "Highlight MATCH-STR in BUFFER using an overlay.

Case is ignored when IGNORE-CASE is non-nil.
This is provided for convinience, if needed in formating candidates
or preview buffers."
  (let ((buffer (or (and buffer (get-buffer buffer)) (current-buffer))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (save-match-data
          (save-mark-and-excursion
            (remove-overlays (point-min) (point-max) 'consult-omni-overlay t)
            (goto-char (point-min))
            (let ((case-fold-search ignore-case))
              (while (search-forward match-str nil t)
                (when-let* ((m (match-data))
                            (beg (car m))
                            (end (cadr m))
                            (overlay (make-overlay beg end)))
                  (overlay-put overlay 'consult-omni-overlay t)
                  (overlay-put overlay 'face 'consult-omni-highlight-match-face))))))))))

(defun consult-omni-overlays-toggle (&optional buffer)
  "Toggle highlight overlays in BUFFER.

BUFFER defaults to the current buffer."
  (interactive)
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (dolist (o (overlays-in (point-min) (point-max)))
        (when (overlay-get o 'consult-omni-overlay)
          (if (and (overlay-get o 'face) (eq (overlay-get o 'face) 'consult-omni-highlight-match-face))
              (overlay-put o 'face nil)
            (overlay-put o 'face 'consult-omni-highlight-match-face)))))))

(defun consult-omni--numbers-human-readable (number &optional unit separator base prefixes)
  "Convert NUMBER to  a human-redable string.

SEPARATOR is a string placed between unmber and unit
UNIT is a string used as unit
BASE is the number base used to derive prefix
PREFIXES is a list of chars for each magnitude
\(e.g. \='(“” “K” “M” “G” ...\) for none, kilo, mega, giga, ...

adapted from `file-size-human-readable'."
  (let* ((power (if (and base (numberp base)) (float base) 1000.0))
	 (prefixes (or prefixes '("" "k" "M" "G" "T" "P" "E" "Z" "Y" "R" "Q")))
         (number (pcase number
                   ((pred numberp)
                    number)
                   ((pred stringp)
                    (string-to-number number))
                   (_ 0))))
    (while (and (>= number power) (cdr prefixes))
      (setq number (/ number power)
	    prefixes (cdr prefixes)))
    (let* ((prefix (car-safe prefixes)))
      (format (if (and (< number 10)
                       (>= (mod number 1.0) 0.05)
                       (< (mod number 1.0) 0.95))
                  "%.1f%s%s%s"
	        "%.0f%s%s%s")
	      number
              prefix
              (or separator " ")
              unit))))

(defun consult-omni--make-url-string (url params &optional ignore-keys)
  "Add key value pairs in PARAMS to URL as “&key=val”.

PARAMS should be an alist with keys and values to add to the URL.
key in IGNORE-KEYS list will be ignored."

  (let* ((url (if (equal (substring-no-properties url -1 nil) "?")
                  url
                (concat url "?")))
         (list (append (list url) (cl-loop for (key . value) in params
                                           collect
                                           (unless (member key ignore-keys)
                                             (format "&%s=%s" key value))))))
    (url-encode-url (mapconcat #'identity list))))

(defun consult-omni-hashtable-to-plist (hashtable &optional ignore-keys)
  "Convert a HASHTABLE to a plist.

Ommits keys in IGNORE-KEYS."
  (let ((pl nil))
    (maphash
     (lambda (k v)
       (unless (member k ignore-keys)
         (push (list k v) pl)))
     hashtable)
    (apply #'append pl)))

(defun consult-omni-expand-variable-function (var)
  "Call the function if VAR is a function."
  (if (functionp var)
      (funcall var)
    var))

(defun consult-omni--pulse-regexp (regexp &optional delay)
  "Find and pulses REGEXP for DELAY seconds.

DELAY defaults to `pulse-delay'."
  (goto-char (point-min))
  (while (re-search-forward regexp nil t)
    (when-let* ((m (match-data))
                (beg (car m))
                (end (cadr m))
                (ov (make-overlay beg end))
                (pulse-delay (or delay 0.075)))
      (pulse-momentary-highlight-overlay ov 'highlight))))

(defun consult-omni--pulse-region (beg end &optional delay)
  "Find and pulses region from BEG to END for DELAY seconds.

DELAY defaults to `pulse-delay'."
  (let ((ov (make-overlay beg end))
        (pulse-delay (or delay 0.075)))
    (pulse-momentary-highlight-overlay ov 'highlight)))

(defun consult-omni--pulse-line (&optional delay)
  "Pulse line at point momentarily for DELAY seconds.

DELAY defaults to `pulse-delay'."
  (let* ((pulse-delay (or delay 0.075))
         (ov (make-overlay (car (bounds-of-thing-at-point 'line)) (cdr (bounds-of-thing-at-point 'line)))))
    (pulse-momentary-highlight-overlay ov 'highlight)))

(defun consult-omni--url-log (string)
  "Insert STRING in the buffer `consult-omni-log-buffer-name'.

This is used for logging the response form `consult-omni-url-retrieve-sync'."
  (with-current-buffer (get-buffer-create consult-omni-log-buffer-name)
    (goto-char (point-min))
    (insert "**********************************************\n")
    (goto-char (point-min))
    (insert (format-time-string "%F - %T%n" (current-time)))
    (insert string)
    (insert "\n")
    (goto-char (point-min))
    (insert "\n\n**********************************************\n")))

(defun consult-omni--parse-http-response (&optional buffer)
  "Parse the first header line in BUFFER.

BUFFER defaults to the current buffer.
This would for example be “HTTP/1.1 200 OK” from an HTTP response."
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "\\=[ \t\n]*HTTP/\\(?1:[0-9\\.]+\\) +\\(?2:[0-9]+\\)" url-http-end-of-headers t)
        `(:http-version ,(match-string 1) :code ,(string-to-number (match-string 2)))))))

(defun consult-omni--url-response-body (response-data)
  "Extract the body from RESPONSE-DATA."
  (plist-get response-data :data))

(defun consult-omni--url-retrieve-error-handler (&rest _args)
  "Handle errors for consult-omni-url-retrieve functions."
  (message "consult-omni: url-retrieve got an error: %s" (consult-omni--parse-http-response)))

(cl-defun consult-omni-url-retrieve (url &rest settings &key (sync 'nil) (type "GET") params headers data parser callback error timeout &allow-other-keys)
  "Retrieve URL with SETTINGS.

Passes all the arguments to `url-retrieve', `url-retrieve-queue' or
`url-retrieve-snchronously'.

Description of Arguments:
  SYNC     when non-nil, retrieve URL sunchronously
           (see `url-retrieve-synchronously'.)
  TYPE     http request type (e.g. “GET”, “POST”)
  PARAMS   key values pairs added to the base url
           using `consult-omni--make-url-string'.
  HEADERS  key value pairs passed to headers
           (e.g `url-request-extra-headers').
  DATA     are http request data passed to data (e.g. `url-request-data').
  PARSER   a function that is executed in the `url-retrieve' response and
           the results are passed to CALLBACK.  It is called
           without any arguments in the response buffer
           \(i.e. (funcall PARSER) \) This is for example suitable for
           `json-read'.
  CALLBACK a function that is executed when the request is complete.
           It takes one argument, PARSED-DATA which is the output of the
           PARSER above \(i.e. (funcall CALLBACK (funcall PARSER))\).
  ERROR    a function that handles errors.  It is called without any
           arguments in the response buffer.
  TIMEOUT  is the time in seconds for timing out synchronous requests.
           This is ignored in async requests.

Note that  when `consult-omni-url-use-queue' is set to t, this function
uses `url-queue-retrieve', and sets `url-queue-parallel-processes' and
`url-queue-timeout' to `consult-omni-url-queue-parallel-processes' and
`consult-omni-url-queue-timeout', respectively."
  (let* ((url-request-method type)
         (url-request-extra-headers headers)
         (url-request-data data)
         (url-with-params (consult-omni--make-url-string url params))
         (url-debug (if consult-omni-log-level t nil))
         (url-queue-parallel-processes consult-omni-url-queue-parallel-processes)
         (url-queue-timeout consult-omni-url-queue-timeout)
         (retriever (if consult-omni-url-use-queue #'url-queue-retrieve #'url-retrieve))
         (response-data '(:status nil :data nil))
         (buffer (if sync
                     (if timeout
                         (with-timeout
                             (timeout
                              (setf response-data (plist-put response-data :status 'timeout))
                              nil)
                           (url-retrieve-synchronously url-with-params 'silent nil timeout))
                       (url-retrieve-synchronously url-with-params 'silent nil timeout))
                   (funcall retriever url-with-params
                            (lambda (status &rest _args)
                              (let* ((parsed-data (condition-case nil
                                                      (if parser (funcall parser) (buffer-substring (point-min) (point-max)))
                                                    (error (funcall error)))))
                                (setf response-data (plist-put response-data :status status))
                                (when parsed-data
                                  (setf response-data (plist-put response-data :data (funcall callback parsed-data)))))) nil 'silent))))
    (when (and buffer (buffer-live-p buffer))
      (add-to-list 'consult-omni--hidden-buffers-list buffer)
      (if sync
          (with-current-buffer buffer
            (save-excursion
              (goto-char (point-min))
              (let* ((end-of-headers (if (and (bound-and-true-p url-http-end-of-headers)
                                              (number-or-marker-p url-http-end-of-headers))
                                         url-http-end-of-headers
                                       (point-min)))
                     (response (buffer-substring (point-min) (pos-eol)))
                     (header (buffer-substring (point-min) end-of-headers))
                     (body (buffer-substring end-of-headers (point-max))))
                (when consult-omni-log-level
                  (cond
                   ((eq consult-omni-log-level 'info)
                    (consult-omni--url-log (format "URL: %s\nRESPONSE: %s" url response)))
                   ((eq consult-omni-log-level 'debug)
                    (consult-omni--url-log (format "URL: %s\n\nRESPONSE-HEADER:\n%s\n\nRESPONSE-BODY: %s\n" url header body)))))
                (setf response-data (plist-put response-data :status response))
                (delete-region (point-min) (+ end-of-headers 1))
                (goto-char (point-min))
                (if-let* ((parsed-data (condition-case nil
                                           (funcall parser)
                                         (error (funcall error)))))
                    (setf response-data (plist-put response-data :data (funcall callback parsed-data)))))))))
    response-data))

(cl-defun consult-omni--request-error-handler (&rest args &key symbol-status error-thrown &allow-other-keys)
  "Handle errors for request backend.

See `request' for more details on ARGS, SYMBOL-STATUS and ERROR-THROWN."
  (message "consult-omni: <request>  %s - %s" symbol-status error-thrown))

(cl-defun consult-omni--request-sync (url &rest args &key params headers data parser error encoding &allow-other-keys)
  "Convinient wrapper for `request'.

Fetch URL *synchronously* using `request'.
Refer to `request' documents for details on PARAMS, HEADERS, DATA,
PARSER, ERROR, and ENCODING."
  (unless (functionp 'request)
    (error "Request backend not available.  Either install the package “emacs-request” or change the custom variable `consult-omni-retrieve-backend'"))
  (let (candidates)
    (request
      url
      :sync t
      :params params
      :headers headers
      :parser parser
      :error (or error #'consult-omni--request-error-handler)
      :data data
      :encoding (or encoding 'utf-8)
      :success (cl-function (lambda (&key data &allow-other-keys)
                              (setq candidates data))))
    candidates))

(cl-defun consult-omni--plz-error-handler (plz-error &rest _args)
  "Handle errors for `plz' backend.

Refer to `plz' documentation for more details on PLZ-ERROR."
  (message "consult-omni: <plz> %s" plz-error))

(defun consult-omni--json-parse-buffer ()
  "Default json parser used in consult-omni."
  (let ((end-of-headers (if (and (bound-and-true-p url-http-end-of-headers)
                                 (number-or-marker-p url-http-end-of-headers))
                            url-http-end-of-headers
                          (point-min))))
    (goto-char end-of-headers)
    (json-parse-buffer :object-type 'hash-table :array-type 'list :false-object :false :null-object :null)))

(cl-defun consult-omni--fetch-url (url backend &rest args &key type params headers data parser callback error encoding timeout sync &allow-other-keys)
  "Retrieve URL with BACKEND.

This is a wrapper that passes the ARGS to the corresponding
BACKEND function.  \(i.e. `consult-omni-url-retrieve',
`request', `plz', ...\).  See backend functions for details.

Description of Arguments:
  SYNC     if non-nil, retrieve URL sunchronously.
  TYPE     http request type \(e.g. “GET”, “POST”\)
  PARAMS   key value pairs added to the base url using
           `consult-omni--make-url-string'.
  HEADERS  key value pairs passed to headers
           \(e.g. `url-request-extra-headers'\).
  DATA     http request data passed to data \(e.g. `url-request-data'\).
  PARSER   a function that is executed in the `url-retrieve' buffer,
           and the results are passed to CALLBACK.
           See `consult-omni-url-retrieve', `request', or `plz' for more
           info.
  CALLBACK a function that is executed when the request is complete.
           It takes one argument, PARSED-DATA \(e.g. the output of
           the PARSER above.\)
           It is called by (funcall CALLBACK (funcall PARSER)).  See
           `consult-omni-url-retrieve', `request', or `plz' for more info.
  ERROR    a function that handles errors.  It is called without any
           arguments in the response buffer.
  ENCODING is the encoding used for the request backend (e.g. \='utf-8)
  TIMEOUT  is the time in seconds for timing out synchronous requests.
           This is ignored in async requests."
  (cond
   ((eq backend 'plz)
    (when (and type (stringp type)) (setq type (intern (downcase type))))
    (if sync
        (funcall callback (funcall #'plz (or type 'get) (consult-omni--make-url-string url params)
                                   :headers headers
                                   :body data
                                   :as parser
                                   :then 'sync
                                   :else (or error #'consult-omni--plz-error-handler)
                                   :timeout (or timeout consult-omni-default-timeout)))
      (funcall #'plz (or type 'get) (consult-omni--make-url-string url params)
               :headers headers
               :body data
               :as parser
               :then callback
               :else (or error #'consult-omni--plz-error-handler)
               :timeout (or timeout consult-omni-default-timeout))))
   ((eq backend 'url)
    (if sync
        (consult-omni--url-response-body
         (funcall #'consult-omni-url-retrieve url
                  :sync sync
                  :type (or type "GET")
                  :params params
                  :headers headers
                  :parser parser
                  :data data
                  :error (or error #'consult-omni--url-retrieve-error-handler)
                  :callback (or callback #'identity)
                  :timeout (or timeout consult-omni-default-timeout)))
      (funcall #'consult-omni-url-retrieve url
               :sync sync
               :type (or type "GET")
               :params params
               :headers headers
               :parser parser
               :data data
               :error (or error #'consult-omni--url-retrieve-error-handler)
               :callback (or callback #'identity)
               :timeout (or timeout consult-omni-default-timeout))))
   ((eq backend 'request)
    (if sync
        (funcall callback
                 (request-response-data
                  (funcall #'request url
                           :type (or type "GET")
                           :sync sync
                           :params params
                           :headers headers
                           :parser parser
                           :data data
                           :error (or error #'consult-omni--request-error-handler)
                           :encoding (or encoding 'utf-8)
                           :timeout (or timeout consult-omni-default-timeout))))
      (funcall #'request url
               :type (or type "GET")
               :params params
               :headers headers
               :parser parser
               :data data
               :error (or error #'consult-omni--request-error-handler)
               :encoding (or encoding 'utf-8)
               :timeout (or timeout consult-omni-default-timeout)
               :complete (cl-function (lambda (&key data &allow-other-keys)
                                        (funcall (or callback #'identity) data))))))))

(defun consult-omni--kill-hidden-buffers ()
  "Kill all open preview buffers.

Kills the buffers stored in`consult-gh--preview-buffers-list'.
Ask for confirmation if the buffer is modified and remove the buffers that
are killed from the list."
  (interactive)
  (when consult-omni--hidden-buffers-list
    (mapc (lambda (buff) (if (and (buffer-live-p buff) (not (get-buffer-process buff)))
                             (kill-buffer buff))) consult-omni--hidden-buffers-list))
  (setq consult-omni--hidden-buffers-list nil))

(defun consult-omni--kill-url-dead-buffers ()
  "Kill buffers in `url-dead-buffer-list'."
  (interactive)
  (when url-dead-buffer-list
    (mapc (lambda (buff) (if  (and (buffer-live-p buff) (not (get-buffer-process buff)))
                             (kill-buffer buff)))
          url-dead-buffer-list))
  (setq url-dead-buffer-list nil))

(defun consult-omni--async-log (formatted &rest args)
  "Log FORMATTED ARGS to variable `consult-omni--async-log-buffer'."
  (with-current-buffer (get-buffer-create consult-omni--async-log-buffer)
    (goto-char (point-max))
    (insert (apply #'format formatted args))))

(defun consult-omni--get-source-prop (source prop)
  "Get PROP for SOURCE from `consult-omni--sources-alist'."
  (plist-get (cdr (assoc source consult-omni--sources-alist)) prop))

(defun consult-omni-dynamic--split-thingatpt (thing)
  "Return THING at point."
  (when-let* (str (thing-at-point thing t))
      (format "%s" str)))

(defun consult-omni--read-search-string (&optional initial)
  "Read a string from the minibuffer with INITIAL.

This is used to get initial input for static commands, when
`consult-omni-default-autosuggest-command' is nil."
  (consult--read nil
                 :prompt "Search: "
                 :initial initial
                 :category 'consult-omni
                 :history 'consult-omni--search-history
                 :add-history (consult-omni--add-history '(symbol))))

(defun consult-omni--get-split-style-character (&optional style)
"Get the character for consult async split STYLE.

STYLE defaults to `consult-async-split-style'."
(let ((style (or style consult-async-split-style 'none)))
  (or (char-to-string (plist-get (alist-get style consult-async-split-styles-alist) :initial))
      (char-to-string (plist-get (alist-get style consult-async-split-styles-alist) :separator))
      "")))

(cl-defun consult-omni--simple-format-candidate (&rest args &key source title &allow-other-keys)
  "Return a simple formatted string for candidates with ARGS.

Description of Arguments:
  SOURCE     the name string of the source for candidate
  TITLE      the title of the candidate"
  (let* ((frame-width-percent (floor (* (frame-width) 0.1)))
         (title-str (consult-omni--set-string-width title (* 5 frame-width-percent))))
    (concat title-str
            (when source (concat "\t" source)))))

(cl-defun consult-omni--highlight-format-candidate (&rest args &key source query url title snippet face &allow-other-keys)
  "Return a highlighted formatted string for candidates with ARGS.

Description of Arguments:
  SOURCE      the name string of the source for candidate
  QUERY       the query string used for searching
  URL         a string pointing to url of the candidate
  TITLE       the title of the candidate
  SNIPPET     a string containing a snippet/description of candidate
  FACE        the face used for the title"
  (let* ((frame-width-percent (floor (* (frame-width) 0.1)))
         (source (and (stringp source) (propertize source 'face 'consult-omni-source-type-face)))
         (match-str (and (stringp query) (not (equal query ".*")) (consult--split-escaped query)))
         (face (or (consult-omni--get-source-prop source :face) face 'consult-omni-default-face))
         (title-str (propertize title 'face face))
         (title-str (consult-omni--set-string-width title-str (* 4 frame-width-percent)))
         (snippet (and (stringp snippet) (consult-omni--set-string-width snippet (* 3 frame-width-percent))))
         (snippet (and (stringp snippet) (propertize snippet 'face 'consult-omni-snippet-face)))
         (urlobj (and url (url-generic-parse-url url)))
         (domain (and (url-p urlobj) (url-domain urlobj)))
         (domain (and (stringp domain) (propertize domain 'face 'consult-omni-domain-face)))
         (path (and (url-p urlobj) (url-filename urlobj)))
         (path (and (stringp path) (propertize path 'face 'consult-omni-path-face)))
         (url-str (consult-omni--set-url-width domain path (* frame-width-percent 2)))
         (str (concat title-str
                      (when url-str (concat "\s" url-str))
                      (when snippet (concat "\s\s" snippet))
                      (when source (concat "\t" source)))))
    (if consult-omni-highlight-matches-in-minibuffer
        (cond
         ((listp match-str)
          (mapc (lambda (match) (setq str (consult-omni--highlight-match match str t))) match-str))
         ((stringp match-str)
          (setq str (consult-omni--highlight-match match-str str t)))))
    str))

(defun consult-omni--group-function (sources cand transform &optional group-by)
  "Group CAND, in the minibuffer according to GROUP-BY category.

CAND is a candiate in a list of candidates from SOURCES, a list
of all sources for the candidates.  GROUP-BY is a keyword.  If TRANSFORM
is non-nil, the string of CAND is used as the group name.

This is passed as GROUP to `consult--read' on candidates
and is used to define the grouping for CAND."
  (if transform (substring cand)
    (let* ((group-by (or consult-omni--override-group-by group-by consult-omni-group-by))
           (group-by (if (not (keywordp group-by)) (intern (concat ":" (format "%s" group-by))) group-by)))
      (cond
       ((equal group-by :domain)
        (if-let* ((url (get-text-property 0 :url cand))
                  (urlobj (if url (url-generic-parse-url url) nil))
                  (domain (if (url-p urlobj) (url-domain urlobj))))
            domain
          nil))
       ((member group-by '(:nil :none :no :not))
        nil)
       (group-by
        (if-let* ((group (get-text-property 0 group-by cand)))
            (format "%s" group)
          "N/A"))
       (t
        (if-let* ((source (plist-get (consult--multi-source sources cand) :name)))
            source
          nil))))))

(defun consult-omni--add-history (&optional things &rest _args)
  "Make a list for added history based on THINGS at point.

THINGS should be a list of item types \(e.g. \='(url number word sexp)\).
Each of the THINGS at point willbe added as a separate item to
the history in consul-omni's minibuffer completion."
  (delq nil
        (cl-remove-duplicates
         (append
          (when (region-active-p) (list (concat (consult-omni--get-split-style-character) (buffer-substring (region-beginning) (region-end)))))
          (mapcar (lambda (thing) (consult-omni-dynamic--split-thingatpt thing))
                  (or things (list 'number 'word 'sexp 'symbol 'url 'filename 'sentence 'line)))
          (when (and isearch-string (not (string-empty-p isearch-string))) (list (concat (consult-omni--get-split-style-character) isearch-string)))))))

(defun consult-omni--lookup-function ()
  "Lookup function for `consult-omni' minibuffer candidates.

This is passed as LOOKUP to `consult--read' on candidates
and is used to format the output when a candidate is selected."
  (lambda (sel cands &rest _args)
    (let* ((info (or (car (member sel cands)) ""))
           (title (get-text-property 0 :title info))
           (url (get-text-property 0 :url info)))
      (consult-omni-propertize-by-plist (or title url "nil") (or (text-properties-at 0 info) (list))))))

(defun consult-omni--default-url-preview (cand)
  "Default function to use for previewing CAND."
  (when (listp cand) (setq cand (car-safe cand)))
  (when-let* ((url (get-text-property 0 :url cand))
              (buff (funcall consult-omni-default-preview-function url)))
    (funcall (consult--buffer-preview) 'preview buff)))

(cl-defun consult-omni--make-state-function (&rest args &key setup preview exit return &allow-other-keys)
  "Convinient wrapper to make custom state functions with ARGS.

This can be passed as STATE to `consult--read' on candidates and is
used to define actions when setting up, previewing or selecting a
candidate.  SETUP, PREVIEW, EXIT, and RETURN are functions that take one
input argument, the selected candidate.

Refer to `consult--read' documentation for more details."
  (lambda (action cand &rest _args)
    (if cand
        (pcase action
          ('setup
           (funcall setup cand))
          ('preview
           (funcall preview cand))
          ('exit
           (funcall exit cand))
          ('return
           (funcall return cand))))))

(defun consult-omni--dynamic-state-function ()
  "Dynamically make state function for the candidate at point.

This makes a STATE function to be passed to `consult--read' based on the
source of the candidate at point.  The \='setup, \='preview, \='return and
\='exit actions for a specific source is looked up in
`consult-omni--sources-alist'."
  (lambda (action cand &rest args)
    (if cand
        (let* ((source (get-text-property 0 :source cand))
               (state (consult-omni--get-source-prop source :state))
               (preview (consult-omni--get-source-prop source :on-preview))
               (return (consult-omni--get-source-prop source :on-return))
               (exit (consult-omni--get-source-prop source :on-exit)))
          (if state
              (funcall state action cand args)
            (pcase action
              ('preview
               (if preview (funcall preview cand) (consult-omni--default-url-preview cand)))
              ('return
               (if return (funcall return cand) cand))
              ('exit
               (when exit (funcall exit cand))
               (funcall (consult--buffer-preview) 'exit cand))))))))

(defun consult-omni--default-callback (cand)
  "Default CALLBACK for CAND.

The CALLBACK is called when a CAND is selected.

When making consult-omni sources, if a CALLBACK is not provided, this
CALLBACK is used as a fall back option."
  (when (listp cand) (setq cand (car-safe cand)))
  (if-let* ((url (get-text-property 0 :url cand)))
      (funcall consult-omni-default-browse-function url)))

(defun consult-omni-external-search (cand &optional engine)
  "Search for CAND on the search engine, ENGINE.

ENGINE is cons, where the car is the nae of the search engine, and the cdr
is the url string or a function.  See `consult-omni-search-engine-alist'
for some examples."
  (interactive (list (consult--read nil :prompt "Search: ")))
  (let* ((cand (or (and (stringp cand) (propertize cand :query (substring-no-properties cand) :title cand)) cand))
         (engine (or engine consult-omni-default-search-engine (consult--read consult-omni-search-engine-alist :prompt "Select Search Engine: ")))
         (func (cdr (assoc engine consult-omni-search-engine-alist)))
         (search-url (if (stringp func) func nil))
         (url (if (stringp search-url) (format search-url (url-hexify-string cand)) nil)))
    (cond
     (url (funcall consult-omni-default-browse-function url))
     ((functionp (cadr func)) (funcall (cadr func) cand)))))

(defun consult-omni-external-search-with-engine (engine &optional cand)
  "Run `consult-omni-external-search' on CAND with a specific ENGINE."
  (funcall #'consult-omni-external-search cand engine))

(defun consult-omni--choose-other-source-for-new (cand)
  "Choose a source to use for non-existing CAND."
  (interactive)
  (let* ((sources (cl-remove-duplicates (delq nil (mapcar (lambda (item)
                                                            (when-let* ((new (consult-omni--get-source-prop item :on-new))
                                                                       (name (consult-omni--get-source-prop item :name)))
                                                              (when (not (eq new #'consult-omni--default-new))
                                                                (cons name new))))
                                                          consult-omni-multi-sources))))
         (action (consult--read sources
                                :prompt "Create a new item on source: "
                                :lookup #'consult--lookup-cdr)))
    (if (functionp action)
        (funcall action cand)
      (error "Do not know how to make a new item for that source!"))))

(defun consult-omni--default-new (cand)
  "Call `consult-omni-default-new-function' with CAND as the argument."
  (funcall consult-omni-default-new-function cand))

(defun consult-omni--extract-opt-pair (opt opts ignore-opts)
  "Extract a pair of (OPT . value) from a list of OPTS.

value is the next element after OPT in OPTS.
IGNORE-OPTS is a list of opts to exclude.

This is useful for example to extract key value pairs
from command-line options in a list of strings"
  (unless (member opt ignore-opts)
    (save-match-data
      (let* ((key (cond
                   ((string-match "-\\{1,2\\}\\(?1:.*\\)$" opt)
                    (intern (concat ":" (match-string 1 opt))))
                   ((string-match ":\\(?1:.*\\)$" opt)
                    (intern (concat ":" (match-string 1 opt))))
                   (t nil)))
             (val (or (cadr (member opt opts)) "nil")))
        (when key
          (cons key val))))))

(defun consult-omni--split-command (input &rest args)
  "Append command argument and options list in INPUT string to ARGS.

INPUT is a string, for example the user's input from the minibuffer.
command line arguments and options \(e.g. count, page, ...\) in the INPUT
string are extracted and appended to the ARGS list.  If there is a
grouping option in INPUT \(e.g. “:group source”\), it is used to set
`consult-omni--override-group-by'."
  (pcase-let* ((`(,query . ,opts) (consult--command-split input))
               (args (if (member (flatten-list args) (list nil (list nil))) nil args)))
    (if (and opts (listp opts) (> (length opts) 0))
        (progn
          (setq opts (cl-substitute ":count" ":n" opts :test 'equal))
          (setq opts (cl-substitute ":count" "-n" opts :test 'equal))
          (setq opts (cl-substitute ":page" ":p" opts :test 'equal))
          (setq opts (cl-substitute ":page" "-p" opts :test 'equal))
          (setq opts (cl-substitute ":group" ":g" opts :test 'equal))
          (setq opts (cl-substitute ":group" "-g" opts :test 'equal))
          (if (member ":group" opts)
              (setq consult-omni--override-group-by (cadr (member ":group" opts)))
            (setq consult-omni--override-group-by nil))
          (cl-loop for opt in opts
                   do (pcase-let* ((`(,key . ,val) (consult-omni--extract-opt-pair opt opts (list ":group"))))
                        (when key
                          (setq args (append args (list key val)))))))
      (setq consult-omni--override-group-by nil))
    (list (or query input) args)))

(defun consult-omni--match-minibuffer-content-p (cand)
  "Check if CAND matches minibuffer content.

Use regexp to only keep candidates in the minbuffer that match the current
content of the minibuffer \(e.g. user input\).  This is useful when using
a sync source in an async/dynamic fashion to filter the candidates from
the sync source and ony keep the ones that match user's input."
  (let* ((win (active-minibuffer-window))
         (buffer (window-buffer win))
         (split-char (consult-omni--get-split-style-character)))
    (with-current-buffer buffer
      (if (minibuffer-window-active-p win)
          (string-match (concat ".*" (string-trim (car-safe (consult-omni--split-command (minibuffer-contents-no-properties))) split-char "\n") ".*") (substring-no-properties cand))))))

(defun consult-omni--async-builder (input command-args)
  "Build command line from INPUT.

  COMMAND-ARGS are commandline args (e.g. “grep”)"
  (pcase-let ((`(,arg . ,opts) (consult--command-split input)))
    (unless (string-blank-p arg)
      (cons (append (consult--build-args command-args)
                    (consult--split-escaped arg) opts)
            (cdr (consult--default-regexp-compiler input 'basic t))))))

(defun consult-omni--multi-static-sync-candidates (source idx input &rest args)
  "Synchronously collect candidates for INPUT from a “sync” SOURCE.

This returns a list of candidates with properties suitable
for use in a static (not dynamically updated) multi-source command.

Description of Arguments:
  INPUT  a string; the user's input
  SOURCE a plist with properties that define the source to search.
         for example see `consult-omni-sources-alist'.
  ARGS   list of ARGS to pass to the collecting function
         \(e.g. the function defined by :items field in the SOURCE plist\)
  IDX    is passed to `consult-omni--multi-propertize'."
  (let* ((name (plist-get source :name))
         (face (and (plist-member source :face) `(face ,(plist-get source :face))))
         (cat (plist-get source :category))
         (transform (consult-omni--get-source-prop name :transform))
         (fun (plist-get source :items))
         (items))
    (when (functionp fun)
      (cond
       ((and (integerp (cdr (func-arity fun))) (< (cdr (func-arity fun)) 1))
        (setq items (funcall fun)))
       (t
        (setq items (funcall fun input args)))))
    (when (and items transform)
      (setq items (funcall transform items input)))
    (and items (consult-omni--multi-propertize items cat idx face))))

(defun consult-omni--multi-static-dynamic-candidates (source idx input &rest args)
  "Synchronously collect candidates for INPUT from a “dynamic” SOURCE.

This returns a list of candidates with properties suitable
for use in a static (not dynamically updated) multi-source command

Description of Arguments:
  INPUT  a string; the user's input
  SOURCE a plist with properties that define the source to search.
         for example see `consult-omni-sources-alist'.
  ARGS   list of ARGS to pass to the collecting function
         \(e.g. the function defined by :items field in the SOURCE plist\)
  IDX    is passed to `consult-omni--multi-propertize'."
  (let* ((name (plist-get source :name))
         (face (and (plist-member source :face) `(face ,(plist-get source :face))))
         (cat (plist-get source :category))
         (transform (consult-omni--get-source-prop name :transform))
         (fun (plist-get source :items))
         (current))
    (when (functionp fun)
      (funcall fun input
               :callback (lambda (response-items)
                           (if response-items
                               (progn
                                 (when transform (setq response-items (funcall transform response-items input)))
                                 (setq current
                                       (and response-items (consult-omni--multi-propertize
                                                            response-items cat idx face))))
                             (setq current t)))
               args)
      (with-timeout
          (consult-omni-default-timeout
           current)
        (while (not current)
          (sit-for 0.05)))
      current)))

(defun consult-omni--multi-static-async-candidates (source idx input &rest _args)
  "Synchronously collect candidates for INPUT from an “async” SOURCE.

This returns a list of candidates with properties suitable
for use in a static (not dynamically updated) multi-source command

Description of Arguments:
  INPUT  a string; the user's input
  SOURCE a plist with properties that define the source to search.
         for example see `consult-omni-sources-alist'.
  IDX    is passed to `consult-omni--multi-propertize'."
  (let* ((name (plist-get source :name))
         (builder (plist-get source :items))
         (transform (consult-omni--get-source-prop name :transform))
         (filter (consult-omni--get-source-prop name :filter))
         (face (and (plist-member source :face) `(face ,(plist-get source :face))))
         (consult-omni--async-log-buffer (concat " *consult-omni-async-log--" name "*"))
         (cat (plist-get source :category))
         (query (car (consult-omni--split-command input)))
         (cmd (funcall builder input)))
    (unless (stringp (car cmd))
      (setq cmd (car cmd)))
    (when cmd
      (let* ((lines)
             (process-adaptive-read-buffering nil)
             (out (with-temp-buffer
                    (set-buffer-file-coding-system 'cp1047)
                    (list (apply 'call-process (car cmd) nil (current-buffer) nil (cdr cmd))
                          (replace-regexp-in-string "\r" "\n"
                                                    (buffer-string))))))
        (if (eq (car out) 0)
            (progn
              (setq lines (mapcar (lambda (line) (propertize line :source name :title line :query query)) (split-string (cadr out) "[\r\n]+" t)))
              (when (and lines filter (functionp filter)) (setq lines (funcall filter lines query)))
              (when (and lines transform (functionp transform)) (setq lines (funcall transform lines query))))
          (message "process %s returned error with code %s and message %s" name (car out) (cdr out)))
        (consult-omni--multi-propertize lines cat idx face)))))

(defun consult-omni--multi-candidates-static (sources &optional input &rest args)
  "Return candidates for INPUT and ARGS from SOURCES.

This is used for `consult-omni--multi-static'."
  (let* ((candidates)
         (idx 0))
    (seq-doseq (src sources)
      (let* ((name (and (plist-member src :name) (plist-get src :name)))
             (items (plist-get src :items))
             (narrow (plist-get src :narrow))
             (async-type (and name (consult-omni--get-source-prop name :type)))
             (narrow-type (or (car-safe narrow) narrow -1)))
        (when (or (eq consult--narrow narrow-type)
                  (not (or consult--narrow (plist-get src :hidden))))
          (condition-case err
              (progn
                (when (functionp items)
                  (cond
                   (; sync source, append candidates right away
                    (eq async-type 'sync)
                    (push (consult-omni--multi-static-sync-candidates src idx input args) candidates))
                   (; dynamic source, append candidates and wait for it to populate
                    (eq async-type 'dynamic)
                    (push (consult-omni--multi-static-dynamic-candidates src idx input args) candidates))
                   (; async source, append candidates from process
                    (eq async-type 'async)
                    (push (consult-omni--multi-static-async-candidates src idx input args) candidates))
                   (t
                    (message "source %s needs a :type keyword. See the documentation for `consult-omni-define-source'." name)))))
            (wrong-type-argument nil)
            (error
             (message (if consult-omni-log-level
                          (format "error in calling :items of %s source - %s" name (error-message-string err))
                        (format "error in calling :items of %s source" name)))
             nil))))
      (cl-incf idx))
    (apply #'append candidates)))

(defun consult-omni--multi-static (sources input args &rest options)
  "Read candidates from SOURCES with static interface.

This is similar to `consult--multi' but accepts async/dynamic sources
as well.  See `consult--multi' for more info.

Description of Arguments:
  SOURCES is list of sources to use
  INPUT   is the user's input string
  ARGS    are sent as additional args to each SOURCE's
          collection function.
  OPTIONS are similar to options in `consult--multi'."
  (let* ((sources (consult--multi-enabled-sources sources))
         (timeout (max consult-omni--min-timeout (min (* (length sources) consult-omni-default-timeout) consult-omni--max-timeout)))
         (candidates (with-timeout ((or timeout 30) nil)
                       (consult--slow-operation consult-omni--slow-warning-message (consult-omni--multi-candidates-static sources input args))))
         (selected (if (or (not candidates) (and (listp candidates) (= (length candidates) 0)))
                       (progn (message (concat (propertize "no results were found with the input " 'face 'consult-omni-prompt-face)  (propertize (format "%s" input) 'face 'warning)))
                              nil)
                     (apply #'consult--read
                            candidates
                            (append
                             options
                             (list
                              :sort        nil
                              :history     'consult-omni--selection-history
                              :category    'multi-category
                              :predicate   (apply-partially #'consult-omni--multi-predicate sources)
                              :annotate    (apply-partially #'consult-omni--multi-annotate sources)
                              :group       (apply-partially #'consult-omni--multi-group sources)
                              :lookup      (apply-partially #'consult-omni--multi-lookup sources)
                              :preview-key (consult--multi-preview-key sources)
                              :narrow      (consult--multi-narrow sources)
                              :state       (consult--multi-state sources)))))))
    (if (and (listp selected) (plist-member (cdr selected) :match))
        (when-let* (fun (plist-get (cdr selected) :new))
          (funcall fun (car selected))
          (plist-put (cdr selected) :match 'new))
      (when-let* (fun (plist-get (cdr selected) :action))
        (funcall fun (car selected)))
      (setq selected `(,(car selected) :match t ,@(cdr selected))))
    selected))

(defun consult-omni--multi-lookup (sources selected candidates _input narrow &rest _)
  "Lookup SELECTED in CANDIDATES given SOURCES, with potential NARROW.

Adopted from `consult--multi-lookup'."
  (if (or (string-blank-p selected)
          (not (consult--tofu-p (aref selected (1- (length selected))))))
      ;; Non-existing candidate without Tofu or default submitted (empty string)
      (let* ((src (cond
                   (narrow (seq-find (lambda (src)
                                       (let ((n (plist-get src :narrow)))
                                         (eq (or (car-safe n) n -1) narrow)))
                                     sources))
                   ((seq-find (lambda (src) (plist-get src :default)) sources))
                   ((seq-find (lambda (src) (not (plist-get src :hidden))) sources))
                   ((aref sources 0))))
             (idx (seq-position sources src))
             (def (and (string-blank-p selected) ;; default candidate
                       (seq-find (lambda (cand) (eq idx (consult--tofu-get cand))) candidates))))
        (if def
            (cons (cdr (get-text-property 0 'multi-category def)) src)
          `(,selected :match nil ,@src)))
    (let* ((found (member selected candidates))
           (info (if found (or (car found) "") ""))
           (title (get-text-property 0 :title info))
           (url (get-text-property 0 :url info)))
      (if found
          ;; Existing candidate submitted
          (cons (apply #'propertize (or title url "nil") (or (text-properties-at 0 info) (list)))
                (consult--multi-source sources selected))
        ;; Non-existing Tofu'ed candidate submitted, e.g., via Embark
        `(,(substring selected 0 -1) :match nil ,@(consult--multi-source sources selected))))))

(defun consult-omni--multi-group (sources cand &optional transform)
  "Return group string of candidate CAND.

Returns the group string for candidate or transforms it
for all the candidates given SOURCES.

Adopted from `consult--multi-group'."
  (if transform
      cand
    (let* ((fun (and (plist-member (consult--multi-source sources cand) :group)
                     (plist-get (consult--multi-source sources cand) :group))))
      (cond
       ((functionp fun)
        (let ((argnum (cdr (func-arity fun))))
          (cond
           ((or (stringp argnum) (and (numberp argnum) (> argnum 2)))
            (funcall fun sources cand transform))
           ((and (numberp argnum) (= argnum 2))
            (funcall fun cand transform))
           ((and (numberp argnum) (= argnum 1))
            (funcall fun cand)))))
       ((stringp fun)
        fun)
       ((eq fun 'nil)
        nil)
       (t
        (plist-get (consult--multi-source sources cand) :name))))))

(defun consult-omni--multi-predicate (sources cand)
  "Predicate function called for each candidate CAND given SOURCES.

Adopted from `consult--multi-predicate'."
  (consult--multi-visible-p (consult--multi-source sources cand)))

(defun consult-omni--multi-enabled-sources (sources)
  "Return vector of enabled SOURCES.

Adopted from `consult--multi-enabled-sources'."
  (vconcat
   (cl-loop
    for src in sources
    if (when (setq src (if (symbolp src) (symbol-value src) src))
         (if-let* ((pred (plist-get src :enabled)))
           (cond
            ((functionp pred)
             (funcall pred))
            (t
             pred))
            (funcall #'always)))
    collect src)))

(defun consult-omni--multi-propertize (response-items category pos &optional face)
  "Propertize RESPONSE-ITEMS with the multi-category datum and FACE.

POS and CATEGORY are the group ID and category for these items.

Adopted from `consult--multi-candidates'."
    (cl-loop for item in response-items
             collect
      (let* ((str (or (car-safe item) item))
             (len (length str))
             (cand (consult--tofu-append str pos)))
        ;; Preserve existing `multi-category' datum of the candidate.
        (unless (get-text-property 0 'multi-category cand)
          (put-text-property 0 len 'multi-category (cons category (or (cdr-safe item) item)) cand))
        (when face (add-text-properties 0 len face cand))
        cand)))

(defun consult-omni--multi-annotate (sources cand)
  "Annotate candidate CAND from multi SOURCES.

Adopted from `consult--multi-annotate'."
  (let ((src (consult--multi-source sources cand)))
    (if-let* ((fun (plist-get src :annotate)))
        (cond
         ((functionp fun)
          (funcall fun (cdr (get-text-property 0 'multi-category cand))))
         ((and (symbolp fun) (functionp (eval fun)))
          (funcall (eval fun) (cdr (get-text-property 0 'multi-category cand))))))))

(defun consult-omni--async-min-input (&optional min-input)
  "Async function enforcing a minimum input length.
This is similar
MIN-INPUT is the minimum input length and defaults to
`consult-omni-async-min-input'.

Adopted from `consult--async-min-input'."
  (setq min-input (or min-input consult-omni-async-min-input))
  (lambda (sink)
    (lambda (action)
      (if (stringp action)
          ;; Input can be marked with the `consult--force' property such that it
          ;; is passed through in any case.
          (funcall sink (if (or (and (not (equal action ""))
                                     (get-text-property 0 'consult--force action))
                                (>= (length action) min-input))
                       action 'cancel))
        (funcall sink action)))))

(defun consult-omni--multi-update-sync-candidates (async source idx input &rest args)
  "Asynchronously collect candidates for INPUT from a “sync” SOURCE.

This returns a list of candidates with properties suitable
for use in a dynamically updated multi-source command

Description of Arguments:
  ASYNC  a funciton; the sink function that updates the minibuffer
         candidates list
  SOURCE a plist with properties that define the source to search.
         for example see `consult-omni-sources-alist'.
  INPUT  a string; the user's input to pass to the collecting function
         \(e.g. the value of :items field in the SOURCE plist\)
  ARGS   list of ARGS to pass to the collecting function
         \(e.g. the value of :items field in the SOURCE plist\)
  IDX    is passed to `consult-omni--multi-propertize'."
  (let* ((name (plist-get source :name))
         (face (and (plist-member source :face) `(face ,(plist-get source :face))))
         (cat (plist-get source :category))
         (transform (consult-omni--get-source-prop name :transform))
         (min-input (or (consult-omni--get-source-prop name :min-input) consult-omni-async-min-input))
         (valid-input (or (consult-omni--get-source-prop name :valid-input) nil))
         (fun (plist-get source :items))
         (items))
    (when (and valid-input (functionp valid-input)) (setq input (funcall valid-input input)))
    (when (and (functionp fun) (stringp input) (>= (length input) min-input))
      (cond
       ((and (integerp (cdr (func-arity fun))) (< (cdr (func-arity fun)) 1))
        (setq items (funcall fun)))
       (t (setq items (funcall fun input args)))))
    (when (and items transform)
      (setq items (funcall transform items input)))
    (funcall async (and items (consult-omni--multi-propertize items cat idx face)))
    (funcall async 'refresh)))

(defun consult-omni--multi-update-dynamic-candidates (async source idx input &rest args)
  "Asynchronously collect candidates for INPUT from a “dynamic” SOURCE.

This returns a list of candidates with properties suitable
for use in a dynamically updated multi-source command

Description of Arguments:
  ASYNC  a funciton; the sink function that updates the minibuffer
         candidates list
  SOURCE a plist with properties that define the source to search.
         for example see `consult-omni-sources-alist'.
  INPUT  a string; the user's input to pass to the collecting function
         \(e.g. the value of :items field in the SOURCE plist\)
  ARGS   list of ARGS to pass to the collecting function
         \(e.g. the value of :items field in the SOURCE plist\)
  IDX    is passed to `consult-omni--multi-propertize'."
  (let* ((name (plist-get source :name))
         (face (and (plist-member source :face) `(face ,(plist-get source :face))))
         (cat (plist-get source :category))
         (transform (consult-omni--get-source-prop name :transform))
         (min-input (or (consult-omni--get-source-prop name :min-input) consult-omni-async-min-input))
         (valid-input (or (consult-omni--get-source-prop name :valid-input) nil)))
    (when (and (stringp input) (>= (length input) min-input))
      (when (and valid-input (functionp valid-input)) (setq input (funcall valid-input input)))
      (funcall (plist-get source :items) input
               :callback (lambda (response-items)
                 (when response-items
                   (when transform (setq response-items (funcall transform response-items input)))
                   (funcall async (consult-omni--multi-propertize response-items cat idx face))
                   (funcall async 'refresh)))
               args))))

(defun consult-omni--multi-update-async-candidates (async source idx input &rest args)
  "Asynchronously collect candidates for INPUT from a “async” SOURCE.

This returns a list of candidates with properties suitable
for use in a dynamically updated multi-source command

Description of Arguments:
  ASYNC  a funciton; the sink function that updates the minibuffer
         candidates list
  SOURCE a plist with properties that define the source to search.
         for example see `consult-omni-sources-alist'.
  INPUT  a string; the user's input to pass to the collecting function
         \(e.g. the value of :items field in the SOURCE plist\)
  ARGS   list of ARGS to pass to the collecting function
         \(e.g. the value of :items field in the SOURCE plist\)
  IDX    is passed to `consult-omni--multi-propertize'."
  (let* ((name (plist-get source :name))
         (builder (plist-get source :items))
         (transform (consult-omni--get-source-prop name :transform))
         (min-input (or (consult-omni--get-source-prop name :min-input) consult-omni-async-min-input))
         (valid-input (or (consult-omni--get-source-prop name :valid-input) nil))
         (filter (consult-omni--get-source-prop name :filter))
         (props (seq-drop-while (lambda (x) (not (keywordp x))) args))
         (proc)
         (proc-buf)
         (count)
         (face (and (plist-member source :face) `(face ,(plist-get source :face))))
         (consult-omni--async-log-buffer (concat " *consult-omni-async-log--" name "*"))
         (cat (plist-get source :category))
         (query (car (consult-omni--split-command input)))
         (_ (when (and valid-input (functionp valid-input))
              (setq input (funcall valid-input input))))
         (args (when (and (stringp input) (>= (length input) min-input))
                     (funcall builder input))))
    (unless (stringp (car args))
      (setq args (car args)))
    (when proc
      (delete-process proc)
      (kill-buffer proc-buf)
      (setq proc nil proc-buf nil))
    (when args
      (let* ((rest "")
             (proc-filter
              (lambda (_ out)
                (let* ((lines (split-string out "[\r\n]+")))
                  (if (not (cdr lines))
                      (setq rest (concat rest (car lines)))
                    (setcar lines (concat rest (car lines)))
                    (let* ((len (length lines))
                           (last (nthcdr (- len 2) lines)))
                      (setq rest (cadr last)
                            count (+ count len -1))
                      (setcdr last nil)
                      (when lines
                        (when (and filter (functionp filter)) (setq lines (funcall filter lines query)))
                        (when (and transform (functionp transform))
                          (setq lines (funcall transform lines query)))
                        (setq lines (mapcar (lambda (line) (propertize line :source name :title line :query query)) lines))
                        (funcall async (consult-omni--multi-propertize lines cat idx face))
                        (funcall async 'refresh)))))))
             (proc-sentinel
              (lambda (_ event)
                (funcall async `[indicator
                         ,(cond
                          ((string-prefix-p "killed" event)   'killed)
                          ((string-prefix-p "finished" event) 'finished)
                          (t 'failed))])
                (when (and (string-prefix-p "finished" event) (not (equal rest "")))
                  (cl-incf count)
                  (funcall async (list rest)))
                (consult-omni--async-log
                 "consult--async-process sentinel: event=%s lines=%d\n"
                 (string-trim event) count)
                (when (> (buffer-size proc-buf) 0)
                  (with-current-buffer (get-buffer-create consult-omni--async-log-buffer)
                    (goto-char (point-max))
                    (insert ">>>>> stderr >>>>>\n")
                    (let ((beg (point)))
                      (insert-buffer-substring proc-buf)
                      (save-excursion
                        (goto-char beg)
                        (message #("%s" 0 2 (face error))
                                 (buffer-substring-no-properties (pos-bol) (pos-eol)))))
                    (insert "<<<<< stderr <<<<<\n")))))
             (process-adaptive-read-buffering nil))
        (funcall async [indicator running])
        (consult-omni--async-log "consult--async-process started %S\n" args)
        (setq count 0
              proc-buf (generate-new-buffer (concat " *consult-omni-async-stderr-" name "*"))
              proc (apply #'make-process
                          `(,@props
                            :connection-type pipe
                            :name ,(car args)
                            :process-buffer ,proc-buf
                            :noquery t
                            :command ,args
                            :filter ,proc-filter
                            :sentinel ,proc-sentinel)))))
    (when proc (add-to-list 'consult-omni--async-processes `(,proc . ,proc-buf)))))

(defun consult-omni--multi-cancel ()
  "Kill asynchronous subprocesses created for async multi-source commands."
  (mapc (lambda (proc) (when proc (delete-process (car proc))
                               (kill-buffer (cdr proc))))
          consult-omni--async-processes)
  (setq consult-omni--async-processes nil)
  (mapc (lambda (timer) (when timer (cancel-timer timer))) consult-omni--dynamic-timers)
  (setq consult-omni--dynamic-timers nil))

(defun consult-omni--multi-update-candidates (async sources action &rest args)
  "Dynamically update CANDIDATES for multiple SOURCES.

Description of Arguments:
  ASYNC   a function; the sink function
  SOURCES a list; sources to use
  ACTION  a string or symbol; the action argument passed to ASYNC.
          See `consult--async-sink' for more info"
  (let ((idx 0))
    (seq-doseq (src sources)
      (let* ((name (plist-get src :name))
             (items (plist-get src :items))
             (narrow (plist-get src :narrow))
             (async-type (consult-omni--get-source-prop name :type))
             (narrow-type (or (car-safe narrow) narrow -1)))
        (when (or (eq consult--narrow narrow-type)
                  (not (or consult--narrow (plist-get src :hidden))))
          (condition-case err
              (progn
                (when (functionp items)
                  (cond
                   (; sync source, append candidates right away
                    (equal async-type 'sync)
                    (consult-omni--multi-update-sync-candidates async src idx action args))
                   (; async source, append candidatesin process
                    (equal async-type 'async)
                    (consult-omni--multi-update-async-candidates async src idx action args))
                   (; dynamic source, append candidates in a callback function
                    (equal async-type 'dynamic)
                    (consult-omni--multi-update-dynamic-candidates async src idx action args))
                   (t
                    (message "source %s needs a :type keyword. See the documentation for `consult-omni-define-source'." name)))))
            (error ;; message other erros
             (funcall async [indicator killed])
             (message (if consult-omni-log-level
                          (format "error in calling :items of %s source - %s" name (error-message-string err))
                        (format "error in calling :items of %s source" name)))
             nil))))
      (cl-incf idx))))

(defun consult-omni--multi-dynamic-collection (async sources &optional min-input &rest args)
  "Dynamically compute candidates from SOURCES.

This is a generalized replacement for `consult--async-process', and
`consult--dynamic-collection' that allows collecting candidates from
synchronous \(e.g. elisp funciton with no input args\), dynamic \(e.g. elip
function with input args\), or asynchronous \(e.g. shell process\) SOURCES.

Description of Arguments:
  ASYNC       a funciton; the sink function
  SOURCES     a list; sources to use
  MIN-INPUT   a number; minimum number of characters for fetching result
  ARGS        a list of args; extra arguments passed to each source
              ARGS is passed to `consult-omni--multi-update-candidates'"
  (let ((min-input (or min-input consult-omni-async-min-input))
        (consult-omni--async-processes (list))
        (consult-omni--dynamic-timers (list))
        (current))
      (lambda (action)
        (pcase action
          ((pred stringp)
           (cond
            ((or (length< action min-input) (equal action current))
               (funcall async [indicator finished]))
            (t
               (setq current action)
               (funcall async 'flush)
               (funcall async [indicator running])
               (consult-omni--multi-update-candidates async sources action args)
               (funcall async [indicator finished])
               (funcall async 'refresh))))
          ((or 'cancel 'destroy)
           (setq current nil)
           (consult-omni--multi-cancel)
           (funcall async action))
          (_ (funcall async action))))))

(defun consult-omni--multi-dynamic-command (sources &optional min-input &rest args)
  "Dynamically collect with input splitting on multiple SOURCES.

MIN-INPUT is the minimum number of characters before the synamic command
fetches results.

ARGS is passed to each source \(by passing it along with SOURCES to
`consult-omni--multi-dynamic-collection'\).

This is a generalized form of `consult--async-command'
and `consult--dynamic-compute' that allows synchronous, dynamic,
and asynchronous sources."
  (consult--async-pipeline
   (consult--async-min-input (or min-input consult-omni-async-min-input))
   (consult--async-throttle consult-omni-dynamic-input-throttle consult-omni-dynamic-input-debounce)
   (lambda (sink) (consult-omni--multi-dynamic-collection sink sources (or min-input consult-omni-async-min-input) args))))

(cl-defun consult-omni--multi-dynamic (sources &optional min-input args &rest options)
  "Select candidates with dynamic input from a list of SOURCES.

This is similar to `consult--multi' but with dynamic update of candidates
and accepts async (shell commands simlar to `consult--grep'),
or dynamic sources (elisp functions like `consult-line-multi') as well.

Description of Arguments:
  SOURCES     a list; sources to use
  ARGS        list of args; additional arguments sent to each SOURCE's
              collection function.
  MIN-INPUT   a number; minimum number of charatcers for fetching results
  OPTIONS   are similar to options in `consult--multi'."
  (let* ((sources (consult-omni--multi-enabled-sources sources))
         (selected
          (apply #'consult--read
                 (consult-omni--multi-dynamic-command sources min-input args)
                 (append
                  options
                  (list
                   :sort        nil
                   :history     '(:input consult-omni--search-history)
                   :initial     nil
                   :category    'multi-category
                   :predicate   (apply-partially #'consult-omni--multi-predicate sources)
                   :annotate    (apply-partially #'consult-omni--multi-annotate sources)
                   :group       (apply-partially #'consult-omni--multi-group sources)
                   :lookup      (apply-partially #'consult-omni--multi-lookup sources)
                   :preview-key (consult--multi-preview-key sources)
                   :narrow      (consult--multi-narrow sources)
                   :state       (consult--multi-state sources))))))
    (if (plist-member (cdr selected) :match)
        (when-let* (fun (plist-get (cdr selected) :new))
          (funcall fun (car selected))
          (plist-put (cdr selected) :match 'new))
      (when-let* (fun (plist-get (cdr selected) :action))
        (funcall fun (car selected)))
      (setq selected `(,(car selected) :match t ,@(cdr selected))))
    selected))

(defun consult-omni--source-name (source-name &optional suffix)
  "Return a symbol for SOURCE-NAME variable with optional SUFFIX.

The variable is consult-omni--source-%s (%s=source-name).
Adds SUFFIX to the name if provided."
  (intern (format "consult-omni--source-%s" (concat (replace-regexp-in-string " " "-" (downcase source-name)) (if suffix (downcase suffix) nil)))))

(defun consult-omni--source-generate-docstring (source-name)
  "Make a generic documentation string for SOURCE-NAME.

This is used in `consult-omni-define-source' macro to make generic
docstrings for variables."
  (format "consult-omni source for %s.\n \nThis variable was created by the macro `consult-omni-define-source'."
          (capitalize source-name)))

(defun consult-omni--func-name (source-name &optional prefix suffix)
  "Make a function symbol for SOURCE-NAME with optional PREFIX and SUFFIX.

This is used to make interactive command symbols.

Adds PREFIX and SUFFIX to the name if non-nil."
  (intern (concat "consult-omni-" (if prefix prefix)
                  (replace-regexp-in-string " " "-" (downcase source-name))
                  (if suffix suffix))))

(defun consult-omni--func-generate-docstring (source-name &optional dynamic)
  "Make a generic documentaion string for an interactive command.

DEscription of Arguments:

  SOURCE-NAME a string; name of the source \(e.g. “Google”\)
  DYNAMIC     a boolean; whether the funciton is dynamic or not.
              dynamic here means dynamic completion in minibuffer like
              consult-grep behavior.

This is used to make docstring for function made by
`consult-omni-define-source'."
  (concat (if dynamic "Dynamic interactive" "Interactive ")
          (format "command to search %s."
                  (capitalize source-name))
          "\n\n This function was created by the macro `consult-omni-define-source'."))

(defun consult-omni--make-source-list (source-name request annotate face narrow-char state preview-key category lookup group require-match sort enabled predicate select-hist add-hist)
  "Internal function to make a source for `consult-omni--multi'.

Do not use this function directly, use `consult-omni-define-source' macro
instead.  Refer to `consult-omni-define-source' for details on
SOURCE-NAME, REQUEST, ANNOTATE, FACE, NARROW-CHAR, STATE, PREVIEW-KEY,
CATEGORY, LOOKUP, GROUP, REQUIRE-MATCH, SORT, ENABLED, PREDICATE,
SELECT-HIST, and ADD-HIST."
  `(:name ,source-name
          ,(when (and annotate face) :face)
          ,(when (and annotate face)
             (cond
              ((eq face t)
               'consult-omni-default-face)
              (t face)))
          :narrow ,narrow-char
          :state ,(or state #'consult-omni--dynamic-state-function)
          :category ,(or category 'consult-omni)
          :history ,select-hist
          :add-history ,(or add-hist #'consult-omni--add-history)
          :items  ,request
          :annotate ,(cond
                      ((and annotate (functionp annotate))
                       annotate)
                      (t nil))
          :lookup ,(if (and lookup (functionp lookup))
                       lookup
                     #'consult-omni--lookup-function)
          :group ,(or group #'consult-omni--group-function)
          :preview-key ,(and consult-omni-show-preview (or preview-key consult-omni-preview-key))
          ,(when enabled ':enabled)
          ,(when enabled enabled)
          :sort ,sort
          ,(when predicate ':predicate)
          ,(when predicate predicate)
          :require-match ,require-match))

(defun consult-omni--call-static-command (input prompt no-callback args source-name require-match select-hist-var sort)
  "Internal function to make static `consult--read' command.

Do not use this function directly, use `consult-omni-define-source'
macro instead.

Refer to `consult-omni-define-source' for details on INPUT, PROMPT,
NO-CALLBACK, ARGS, SOURCE-NAME, REQUIRE-MATCH, SELECT-HIST-VAR, and SORT."
  (let* ((input (or input
                    (and consult-omni-default-autosuggest-command (funcall-interactively consult-omni-default-autosuggest-command))
                    (consult-omni--read-search-string)))
         (setup (consult-omni--get-source-prop source-name :on-setup))
         (exit (consult-omni--get-source-prop source-name :on-exit))
         (prompt (or prompt (concat "[" (propertize (format "%s" (consult-omni--func-name source-name)) 'face 'consult-omni-prompt-face) "]" " Search: ")))
         (_ (if (functionp setup) (funcall setup)))
         (selected (consult-omni--multi-static (list (consult-omni--source-name source-name))
                                               input
                                               args
                                               :prompt prompt
                                               :sort sort
                                               :history select-hist-var
                                               :require-match require-match))
         (_ (if (functionp exit) (funcall exit)))
         (match (plist-get (cdr selected) :match))
         (source  (plist-get (cdr selected) :name))
         (selected (cond
                    ((consp selected) (car-safe selected))
                    (t selected)))
         (selected (if match selected (and (stringp selected) (string-trim selected (consult-omni--get-split-style-character)))))
         (callback-func (and (not no-callback)
                             (or (and match source (consult-omni--get-source-prop source :on-callback))
                                 (and source (consult-omni--get-source-prop source :on-new))))))
    (unless consult-omni-log-level
      (consult-omni--kill-hidden-buffers)
      (consult-omni--kill-url-dead-buffers))
    (when selected
    (cond
     ((and match (functionp callback-func))
      (funcall callback-func selected))
     ((functionp callback-func)
      (setq selected (funcall callback-func selected))))
    selected)))

(defun consult-omni--call-dynamic-command (initial prompt no-callback args min-input source-name require-match select-hist-var add-hist sort)
  "Internal function to make dynamic `consult--read' command.

Do not use this function directly, use `consult-omni-define-source'
macro instead.

Refer to `consult-omni-define-source' for details on INITIAL, PROMPT,
NO-CALLBACK, ARGS, MIN-INPUT, SOURCE-NAME, REQUIRE-MATCH,
SELECT-HIST-VAR, ADD-HIST, and SORT."

  (let* ((consult-async-refresh-delay consult-omni-dynamic-refresh-delay)
         (consult-async-input-throttle consult-omni-dynamic-input-throttle)
         (consult-async-input-debounce consult-omni-dynamic-input-debounce)
         (setup (consult-omni--get-source-prop source-name :on-setup))
         (exit (consult-omni--get-source-prop source-name :on-exit))
         (min-input (or min-input consult-omni-async-min-input))
         (prompt (or prompt (concat "[" (propertize (format "%s" (consult-omni--func-name source-name)) 'face 'consult-omni-prompt-face) "]" " Search: ")))
         (_ (if (functionp setup) (funcall setup)))
         (selected (consult-omni--multi-dynamic (list (consult-omni--source-name source-name))
                                                min-input
                                                args
                                                :prompt prompt
                                                :history '(:input search-hist-var)
                                                :add-history (or add-hist (consult-omni--add-history))
                                                :initial initial
                                                :sort sort
                                                :require-match require-match))
         (_ (if (functionp exit) (funcall exit)))
         (match (plist-get (cdr selected) :match))
         (source  (plist-get (cdr selected) :name))
         (selected (cond
                    ((consp selected) (car selected))
                    (t selected)))
         (selected (if match selected (string-trim selected (consult-omni--get-split-style-character))))
         (title (get-text-property 0 :title selected))
         (callback-func (and (not no-callback)
                             (or (and match source (consult-omni--get-source-prop source :on-callback))
                                 (and source (consult-omni--get-source-prop source :on-new))))))
    (add-to-history select-hist-var title)
    (unless consult-omni-log-level
      (consult-omni--kill-hidden-buffers)
      (consult-omni--kill-url-dead-buffers))
    (cond
     ((and match (functionp callback-func))
      (funcall callback-func selected))
     ((functionp callback-func)
      (setq selected (funcall callback-func selected))))
    selected))

;;; Macros

;;;###autoload
(cl-defmacro consult-omni-define-source (source-name &rest args &key type request transform filter min-input valid-input on-setup on-preview on-return on-exit state on-callback on-new require-match interactive lookup group narrow-char category search-hist select-hist add-hist face annotate enabled sort predicate preview-key docstring  &allow-other-keys)
  "Macro to make a consult-omni-source for SOURCE-NAME with ARGS.

Generates the following:
  - a source plist
  - interactive commands \(static or dynamic\) for single source
  - adds a new row to to `consult-omni--sources-alist' with all the
    metadata as a property list.

Description of Arguments:

  Brief Description:

  ==========   ========  =================================================
  Keyword      Type      Explanation
  ==========   ========  =================================================
  TYPE         symbol    How to collect items for source?
                         \(one of \='sync, \='dynamic, or \='async\)
  REQUEST      function  Fetch results from source
  TRANSFORM    funciton  Function to transform/format candidates
  FILTER       funciton  Function to filter candidates
  MIN-INPUT    number    Minimum number of characters for fetching results
                         This is used in dynamic commands only
  VALID-INPUT  function  Predicate function for valid user's input string
                         This is used in dynamic commands only
  ON-SETUP     function  Setup action in `consult--read'
  ON-PREVIEW   function  Preview action in `consult--read'
  ON-RETURN    function  Return action in `consult--read'
  ON-EXIT      function  Exit action in `consult--read'
  STATE        function  STATE passed to `consult--read'
                         \(bypasses ON-PREVIEW and ON-RETURN\)
  ON-CALLBACK  function  Function called on selected candidate
  ON-NEW       function  Function called on non-existing candidate
  REUIRE-MATCH function  Can non-matching candidates be selected
  INTERACTIVE  symbol    make \='dynamic, \='static or \='both commands
  LOOKUP       function  Lookup function for `consult--read'
  GROUP        function  Passed as GROUP to `consult--read'
  NARROW-CHAR  char      Passed as NARROW to `consult-read'
  CATEGORY     symbol    Passed as CATEGORY to `consult--read'
  SEARCH-HIST  symbol    Passed as HISTORY to `consult--read'
  SELECT-HIST  symbol    Collects list of selected items
  ADD-HIST     list      List of items to add to the history.
  FACE         face      Passed as FACE to `consult--read-multi'
  ANNOTATE     function  Passed as ANNOTATE to `consult--read'
  ENABLED      function  Passed as ENABLED to `consult--read'
  SORT         boolean   Passed as SORT to `consult--read'
  PREDICATE    function  Passed as PREDICATE to `consult--read'
  PREVIEW-KEY  key       Passed as PREVIEW-KEY to `consult--read'
  DOCSTRING    string    DOCSTRING for the SOURCE-NAME variable
  ========================================================================

  Detailed Decription:

  TYPE can be \='sync, \='dynamic or \='async, depending on how the items
       for the source should be collected.
       - \='sync    sources get their candidates from a synchronous elisp
                    function \(i.e. a function that returns a list\).
       - \='dynamic sources use an elisp function that runs asynchronously
                    to produce list of candidates \(e.g. a web request that
                    runs in the background\)
       - \='async   sources run a shell process \(e.g. a command line\)
                    asynchronously and return the results \(lines from
                    stdout\) as list of candidates.
       Note that all three types can have dynamic completion
       \(meaning that the funciton takes an input argument and returns
       the result based on the input\), but the difference is whether the
       function uses synchronous or asynchronous collection and whether it
       is an elsip function or a shell subprocess.

  REQUEST is a function that returns the list of candidates.
          - In synchronous sources, REQEUEST can take 0 or 1 input
            argument, and returns a list of candidates.
          - In asynchronous sources, REQUEST takes at least 1 input
            argument, and returns a list of strings that are command line
            process arguments.
          - In dynamic sources, REQUEST takes at least 1 input argument and
            a keyword argument called CALLBACK.  The CALLBACK should be
            called with candidates as input in the body.
            Here is the recommended format:
            (cl-defun REQUEST (input
                               &rest args
                               &key callback
                               &allow-other-keys)
              BODY
              (when callback (funcall callback candidates))
              candidates)
           See `consult-omni--brave-fetch-results' and
           `consult-omni--grep-builder' for examples.  More examples
           can be found in the wiki pages of the repo or in
           Lisp files under “sources” directory.

  TRANSFORM is a function that takes a list of candidates (e.g. strings)
            and optionally the query string and returns a list of
            transformed/formatted strings.  It's called with
            (funcall tranform candidates query).
            This is especially useful for async sources
            where the process returns a list of candiate strings,
            in which case TRANSFORM is applied to all candiates using `mapcar'.
            See `consult-omni--grep-transform' for an example.

  FILTER is a function that takes a list of candidates (e.g. strings)
         and optionally the query string and returns a list of filtered
         strings.  It's called with `(funcall filter candidates query)`.
         This is especially useful for async sources
         where the process returns a list of candiate strings,
         in which case FILTER is applied to all candidates using `seq-filter'.
         See `consult-omni--locate-filter' for an example.

  MIN-INPUT is a the minimum required number of characters in user's input
            before the input is sent to the source to fetch results/items

  VALID-INPUT is a function that checks if the user's input is valid for
              the source.  It is called with one argument, the user's input
              string, and should return the input \(with possible
              transformations\) when the inputn is valid and \='nil when
              the input is not valid.

  ON-SETUP is a function called when setting up the minibuffer.
           This is used inside an state funciton by `consult--read.
           See and its `consult--read' and state functions for more info.

  ON-PREVIEW is used as a function to call on the candidate, when a
             preview is requested.  It takes one required argument, the
             candidate.  For an example, see
             `consult-omni-default-preview-function'.

  ON-RETURN is used as a function to call on the candidate, when the
            candidate is selected.  This is passed to consult built-in
            state function machinery.  Note that in consult-omni's commands
            the output of this function will be returned, and ON-CALLBACK
            is used to do further actions on this returned value.  This
            allows to separate the returned value from the commands and
            actions that run on the selected candidates.  Therefore, for
            most use cases, ON-RETURN can just be `#'identity' to get the
            candidate back as it is.  But, if some transformation is
            needed, ON-RETURN can be used to transform the selected
            candidate.

  ON-EXIT is a function called when exiting the minibuffer.
          This is used inside an state funciton by `consult--read.
          See `consult--read' and its state functions for more info.

  STATE is a function that takes no argument and returns a function for
        consult--read STATE argument.  For an example see
        `consult-omni--dynamic-state-function' that builds state function
        based on ON-PREVIEW and ON-RETURN.  If STATE is non-nil, instead of
        using ON-PREVIEW and ON-RETURN to make a state function, STATE will
        be directly used in consult--read.

  ON-CALLBACK is a function that is called with one required input
              argument, the selected candidate.
              For example, see `consult-omni--default-callback'
              that opens the url of the candidate in the default browser.
              Other examples can be found in the wiki pages of the repo or
              in Lisp files under “source” directory.

  ON-NEW is similiar to ON-CALLBACK but for new non-pre-existing
         candidates, in other words the minibuffer content itself.  This is
         useful for example in autosuggestion commands, to get the query
         itself rather than a suggestion.

  REQUIRE-MATCH is a boolean.  When non-nil non-matching candidates \(e.g.
                the minibuffer content itself\) can be selected as a
                candidate.

  INTERACTIVE can be a symbol of the list:
         - If \='dynamic, only \*dynamic\* interactive commands are created
           in this macro.
         - If \='static, only \*static\* interactive commands are created
           in this macro.
         - Otherwise, \*Both\* dynamic and static
           commands are created.

  LOOKUP is passed to `consult--read'.
  GROUP is passed to `consult--read'.
  ANNOTATE is passed to `consult--read'.
  NARROW-CHAR is passed to `consult--read'.
  CATEGORY is passed to `consult--read'.
  ENABLED is passed to `consult--read'.
  SORT is passed to `consult--read'.
  PREVIEW-KEY is passed to `consult--read'.
    See consult's Documentaion for more details.

  SEARCH-HIST is a history list varibale to keep records of search terms.
  SELECT-HIST is a history list varibale to keep records of selected
              candidates.
  ADD-HIST is a list of items to add to the history list.

  FACE is used to format the candidate.  This is useful for simple
       formating without making use of TRANSFORM or formating candidates
       inside the REQUEST function.

  DOCSTRING is used as docstring for the variable consult-omni--source-%s
            variable that this macro creates for SOURCE-NAME."
  (if (symbolp source-name) (setq source-name (eval source-name)))
  `(progn
     ;; make a variable called consult-omni--source-%s (%s=source-name)
     (defvar ,(consult-omni--source-name source-name) nil)
     (setq ,(consult-omni--source-name source-name) (consult-omni--make-source-list ,source-name ,request ,annotate ,face ,narrow-char ,state ,preview-key ,category ,lookup ,group ,require-match ,sort ,enabled ,predicate ,select-hist ,add-hist))
     ;; make a dynamic interactive command called consult-omni-%s (%s=source-name)
     (unless (eq ,interactive 'dynamic)
       (defun ,(consult-omni--func-name source-name) (&optional initial prompt no-callback &rest args)
         ,(or docstring (consult-omni--func-generate-docstring source-name t))
         (interactive "P")
         (consult-omni--call-dynamic-command initial prompt no-callback args ,min-input ,source-name ,require-match ,select-hist ,add-hist ,sort)))
     ;; make a static interactive command called consult-omni-%s-static (%s=source-name)
     (unless (eq ,interactive 'static)
       (defun ,(consult-omni--func-name source-name nil "-static") (&optional input prompt no-callback &rest args)
         ,(or docstring (consult-omni--func-generate-docstring source-name))
         (interactive "P")
         (consult-omni--call-static-command input prompt no-callback args ,source-name ,require-match ,select-hist ,sort)))
     ;; add source to consult-omni--sources-alist
     (add-to-list 'consult-omni--sources-alist (cons ,source-name
                                                     (list :name ,source-name
                                                           :type ,type
                                                           :require-match ,require-match
                                                           :source (consult-omni--source-name ,source-name)
                                                           :face ,face
                                                           :request-func ,request
                                                           :min-input ,min-input
                                                           :valid-input ,valid-input
                                                           :transform ,transform
                                                           :filter ,filter
                                                           :on-setup ,on-setup
                                                           :on-preview (or ,on-preview #'consult-omni--default-url-preview)
                                                           :on-return (or ,on-return #'identity)
                                                           :on-exit ,on-exit
                                                           :on-callback (or ,on-callback #'consult-omni--default-callback)
                                                           :on-new (or ,on-new #'consult-omni--default-new)
                                                           :state ,state
                                                           :group ,group
                                                           :annotate ,annotate
                                                           :narrow-char ,narrow-char
                                                           :preview-key ,preview-key
                                                           :category (or ',category 'consult-omni)
                                                           :search-hist ,search-hist
                                                           :select-hist ,select-hist
                                                           :add-hist ,add-hist
                                                           :intactive ,interactive
                                                           :static-command (and (functionp (consult-omni--func-name ,source-name nil "-static")) (consult-omni--func-name ,source-name nil "-static"))
                                                           :dynamic-command (and (functionp (consult-omni--func-name ,source-name)) (consult-omni--func-name ,source-name))
                                                           :enabled ,enabled
                                                           :sort ,sort
                                                           :predicate ,predicate)))
     ,source-name))

;;;###autoload
(cl-defmacro consult-omni--make-fetch-function (source &rest args &key docstring &allow-other-keys)
  "Make a function for fetching result based on SOURCE with ARGS.

Description of Arguments:
SOURCE       a source for consult \(e.g. a plist that is passed
             to consult--multi\).  See `consult-buffer-sources' for
             examples.
DOCSTRING    the docstring for the function that is returned."
  (let* ((source (if (plistp source) source (eval source)))
         (source-name (substring-no-properties (plist-get source :name))))
    `(progn
       ;; make a function that creates a consult--read source for consult-omni-multi
       (cl-defun ,(consult-omni--source-name source-name "-fetch-results") (input &rest args &key callback &allow-other-keys)
         ,(or docstring (format "Fetch results from %s for consult-omni.\n\n This funciton was created by `consult-omni--make-fetch-function'." source-name))
         (pcase-let* ((`(,query . ,opts) (consult-omni--split-command input args))
                      (opts (car-safe opts))
                      (fun  (plist-get ',source :items))
                      (results (cond
                                ((functionp fun) (funcall fun))
                                ((listp fun) fun)))
                      (source (substring-no-properties (plist-get ',source :name))))
           (delq nil (mapcar (lambda (item)
                               (if (consp item) (setq item (or (car-safe item) item)))
                               (when (string-match (concat ".*" query ".*") item)
                                 (propertize item
                                             :source source
                                             :title item
                                             :url nil
                                             :query query
                                             :search-url nil)))
                             results)))))))

(cl-defun consult-omni--make-source-from-consult-source (consult-source &rest args &key type request min-input valid-input transform on-setup on-preview on-return on-exit on-callback on-new group narrow-char category interactive search-hist select-hist face annotate enabled sort predicate preview-key require-match docstring &allow-other-keys)
  "Make a consult-omni source from a consult source plist, CONSULT-SOURCE.

all ARGS are passed to `consult-omni-define-source' macro.

See `consult-omni-define-source' for more details on TYPE, REQUEST,
MIN-INPUT, VALID-INPUT, TRANSFORM, ON-SETUP, ON-PREVIEW, ON-RETURN,
ON-EXIT, ON-CALLBACK, ON-NEW, GROUP, NARROW-CHAR, CATEGORY,
INTERACTIVE, SEARCH-HIST, SELECT-HIST, FACE, ANNOTATE, ENABLED, SORT,
PREDICATE, PREVIEW-KEY, REQUIRE-MATCH, DOCSTRING."
  (let* ((source (if (plistp consult-source) consult-source (and (boundp consult-source) (eval consult-source))))
         (source (if (plistp source) source (eval source)))
         (name (and (plistp source) (substring-no-properties (plist-get source :name))))
         (narrow-char (or narrow-char (and (plistp source) (plist-get source :narrow))))
         (narrow-char (if (listp narrow-char) (car narrow-char)))
         (face (or face (and (plistp source) (plist-get source :face))))
         (annotate (cond
                    ((eq annotate 'nil) nil)
                    ((eq annotate 't) (and (plistp source) (plist-get source :annotate)))
                    (t annotate)))
         (preview-key (or preview-key (and (plistp source) (plist-get source :preview-key)) consult-omni-preview-key))
         (predicate (or predicate (and (plistp source) (plist-get source :predicate))))
         (require-match (if (plist-member args :require-match)
                            require-match
                          (and (plistp source) (plist-get source :require-match))))
         (group (or group (and (plistp source) (plist-get source :group))))
         (sort (or sort (and (plistp source) (plist-get source :sort))))
         (enabled (or enabled (and (plistp source) (plist-get source :enabled))))
         (category (or category (and (plistp source) (plist-get source :category)) 'consult-omni)))
    (eval (macroexpand
           `(consult-omni-define-source ,name
                                        :docstring ,docstring
                                        :narrow-char ,narrow-char
                                        :face ',face
                                        :category ',category
                                        :type ',type
                                        :request (or ,request (consult-omni--make-fetch-function ,source))
                                        :min-input ,min-input
                                        :valid-input ,valid-input
                                        :transform ,transform
                                        :on-setup ',on-setup
                                        :on-preview ',on-preview
                                        :on-return ',on-return
                                        :on-exit ',on-exit
                                        :on-callback ',on-callback
                                        :on-new ',on-new
                                        :preview-key ,preview-key
                                        :search-hist ',search-hist
                                        :select-hist ',select-hist
                                        :enabled ',enabled
                                        :predicate ',predicate
                                        :group ',group
                                        :sort ',sort
                                        :interactive ',interactive
                                        :annotate ',annotate
                                        :require-match ',require-match)))))

;;; Interactive Commands

;;;###autoload
(defun consult-omni-multi (&optional initial prompt sources no-callback min-input &rest args)
  "Interactive “multi-source dynamic search”.

This is an interactive command that fetches results form all the sources
in either SOURCES or in `consult-omni-multi-sources' with dynamic
completion meaning that the search term can be dynamically updated by the
user and the results are fetched as the user types in the miinibuffer.

Description of Arguments:

  INITIAL     a string; the initial search prompt in the minibuffer.
  PROMPT      a string; an optional minibuffer prompt
  SOURCES     a list of strings or symbols;
              - strings can be the name of a source, a key from
                `consult-omni--sources-alist', which can be made with
                the convinient macro `consult-omni-define-source' or by
                using `consult-omni--make-source-from-consult-source'.
              - symbols can be other consult sources
                (see `consult-buffer-sources' for example.)
              If SOURCES is nil, `consult-omni-multi-sources' is used
              instead.
  MIN-INPUT   a number; minimum number of input characters before
              fetching results.
  NO-CALLBACK a boolean; If t, only the selected candidate is returned
              without any callback action.

Other Features:

Additional commandline arguments can be passed in the minibuffer
entry similar to `consult-grep' by typing `--` followed by arguments.
These additional arguments are passed to async sources similar to
`consult-grep' syntax.  In addition, other arguments can be passed
to all sources by using key, val pairs \(e.g. “:group domain”\)

For example the user can enter:
  “#consult-omni -- :g domain”
This will run a search on all the sources for
the term “consult-omni” and then groups the results by the “domain
of the URL” of the results.

Built-in arguments include:
  - :g, or :group for grouping (see `consult-omni-group-by' and
    `consult-omni--override-group-by' for more info.)
  - :n, or :count is passed as the value for COUNT to any source in
    `consult-omni-multi-sources'.
  - :p, or :page is passed as the value for PAGE to any source in
    `consult-omni-multi-sources'.

Custom arguments can be passed by using “:ARG value”.
For example, if the user types the following in the minibuffer:
  “#how to do web search in emacs? -- :model gpt-4”
The term “how to do web search in emacs?” is passed as the search
term and the “gpt-4” as a keyword argument for :model to every
source in `consult-omni-multi-sources'.  If any request function of
the sources takes a keyword argument for :model, “gpt-4” is
used then.

Once the results are fetched, narrowing down can be done by
using `consult-async-split-style' syntax \(e.g. “#” for “perl” style\)
after the serach term, similar to `consult-grep'.
For example:
  “#consult-omni#github.com”
uses “consult-omni” as the search term, and then narrows the choices to
results that have “github.com” in them.

For more examples, refer to the official documentation of the repo here:
URL `https://github.com/armindarvish/consult-omni'.

For more details on consult--async functionalities, you can also
see `consult-grep' and the official manual of consult, here:
URL `https://github.com/minad/consult'."
  (interactive "P")
  (let* ((consult-async-refresh-delay consult-omni-dynamic-refresh-delay)
         (consult-async-input-throttle consult-omni-dynamic-input-throttle)
         (consult-async-input-debounce consult-omni-dynamic-input-debounce)
         (sources (or sources consult-omni-multi-sources))
         (sources (remove nil (mapcar (lambda (source)
                                        (cond
                                         ((stringp source)
                                          (consult-omni--get-source-prop source :source))
                                         ((symbolp source)
                                          source)))
                                      sources)))
         (prompt (or prompt (concat "[" (propertize "consult-omni-multi" 'face 'consult-omni-prompt-face) "]" " Search:  ")))
         (selected
          (consult-omni--multi-dynamic
           sources
           min-input
           args
           :prompt prompt
           :sort t
           :require-match nil
           :history '(:input consult-omni--search-history)
           :add-history (consult-omni--add-history '(symbol))
           :initial initial))
         (match (plist-get (cdr selected) :match))
         (source  (plist-get (cdr selected) :name))
         (selected (cond
                    ((consp selected) (car-safe selected))
                    (t selected)))
         (selected (if match selected (string-trim selected (consult-omni--get-split-style-character))))
         (callback-func (and (not no-callback)
                             (or (and match source (consult-omni--get-source-prop source :on-callback))
                                 #'consult-omni--default-new))))
    (unless consult-omni-log-level
      (consult-omni--kill-hidden-buffers)
      (consult-omni--kill-url-dead-buffers))
    (cond
     ((and match (functionp callback-func))
      (funcall callback-func selected))
     ((functionp callback-func)
      (setq selected (funcall callback-func selected))))
    selected))

;;;###autoload
(defun consult-omni-multi-static (&optional input prompt sources no-callback &rest args)
  "Interactive “static” multi-source search.

This commands asks user for an input \(a.k.a. a search term\) and fetches
results from all the sources in either SOURCES or in
`consult-omni-multi-sources' and present the result candidates in
minibuffer completion for user to select.

Description of Arguments:

  INPUT       a string; the initial search term.  If non-nil the user
              is queried for one with either
              `consult-omni-default-autosuggest-command' or
              `consult-omni--read-search-string'
  PROMPT      a string; an optional minibuffer prompt
  SOURCES     a list of strings or symbols:
                - strings can be name of a source, a key from
                  `consult-omni--sources-alist', which can be made with the
                  convinient macro `consult-omni-define-source' or by using
                  `consult-omni--make-source-from-consult-source'.
                - symbols can be other consult sources
                  (see `consult-buffer-sources' for example.)
              If SOURCES is nil, `consult-omni-multi-sources' is used
              instead.
  NO-CALLBACK a boolean; If t, only the selected candidate is returned
              without any callback action."
  (interactive "P")
  (let* ((input (or input
                    (and consult-omni-default-autosuggest-command  (funcall consult-omni-default-autosuggest-command))
                    (consult-omni--read-search-string)))
         (input (if (stringp input) (substring-no-properties input)))
         (sources (or sources consult-omni-multi-sources))
         (sources (remove nil (mapcar (lambda (source)
                                        (cond
                                         ((stringp source)
                                          (consult-omni--get-source-prop source :source))
                                         ((symbolp source)
                                          source)))
                                      sources)))
         (prompt (or prompt (concat "[" (propertize "consult-omni-multi-static" 'face 'consult-omni-prompt-face) "]" " Search:  ")))
         (selected (consult-omni--multi-static sources
                                               input
                                               args
                                               :prompt prompt
                                               :require-match nil
                                               :history 'consult-omni--selection-history
                                               :sort t))
         (match (plist-get (cdr selected) :match))
         (source  (plist-get (cdr selected) :name))
         (selected (cond
                    ((consp selected) (car-safe selected))
                    (t selected)))
         (selected (if match selected (string-trim selected (consult-omni--get-split-style-character))))
         (callback-func (and (not no-callback)
                             (or (and match source (consult-omni--get-source-prop source :on-callback))
                                 #'consult-omni--default-new))))
    (unless consult-omni-log-level
      (consult-omni--kill-hidden-buffers)
      (consult-omni--kill-url-dead-buffers))
    (cond
     ((and selected match (functionp callback-func))
      (funcall callback-func selected))
     ((and selected (functionp callback-func))
      (setq selected (funcall callback-func selected))))
    selected))

;;;###autoload
(defun consult-omni (&rest args)
  "Convinient wrapper function for favorite interactive command.

Calls the function in `consult-omni-default-interactive-command' with
ARGS."
  (interactive)
  (apply consult-omni-default-interactive-command args))

;;; provide `consult-omni' module

(provide 'consult-omni)

;;; consult-omni.el ends here
