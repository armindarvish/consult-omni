;;; consult-omni-sources.el --- Sources for Consulting Web Search Engines -*- lexical-binding: t -*-

;; Copyright (C) 2024 Armin Darvish

;; Author: Armin Darvish
;; Maintainer: Armin Darvish
;; Created: 2024
;; Version: 0.1
;; Homepage: https://github.com/armindarvish/consult-omni
;; Keywords: convenience

;;; Commentary:

;;; Code:

(require 'consult-omni)

(setq consult-omni-sources--all-modules-list
      (list 'consult-omni-apps
            'consult-omni-bing
            'consult-omni-brave-autosuggest
            'consult-omni-brave
            'consult-omni-browser-history
            'consult-omni-buffer
            'consult-omni-chatgpt
            'consult-omni-consult-notes
            'consult-omni-doi
            'consult-omni-duckduckgo
            'consult-omni-elfeed
            'consult-omni-find
            'consult-omni-gh
            'consult-omni-google
            'consult-omni-google-autosuggest
            'consult-omni-grep
            'consult-omni-ripgrep
            'consult-omni-gptel
            'consult-omni-invidious
            'consult-omni-line-multi
            'consult-omni-locate
            'consult-omni-mdfind
            'consult-omni-mu4e
            'consult-omni-notes
            'consult-omni-notmuch
            'consult-omni-pubmed
            'consult-omni-scopus
            'consult-omni-stackoverflow
            'consult-omni-wikipedia
            'consult-omni-youtube))

(defun consult-omni-sources--load-module (symbol)
"Loads feature SYMBOL"
(require symbol nil t))

(defun consult-omni-sources-load-modules (&optional list)
  "Loads the LIST of symbols.
If list is nil, loads `consult-omni-sources-modules-to-load'and if that is nil as well, loads `consult-omni-sources--all-modules-list'."
  (mapcar #'consult-omni-sources--load-module (or list consult-omni-sources-modules-to-load consult-omni-sources--all-modules-list)))

(consult-omni-sources-load-modules)

;;; provide `consult-omni-sources' module

(provide 'consult-omni-sources)
;;; consult-omni-sources.el ends here
