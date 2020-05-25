;;; belak-org.el -*- lexical-binding: t; -*-

(require 'belak-core)

;; In the past, it made sense to babel my config file using `org-mode'. However,
;; that method has a number of disadvantages: it's slower and harder to
;; optimize, it's very hard to install the latest version of org, you don't get
;; linting of files, and you end up tailoring your org-mode setup for your
;; config file more than everyday usage.
;;
;; May this serve as a warning for when I inevitably think about switching back
;; to README.org again.

;;
;;; Packages

(use-package org
  :straight org-plus-contrib
  :mode ("\\.org\\'" . org-mode)
  :hook (org-mode . auto-fill-mode)
  :general (("C-c a" #'org-agenda)
            ("C-c b" #'org-switchb)
            ("C-c c" #'org-capture)
            ("C-c l" #'org-store-link))
  :config
  (setq
   ;; Allow using shift-select like in other buffers.
   org-support-shift-select t

   ;; Make tab and indentation in code blocks behave like they would in that
   ;; language's major mode.
   org-src-tab-acts-natively t
   org-src-preserve-indentation t

   ;; Tweak our task keywords.
   ;;(org-todo-keywords
   ;; '((sequence "TODO" "IN-PROGRESS" "REVIEW" "|" "DONE")))

   ;; Make sure org isn't "helpful" in trying to add spaces at the
   ;; start of code blocks. This makes it much easier to work with
   ;; when dealing with babeled files.
   org-edit-src-content-indentation 0

   ;; Try to navigate to file links rather than open them.
   org-link-frame-setup '((file . find-file))

   ;; Don't allow closing todos that aren't really finished.
   org-enforce-todo-dependencies t
   org-enforce-todo-checkbox-dependencies t

   org-todo-keywords
   '((sequence "TODO(t)" "WIP(p)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)"))

   ;; Open source blocks in the same window.
   org-src-window-setup 'current-window

   ;; We don't want text inside headings to be indented. This requires us to use
   ;; a non-bundled `org-mode' .
   org-adapt-indentation 'headline-data

   ;; org-agenda settings
   org-log-done t
   org-log-done-with-time t
   org-log-refile t))

(provide 'belak-org)
;;; belak-org.el ends here
