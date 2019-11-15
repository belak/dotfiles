;;; belak-lang-org --- org-mode related settings

;;; Commentary:

;; In the past, it made sense to babel my config file using
;; `org-mode'.  However, that method has a number of disadvantages:
;; it's slower and harder to optimize, it's very hard to install the
;; latest version of org, you don't get linting of files, and you end
;; up tailoring your org-mode setup for your config file more than
;; everyday usage.
;;
;; May this serve as a warning for when I want to switch back to
;; README.org again.

;;; Code:

(use-package org
  :straight org-plus-contrib
  :mode ("\\.org\\'" . org-mode)
  :hook (org-mode-hook . auto-fill-mode)
  :general
  ("C-c a" 'org-agenda)
  ("C-c b" 'org-switchb)
  ("C-c c" 'org-capture)
  :config
  (setq
   ;; Allow using shift-select like in other buffers.
   org-support-shift-select t

   ;; Make tab in code blocks behave like they would in that language's
   ;; major mode.
   org-src-tab-acts-natively t

   ;; Make sure org isn't "helpful" in trying to add spaces at the
   ;; start of code blocks. This makes it much easier to work with
   ;; when dealing with babeled files.
   org-edit-src-content-indentation 0

   ;; org-agenda settings
   org-log-done t
   org-log-done-with-time t
   org-log-refile t))

(provide 'belak-lang-org)

;;; belak-lang-org.el ends here
