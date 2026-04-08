;;; belak-org.el -*- lexical-binding: t; -*-

(require 'belak-lib)

;; In the past, it made sense to babel my config file using `org-mode'. However,
;; that method has a number of disadvantages: it's slower and harder to
;; optimize, it's very hard to install the latest version of org, you don't get
;; linting of files, and you end up tailoring your org-mode setup for your
;; config file more than everyday usage.
;;
;; May this serve as a warning for when I inevitably think about switching back
;; to README.org again.

;;
;;; Variables

(defvar belak-org-directory
  (expand-file-name "~/Documents/org")
  "Base directory for all org files.")


;;
;;; Packages

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c b" . org-switchb)
         ("C-c l" . org-store-link))
  :config
  (setq
   ;; Base directory
   org-directory belak-org-directory

   ;; Allow using shift-select like in other buffers.
   org-support-shift-select t

   ;; If you try to insert a heading in the middle of an entry, don't split it
   ;; in half, but instead insert the new heading after the end of the current
   ;; entry.
   org-insert-heading-respect-content t

   ;; Make tab and indentation in code blocks behave like they would in that
   ;; language's major mode.
   org-src-tab-acts-natively t
   org-src-preserve-indentation t

   ;; Make sure org isn't "helpful" in trying to add spaces at the start of code
   ;; blocks. This makes it much easier to work with when dealing with babeled
   ;; files.
   org-edit-src-content-indentation 0

   ;; Try to navigate to file links rather than open them.
   org-link-frame-setup '((file . find-file))

   ;; Open source blocks in the same window.
   org-src-window-setup 'current-window

   ;; We don't want text inside headings to be indented. This requires us to use
   ;; a non-bundled `org-mode' .
   org-adapt-indentation 'headline-data

   ;; Some special magical bindings to make keys act smarter
   org-special-ctrl-a/e t
   org-special-ctrl-k t

   org-babel-load-languages '((emacs-lisp . t)
                              (calc . t)
                              (python . t)
                              (shell . t)
                              (ruby . t))

   ;; Show headlines but not content by default.
   org-startup-folded 'content))

(provide 'belak-org)
;;; belak-org.el ends here
