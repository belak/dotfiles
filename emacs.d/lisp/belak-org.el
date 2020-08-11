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
;;; Functions

;; Initially from Sacha Chua's Emacs config
;; TODO: bind to something?
(defun belak-org-insert-heading-for-next-day ()
  "Insert a same-level heading for the following day."
  (interactive)
  (let ((new-date
         (seconds-to-time
          (+ 86400.0
             (float-time
              (org-read-date nil 'to-time (elt (org-heading-components) 4)))))))
    (org-insert-heading-after-current)
    (insert (format-time-string "%Y-%m-%d\n\n" new-date))))


;;
;;; Packages

(use-package! org
  :straight org-plus-contrib
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c a" . org-agenda)
         ("C-c b" . org-switchb)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link)
         ("C-c j" . org-clock-goto)
         (:map org-mode-map
               ("C-c C-r" . org-refile)
               ("C-c v"   . org-show-todo-tree)))
  :config
  (setq
   ;; Allow using shift-select like in other buffers.
   org-support-shift-select t

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

   ;; Don't allow closing todos that aren't really finished.
   org-enforce-todo-dependencies t
   org-enforce-todo-checkbox-dependencies t

   ;; Tweak our task keywords
   org-todo-keywords
   '((sequence "TODO(t)" "WIP(p)" "WAITING(w)" "SOMEDAY(.)" "|" "DONE(d)" "CANCELLED(c)")
     (sequence "TRY" "LEARN" "|" "COMPLETE(x)"))

   ;; Open source blocks in the same window.
   org-src-window-setup 'current-window

   ;; We don't want text inside headings to be indented. This requires us to use
   ;; a non-bundled `org-mode' .
   org-adapt-indentation 'headline-data

   ;; org-agenda settings
   org-log-done t
   org-log-done-with-time t
   org-log-refile t

   org-agenda-dim-blocked-tasks t

   ;; TODO: org-log-into-drawer, org-clock-into-drawer
   ;; TODO: look at Sacha's effort property
   ;; TODO: look at Sacha's agenda-contexts
   ;; TODO: look at Sacha's org-agenda-done and org-agenda-mark-done-and-add-followup
   ;; TODO: look at Sacha's weekly and monthly reviews
   ;; TODO: look at org-move-line-to-end-of-list
   ;; TODO: look at org-attach for board game files

   org-babel-load-languages '((emacs-lisp . t)
                              (calc . t)
                              (python . t)
                              (shell . t)
                              (ruby . t))

   ;; TODO: maybe replace calc with literate-calc and set up org-src-lang-modes
   ;; TODO: look at Sacha's org-sort-list-in-custom-order
   ;; TODO: look at some of the Emacs news feeds in Sacha's config

   org-use-effective-time t
   org-extend-today-until 3

   org-startup-folded nil
   )

  ;;(add-function :after after-focus-change-function 'org-save-all-org-buffers)
  )

(use-package! org-roam
  :after org
  :hook (after-init . org-roam-mode))

(provide 'belak-org)
;;; belak-org.el ends here
