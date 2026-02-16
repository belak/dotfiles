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

(defvar belak-org-context-filter
  (if IS-WORK 'work 'home)
  "Current org context filter: 'work, 'home, or nil.
When nil, no filter is applied and all directories are included in agenda.")


;;
;;; Functions

(defun belak/org-update-agenda-files ()
  "Update org-agenda-files based on current context filter."
  (setq org-agenda-files
        (pcase belak-org-context-filter
          ('work  (list (expand-file-name "work" belak-org-directory)))
          ('home  (list (expand-file-name "home" belak-org-directory)))
          (_      (list belak-org-directory)))))

(defun belak/org-toggle-context ()
  "Toggle between work, home, and nil contexts.
Updates agenda files and refreshes agenda if open."
  (interactive)
  (setq belak-org-context-filter
        (pcase belak-org-context-filter
          ('work 'home)
          ('home nil)
          (_     'work)))
  (belak/org-update-agenda-files)
  (when (derived-mode-p 'org-agenda-mode)
    (org-agenda-redo))
  (message "Org context: %s" (or belak-org-context-filter 'all)))

(defun belak/org-context-dir ()
  "Return the context directory based on current context filter.
When context is nil, uses IS-WORK to determine directory."
  (pcase belak-org-context-filter
    ('work "work")
    ('home "home")
    (_     (if IS-WORK "work" "home"))))

(defun belak/org-context-file (subpath)
  "Return path to file at SUBPATH within current context directory."
  (expand-file-name
   (format "%s/%s" (belak/org-context-dir) subpath)
   belak-org-directory))

(defun belak/org-current-weekly-file ()
  "Return path to current week's org file based on context filter."
  (belak/org-context-file
   (format-time-string "weekly/%Y-W%V.org")))

(defun belak/org-current-meetings-file ()
  "Return path to meetings file based on context filter."
  (belak/org-context-file "meetings.org"))

(defun belak/org-current-inbox-file ()
  "Return path to inbox file based on context filter."
  (belak/org-context-file "inbox.org"))


;;
;;; Packages

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c a" . org-agenda)
         ("C-c b" . org-switchb)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link)
         ("C-c j" . org-clock-goto)
         ("C-c t" . belak/org-toggle-context)
         (:map org-mode-map
               ("C-c C-r" . org-refile)
               ("C-c v"   . org-show-todo-tree)))
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

   ;; Don't allow closing todos that aren't really finished.
   org-enforce-todo-dependencies t
   org-enforce-todo-checkbox-dependencies t

   ;; Tweak our task keywords
   org-todo-keywords
   '((sequence "TODO(t)" "WIP(p)" "|" "DONE(d)" "CANCELLED(c)"))

   ;; Open source blocks in the same window.
   org-src-window-setup 'current-window

   ;; We don't want text inside headings to be indented. This requires us to use
   ;; a non-bundled `org-mode' .
   org-adapt-indentation 'headline-data

   ;; org-agenda settings
   org-log-done t
   org-log-done-with-time t
   org-log-refile t

   ;; Some special magical bindings to make keys act smarter
   org-special-ctrl-a/e t
   org-special-ctrl-k t

   org-agenda-dim-blocked-tasks t

   org-babel-load-languages '((emacs-lisp . t)
                              (calc . t)
                              (python . t)
                              (shell . t)
                              (ruby . t))

   org-use-effective-time t
   org-extend-today-until 5

   ;; Show headlines but not content by default.
   org-startup-folded 'content

   ;; Capture templates
   org-capture-templates
   '(("t" "Task" entry
      (file belak/org-current-weekly-file)
      "* TODO %?\n"
      :empty-lines 1)

     ("m" "Meeting" entry
      (file+headline belak/org-current-meetings-file "Meetings")
      "* %<%Y-%m-%d %H:%M> - %?\n** Attendees\n- \n\n** Notes\n\n** Action Items\n- [ ] \n"
      :empty-lines 1)

     ("n" "Note" entry
      (file belak/org-current-inbox-file)
      "* %?\n%U\n"
      :empty-lines 1)

     ("j" "Journal" entry
      (file+olp+datetree (lambda () (expand-file-name "home/journal.org" belak-org-directory)))
      "* %<%H:%M>\n%?\n"
      :empty-lines 1)))

  ;; Initialize agenda files based on context
  (belak/org-update-agenda-files))

(use-package org-ql
  :demand t
  :after org
  :config
  ;; Explicitly require org-ql-search to ensure org-ql-block is available
  (require 'org-ql-search)

  (setq org-agenda-custom-commands
        '(("n" "Next Actions"
           ((org-ql-block '(and (todo "TODO")
                                (not (scheduled))
                                (not (deadline)))
                          ((org-ql-block-header "Next Actions")
                           (org-agenda-sorting-strategy '(priority-down))))))

          ("r" "Weekly Review"
           ((org-ql-block '(todo "TODO" "WIP")
                          ((org-ql-block-header "Open Tasks")))
            (org-ql-block '(and (todo "DONE")
                                (closed :from -7))
                          ((org-ql-block-header "Completed Last 7 Days")))))

          ("t" "All Tasks"
           ((org-ql-block '(todo "TODO" "WIP")
                          ((org-ql-block-header "All Tasks")
                           (org-agenda-sorting-strategy '(priority-down category-keep)))))))))

(provide 'belak-org)
;;; belak-org.el ends here
