(use-package org
  :straight org-plus-contrib
  :mode ("\\.org\\'" . org-mode)
  :hook (org-mode-hook . auto-fill-mode)
  :general
  ("C-c a" 'org-agenda)
  ("C-c b" 'org-switchb)
  ("C-c c" 'org-capture)
  :config
  (setq org-log-done t
        org-log-done-with-time t
        org-support-shift-select t
	org-src-tab-acts-natively t
	org-edit-src-content-indentation 0)

  ;; TODO: Drop Dropbox
  (setq org-agenda-files '("~/Dropbox/org/")))

(use-package org-journal
  :after org
  :general
  ("C-c C-j" 'org-journal-new-entry)
  ("C-c C-s" 'org-journal-search)
  :config
  ;; TODO: Drop Dropbox
  (setq org-journal-dir "~/Dropbox/journal"
        org-journal-file-format "%Y-%m-%d.org"
        org-journal-date-format "%d %b %Y (%A)"
        org-journal-time-format "%I:%M %p"
        org-extend-today-until 4))
