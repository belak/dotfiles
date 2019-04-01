;;; belak-org.el --- org-mode related config

;;; Commentary:

;;; Code:

(use-package org
  :pin org
  :ensure org-plus-contrib
  :mode ("\\.org\\'" . org-mode)
  :general
  ("C-c a" 'org-agenda)
  ("C-c b" 'org-switchb)
  ("C-c c" 'org-capture)
  :config
  (setq org-log-done t
        org-log-done-with-time t
        org-support-shift-select t)

  (setq org-agenda-files '("~/Dropbox/org/"))

  (add-hook 'org-mode-hook 'auto-fill-mode))

(use-package org-journal
  :after org
  :general
  ("C-c C-j" 'org-journal-new-entry)
  ("C-c C-s" 'org-journal-search)
  :config
  (setq org-journal-dir "~/Dropbox/journal"
        org-journal-file-format "%Y-%m-%d.org"
        org-journal-date-format "%d %b %Y (%A)"
        org-journal-time-format "%I:%M %p"
        org-extend-today-until 4))

(provide 'belak-org)

;;; belak-org.el ends here
