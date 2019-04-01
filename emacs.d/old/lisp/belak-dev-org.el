;;; belak-dev-org.el --- org-mode related settings

;;; Commentary:

;;; Code:

;; Make sure org mode is set up in a manner that doesn't
;; suck. Meaning, make code blocks act more like their native
;; counterparts, enable fancy indenting and allow for shift select.
;;
;; If the extra require looks hacky, that's because it is.
;; org-indent-mode does not exist by the time diminish is called.

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :diminish org-indent-mode
  :init
  (require 'org-indent)
  :config
  (setq org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-log-done t
        org-log-done-with-time t
        org-log-refile t
        org-support-shift-select t)

  (add-hook 'org-mode-hook 'auto-fill-mode))

(provide 'belak-dev-org)

;;; belak-dev-org.el ends here
