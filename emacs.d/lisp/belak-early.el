;;; belak-early --- setup that needs to happen reasonably early -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package no-littering
  :init
  (setq no-littering-etc-directory (expand-file-name "etc" belak-local-dir)
        no-littering-var-directory (expand-file-name "var" belak-local-dir)))

(provide 'belak-early)

;;; belak-early.el ends here
