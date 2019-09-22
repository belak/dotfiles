;;; belak-ido --- ido related settings

;;; Commentary:

;;; Code:

(use-package ido
  :straight nil
  :config
  (setq ido-save-directory-list-file (expand-file-name "ido.last" belak-local-dir)
        ido-use-filename-at-point nil
        resize-mini-windows t
        ido-use-virtual-buffers t
        ido-auto-merge-work-directories-length -1

        completion-ignored-extensions
        '(".o" ".elc" "~" ".bin" ".bak" ".obj" ".map" ".a" ".ln" ".mod" ".gz"
          ".aux" ".tdo" ".fmt" ".swp" ".pdfsync" ".pdf" ".vrb" ".idx" ".ind"
          ".bbl" ".toc" ".blg" ".snm" ".ilg" ".log" ".out" ".pyc" ".DS_Store"
          "-blx.bib" ".run.xml" ".hi" ".fls" ".fdb_latexmk" ".bcf" ".rel"))

  (ido-mode 1)
  (ido-everywhere 1))

;; smex is a better replacement for M-x built around ido.

(use-package smex
  :after ido
  :general
  ("M-x" 'smex)
  ("M-X" 'smex-major-mode-commands)
  :config
  (setq smex-history-length 50
        smex-save-file (expand-file-name "smex-items" belak-local-dir)))

;; Use ido everywhere possible.

(use-package ido-completing-read+
  :after ido
  :config
  (ido-ubiquitous-mode 1))

;; ido is much more readable when all the options are displayed
;; vertically.

(use-package ido-vertical-mode
  :after ido
  :config
  (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right
        ido-vertical-show-count t)
  (ido-vertical-mode 1))

;; flx-ido changes the matching algorithm to improve the flex matching
;; support.

(use-package flx-ido
  :after ido
  :config
  (setq ido-enable-flex-matching t
        flx-ido-threshold 10000))

(provide 'belak-ido)

;;; belak-ido.el ends here
