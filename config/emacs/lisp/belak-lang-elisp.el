;;; belak-lang-elisp.el -*- lexical-binding: t; -*-

(require 'belak-lib)
(require 'belak-dev)

;;
;;; Packages

(use-package macrostep
  :bind
  (:map emacs-lisp-mode-map
        ("C-c e" . macrostep-expand)
        ("M-."    . find-function-at-point)))

(use-package package-lint
  :commands package-lint)


;;
;;; Functions

(defun belak--eval-region-or-buffer ()
  (interactive)
  (if (region-active-p)
      (eval-region (region-beginning) (region-end))
    (eval-buffer)))


;;
;;; Tweaks

(bind-key "C-c :" 'belak--eval-region-or-buffer)

(provide 'belak-lang-elisp)
;;; belak-lang-elisp.el ends here.
