;;; belak-lang-elisp.el -*- lexical-binding: t; -*-

(after! flycheck
  ;; Reduce verbosity of flycheck when we're in either the emacs or dotfiles
  ;; directories.
  (defun belak--flycheck-emacs-config-h ()
    (when (and (eq major-mode 'emacs-lisp-mode)
               (or (not buffer-file-name)
                   (cl-loop for dir in (list "~/.dotfiles/" "~/.emacs.d/")
                            if (file-in-directory-p
                                buffer-file-name
                                (expand-file-name dir))
                            return t)))

      (add-to-list (make-local-variable 'flycheck-disabled-checkers)
                   'emacs-lisp-checkdoc)))

  (add-hook 'flycheck-mode-hook #'belak--flycheck-emacs-config-h))

(provide 'belak-lang-elisp)
;;; belak-lang-elisp.el ends here
