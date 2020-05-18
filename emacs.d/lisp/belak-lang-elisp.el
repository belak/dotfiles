;;; belak-lang-elisp.el -*- lexical-binding: t; -*-

(require 'belak-core)
(require 'belak-dev)

;;
;;; Packages

(use-package macrostep
  :general
  (:keymaps '(emacs-lisp-mode-map)
            "C-c e" #'macrostep-expand))


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

(after! flycheck
  (setq flycheck-emacs-lisp-load-path 'inherit)

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
;;; belak-lang-elisp.el ends here.