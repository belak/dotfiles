;;; belak-dev-c.el --- C/C++ related dev packages and settings

;;; Commentary:

;;; Code:

;; irony-mode is a pretty solid dev environment for C/C++/ObjC, but we
;; also need to load up the additional company and flycheck modules.

(use-package irony
  :config
  (defun my-irony-mode-on ()
    ;; avoid enabling irony-mode in modes that inherits c-mode, e.g: php-mode
    (when (member major-mode irony-supported-major-modes)
      (irony-mode 1)))
  (add-hook 'c++-mode-hook 'my-irony-mode-on)
  (add-hook 'c-mode-hook 'my-irony-mode-on)
  (add-hook 'objc-mode-hook 'my-irony-mode-on)

  ;; replace the `completion-at-point' and `complete-symbol' bindings
  ;; in irony-mode's buffers by irony-mode's function and run the
  ;; autosetup function
  (defun my-irony-mode-hook ()
    (subword-mode 1)
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async)
    (irony-cdb-autosetup-compile-options))

  (add-hook 'irony-mode-hook 'my-irony-mode-hook))

(use-package company-irony
  :after (irony company)
  :config
  (add-to-list 'company-backends 'company-irony))

(use-package flycheck-irony
  :after (irony flycheck-mode)
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

(defun c-c++-header ()
  "Set either `c-mode' or `c++-mode', whichever is appropriate for header.

This function decides whether .h file is C or C++ header, sets
C++ by default because there's more chance of there being a .h
without a .cc than a .h without a .c (ie. for C++ template files)

This comes from
http://stackoverflow.com/questions/3312114/how-to-tell-emacs-to-open-h-file-in-c-mode"
  (interactive)
  (let ((c-file (concat (substring (buffer-file-name) 0 -1) "c")))
    (if (file-exists-p c-file)
        (c-mode)
      (c++-mode))))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c-c++-header))

;; I much prefer the linux C style to the GNU style.

(setq c-default-style '((java-mode . "java")
                        (awk-mode  . "awk")
                        (other     . "linux")))

(provide 'belak-dev-c)

;;; belak-dev-c.el ends here
