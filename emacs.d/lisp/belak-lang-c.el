;;; belak-lang-c.el --- C/C++ related dev packages and settings

;;; Commentary:

;;; Code:

;; irony-mode is a pretty solid dev environment for C/C++/ObjC, but we
;; also need to load up the additional company and flycheck modules.
(use-package irony
  :delight
  :commands
  belak--maybe-enable-irony-mode
  :hook
  (c-mode    . belak--maybe-enable-irony-mode)
  (c++-mode  . belak--maybe-enable-irony-mode)
  (objc-mode . belak--maybe-enable-irony-mode)
  :config
  (defun belak--maybe-enable-irony-mode ()
    ;; This works around an issue with modes derived from c-mode and
    ;; friends by ensuring the major mode is one directly supported by
    ;; irony-mode.  php-mode is one example of this.
    (when (member major-mode irony-supported-major-modes)
      (irony-mode 1))))

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

(provide 'belak-lang-c)

;;; belak-lang-c.el ends here
