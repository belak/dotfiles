;;; belak-lang-c.el -*- lexical-binding: t; -*-

(require 'belak-lib)

;;
;;; Packages

;; `irony-mode' is a pretty solid dev environment for C/C++/ObjC, but we also
;; need to load up the additional `company' and `flycheck' modules.
(use-package! irony
  :commands
  belak--maybe-enable-irony-mode
  :hook
  (c-mode    . belak--maybe-enable-irony-mode)
  (c++-mode  . belak--maybe-enable-irony-mode)
  (objc-mode . belak--maybe-enable-irony-mode)
  :config
  ;; TODO: look into c-default-style, maybe for java-mode as well.

  (defun belak--maybe-enable-irony-mode ()
    ;; This works around an issue with modes derived from c-mode and friends by
    ;; ensuring the major mode is one directly supported by irony-mode. php-mode
    ;; is one example of this.
    (when (member major-mode irony-supported-major-modes)
      (irony-mode 1))))

;; Add completion support
(use-package! company-irony
  :demand t
  :after (irony company)
  :config
  (set-company-backend! irony-mode-hook company-irony))

;; Add linting support
(use-package! flycheck-irony
  :demand t
  :after (irony flycheck)
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))


;;
;;; Functions

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


;;
;;; Tweaks

;; I much prefer the linux C style to the GNU style.
(setq c-default-style '((java-mode . "java")
                        (awk-mode  . "awk")
                        (other     . "linux")))

(provide 'belak-lang-c)
;;; belak-lang-c.el ends here
