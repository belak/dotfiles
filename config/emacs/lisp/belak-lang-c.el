;;; belak-lang-c.el -*- lexical-binding: t; -*-

(require 'belak-lib)

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
