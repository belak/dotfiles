;;; belak-helpers --- helper functions and related tools

;;; Commentary:

;;; Code:

;; Simple helpers for platform checking

(defun osx-p ()
  "Check if a system is running OSX."
  (eq system-type 'darwin))

(defun linux-p ()
  "Check if a system is running Linux."
  (eq system-type 'gnu/linux))

(defmacro diminish-major-mode (mode name)
  "Use a different `NAME' when displaying a `MODE' in the modeline.

This is a snippet originally from
https://github.com/sandhu/emacs.d/blob/master/lisp/teppoudo-diminish.el."
  `(add-hook (intern (concat (symbol-name ,mode) "-hook"))
             '(lambda () (setq mode-name ,name))))

;; init-frame-hooks is a simple package which handles hooks for after
;; the frame has been initialized and adds convenient hooks for after
;; console init and GUI init.

(require 'init-frame-hooks)

(provide 'belak-helpers)
;;; belak-helpers.el ends here