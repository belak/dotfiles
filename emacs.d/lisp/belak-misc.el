;;; belak-misc --- random leftover setup -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward
      uniquify-strip-common-suffix nil)

;; Make M-z zap-up-to-char (doesn't include char) rather than
;; zap-to-char and make M-Z zap in reverse.
(autoload 'zap-up-to-char "misc" "" 'interactive)
(global-set-key [remap zap-to-char] 'zap-up-to-char)

(defun reverse-zap-up-to-char (char)
  "Zap back to CHAR."
  (interactive "Zap back to char: ")
  (zap-up-to-char -1 char))
(global-set-key "\M-Z" 'reverse-zap-up-to-char)

;; Double-spaces after the end of a sentence is a convention I don't
;; find useful.
(setq sentence-end-double-space nil)

(provide 'belak-misc)

;;; belak-misc.el ends here
