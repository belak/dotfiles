;;; init-frame-hooks -- convenience hooks for frame init
;;
;;; Commentary:
;; This has been adapted from Steve Purcell's dotfiles.
;; https://github.com/purcell/emacs.d/blob/master/lisp/init-frame-hooks.el
;;
;;; Code:

(defvar after-make-console-frame-hooks '()
  "Hooks to run after creating a new TTY frame.")

(defvar after-make-window-system-frame-hooks '()
  "Hooks to run after creating a new graphical frame.")

(defun run-after-make-frame-hooks (frame)
  "Run configured hooks in response to the newly-created FRAME.
Selectively runs either `after-make-console-frame-hooks' or
`after-make-window-system-frame-hooks'"
  (with-selected-frame frame
    (run-hooks (if (display-graphic-p)
                   'after-make-window-system-frame-hooks
                 'after-make-console-frame-hooks))))

(add-hook 'after-make-frame-functions 'run-after-make-frame-hooks)

(defconst belak/initial-frame (selected-frame)
  "The frame (if any) active during Emacs initialization.")

(add-hook 'after-init-hook
          (lambda ()
	    (when belak/initial-frame
	      (run-after-make-frame-hooks belak/initial-frame))))

(provide 'init-frame-hooks)
;;; init-frame-hooks.el ends here
