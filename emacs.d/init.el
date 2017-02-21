;; init -- belak's emacs init.el
;;
;;; Commentary:
;; This file is only to bootstrap into README.org and set up some
;; basic timing.
;;
;;; Code:

;; Define the start time so we can measure how long loading took
;; later.
(defconst emacs-start-time (current-time))

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; Set the gc-cons-threshold as early as possible so we have some
;; extra memory to work with. This increases it to 20M.
(setq gc-cons-threshold 20000000)

;; Make it easier to debug startup errors.
(setq debug-on-error t
      debug-on-quit t)

(require 'org)
(org-babel-load-file
 (expand-file-name "README.org" user-emacs-directory))

;; Now that we're done loading everything, print how long it took.
(when window-system
  (let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
    (message "Loading %s...done (%.3fs)" load-file-name elapsed))

  (add-hook 'after-init-hook
            `(lambda ()
               (let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
                 (message "Loading %s...done (%.3fs) [after-init]"
                          ,load-file-name elapsed)))
            t))

;; Set these variables back to normal
(setq debug-on-error nil
      debug-on-quit nil)

;;; init.el ends here
