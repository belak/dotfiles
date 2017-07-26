;; init -- belak's emacs init.el
;;
;;; Commentary:
;; This file is only to bootstrap into README.org and set up some
;; basic timing.
;;
;;; Code:

;;; Basic Setup:
;;
;; This sets up some initial settings which are only used in this
;; file.  Specifically debugging and some profiling things.

;; Define the start time so we can measure how long loading took
;; later.

;; package.el adds this automatically, but we handle this in README.org
;(package-initialize)

(defconst emacs-start-time (current-time))

;; Set the gc-cons-threshold for init so we have some extra memory to
;; work with. This should be 25M. We also enable debug-on-error and
;; debug-on-quit to make it easier to debug startup errors.
(let ((gc-cons-threshold (* 25 1024 1024))
      (debug-on-error t)
      (debug-on-quit t))

  ;; The rest of the config is in README.org, so we load org-mode and
  ;; bootstrap into README.org.
  (require 'org)
  (org-babel-load-file
   (expand-file-name "README.org" user-emacs-directory))

  (garbage-collect))

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

;;; init.el ends here
