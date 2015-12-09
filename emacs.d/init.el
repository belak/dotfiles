;; init -- belak's emacs init.el
;;
;;; Commentary:
;; This file is only to bootstrap into README.org and set up some
;; basic timing.
;;
;;; Code:

;; Define the start time so we can measure how long loading took later.
(defconst emacs-start-time (current-time))

;; Set the gc-cons-threshold as early as possible so we have some extra memory
;; to work with. This increases it to 20M.
(setq gc-cons-threshold 20000000)

;; Load the org-mode file. This has everything aside from the timing.
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

;;; init.el ends here
