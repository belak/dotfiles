;;; belak-packages --- package related settings

;;; Commentary:

;;; Code:

;; Load package.el and do our best to make sure we don't automatically
;; load everything we have installed.

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu"   . "https://elpa.gnu.org/packages/")
                         ("org"   . "https://orgmode.org/elpa/")))
(setq package-enable-at-startup nil)

;; Hack around the nonsense which will add a call to
;; (package-initialize) in init.el
(setq package--init-file-ensured t)

(package-initialize)

;; Define a few helpers to make it easier to automatically refresh packages.

(defvar belak--package-cache-last-updated (time-subtract (current-time) 3600))

(defun belak--ensure-recently-refreshed ()
  "Ensure the package list has been refreshed in the last hour."
  (if (time-less-p (time-add belak--package-cache-last-updated 3600) (current-time))
    (package-refresh-contents)))

(defun belak--package-ensure-installed (package)
  "Install a missing PACKAGE if it isn't already."
  (unless (package-installed-p package)
    (package-install package)))

;; Ensure we're running on a relatively up-to-date package cache and update
;; the last cached time after each call to package-refresh-contents.

(advice-add 'package-install
            :before
            (lambda (&rest args)
              (belak--ensure-recently-refreshed)))

(advice-add 'package-refresh-contents
            :after
            (lambda (&rest args)
              (setq belak--package-cache-last-updated (current-time))))

;; use-package is a wrapper around package loading which makes lots of
;; common operations easier. We install it right after loading
;; package.el so we can use it to install everything else.

(eval-when-compile
  (belak--package-ensure-installed 'use-package))
(belak--package-ensure-installed 'diminish)
(belak--package-ensure-installed 'general)

(eval-when-compile
  (defvar use-package-verbose t)
  (require 'use-package)
  (require 'diminish)
  (require 'general))


;; Attempt to install packages unless we specify otherwise.

(setq use-package-always-ensure t)

(provide 'belak-packages)

;;; belak-packages.el ends here
