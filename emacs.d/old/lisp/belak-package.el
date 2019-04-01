;;; belak-package --- package manager related settings

;;; Commentary:

;;; Code:

;; Load package.el and do our best to make sure we don't automatically
;; load everything we have installed.

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu"   . "https://elpa.gnu.org/packages/")))
(setq package-enable-at-startup nil)
(package-initialize)

;; Firstly, we disable as many warnings as we can while installing
;; packages. Secondly, increasing the minimum prime bits size to
;; something larger than the default settings stops all the GnuTLS
;; warnings from showing up. This might not be the right place, but it
;; needs to happen before we install packages.

(setq byte-compile-warnings nil
      gnutls-min-prime-bits 4096)

;; This block simply defines a few helpers. belak/ensure-refreshed
;; will run a refresh if the package list hasn't been refreshed this
;; session. belak/package-ensure-installed will ensure the package
;; list has been refreshed and try to install the package if it hasn't
;; been installed already.

(defvar belak/refreshed-package-list nil
  "This will be t if the package list has been refreshed.")

(defun belak/ensure-refreshed ()
  "Ensure the package list has been refreshed this startup."
  (unless belak/refreshed-package-list
    (package-refresh-contents)
    (setq belak/refreshed-package-list t)))

(defun belak/package-ensure-installed (package)
  "Install a missing PACKAGE if it isn't already."
  (unless (package-installed-p package)
    (package-install package)))

;; Now that we have some helpers defined, we wrap package-install to
;; make sure that the first install of each session will refresh the
;; package list.

(advice-add 'package-install
            :before
            (lambda (&rest args)
              (belak/ensure-refreshed)))

;; use-package is a wrapper around package loading which makes lots of
;; common operations easier. We install it right after loading
;; package.el so we can use it to install everything else.

(belak/package-ensure-installed 'use-package)
(belak/package-ensure-installed 'diminish)

(eval-when-compile
  (defvar use-package-verbose t)
  (require 'use-package))

;; Attempt to install packages unless we specify otherwise.

(setq use-package-always-ensure t)

;; We also load general here, as this integrates with use-package for
;; better key bind settings.

(belak/package-ensure-installed 'general)
(require 'general)

(provide 'belak-package)

;;; belak-package.el ends here
