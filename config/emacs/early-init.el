;;; early-init.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs HEAD (27+) introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens. Disabling UI elements and
;; adding some performance optimizations in here can result in an improvement in
;; startup time.

;;; Code:

;; Make the warnings buffer only appear on errors. We set this as early as
;; possible to try and catch everything.
(setq warning-minimum-level :error)

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;; Prevent the glimpse of un-styled Emacs by disabling these UI
;; elements early. We could use `menu-bar-mode', `scroll-bar-mode',
;; and `tool-bar-mode', but this way is slightly faster.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)

;; Resizing the Emacs frame can be a terribly expensive part of
;; changing the font. By inhibiting this, we easily halve startup
;; times with fonts that are larger than the system default.
(setq frame-inhibit-implied-resize t)
