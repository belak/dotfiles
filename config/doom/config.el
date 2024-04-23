;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;
;;; Constants

;; Additional useful defines not included in Doom proper. Even though doom has
;; moved away from `IS-MAC' and `IS-LINUX', this is still the clearest way to
;; express it.
(defconst IS-GUI (display-graphic-p))


;;
;;; Settings

(setq user-full-name    "Kaleb Elwert"
      user-mail-address "belak@coded.io")

;; The default style for uniquifying buffer names is strange, so we change it to
;; something more useful.
;;
;; NOTE: this is disabled (and we use the buffer-name-relative package instead)
;; until doom-emacs removes the dependency on persp-mode for workspaces. See
;; https://github.com/doomemacs/doomemacs/pull/7727#issuecomment-1992785677 for
;; some background information.
;;
;;(setq uniquify-buffer-name-style  'forward
;;      uniquify-strip-common-suffix nil)

(use-package! buffer-name-relative
  :hook (doom-first-buffer . buffer-name-relative-mode))

;; Don't make distinctions between ASCII and siblings (like a and a with an
;; umlaut) when searching.
(setq search-default-mode 'char-fold-to-regexp)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

(after! org
  (setq org-support-shift-select t           ; Re-enable shift-select
        org-insert-heading-respect-content t ; Insert new headings, even when inside one
        org-adapt-indentation 'headline-data ; We don't want text inside headings to be indented
        org-special-ctrl-k t                 ; Make C-k behave specially in headlines

        ;; org-agenda settings
        org-log-done t                   ; TODO: ensure this works
        org-log-done-with-time t         ; Include time as well as date when closing tasks
        org-log-refile t))                 ; TODO: ensure this works

(after! org-agenda
  (setq org-agenda-dim-blocked-tasks t)) ; Make tasks in the blocked state dim

(after! nix-mode
  (setq nix-nixfmt-bin "alejandra"))


;;
;;; UI

(setq doom-theme 'modus-vivendi)

(cond
 ((featurep :system 'macos)
  (setq doom-font                (font-spec :family "Monaco")
        doom-variable-pitch-font (font-spec :family "Monaco")))
 ((featurep :system 'linux)
  (setq doom-font                (font-spec :family "Terminus" :size 12.0)
        doom-variable-pitch-font (font-spec :family "Terminus" :size 12.0))))

;; Italics look pretty rough in most terminals (at least with the fonts I use),
;; so we make sure they're only enabled if we have a full GUI.
(when IS-GUI
  (setq modus-themes-italic-constructs t))

;; Disable icons in the modeline - this makes it a little smaller and causes it
;; to use normal characters instead.
(setq doom-modeline-icon nil)

;; Use relative line numbers rather than absolute.
(setq display-line-numbers-type 'relative)


;;
;;; Utility Functions

(defun belak--unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (barf-if-buffer-read-only)
  (let ((fill-column (point-max)))
    (fill-paragraph nil region)))

(defun belak--eval-region-or-buffer ()
  (interactive)
  (if (region-active-p)
      (eval-region (region-beginning) (region-end))
    (eval-buffer)))

;; Load zap-up-to-char and define reverse-zap-up-to-char to be used in keybinds
;; below.
(autoload 'zap-up-to-char "misc" "" 'interactive)

(defun reverse-zap-up-to-char (char)
  "Zap back to CHAR."
  (interactive "Zap back to char: ")
  (zap-up-to-char -1 char))


;;
;;; Keybinds

;; Fix up a number of keybinds which have strange behaviors.
(map!
 ;; I've never needed the font panel in Emacs, and even if I did I wouldn't want
 ;; it bound to this.
 "s-t" nil

 ;; It's far more useful than you'd expect to be able to easily evaluate a
 ;; region or buffer of code, especially when developing Emacs pacakges.
 "C-c :" #'belak--eval-region-or-buffer

 ;; There doesn't seem to be a decent default for contract-region, so we set one.
 "C-+" #'er/contract-region

 ;; Allow M-S-q to undo M-q
 "M-Q" #'belak--unfill-paragraph

 ;; Prevent accidental usage of `list-buffers'
 "C-x C-b" #'switch-to-buffer

 ;; Add some convenience bindings for font size switching which work on multiple
 ;; platforms.
 "S-=" #'doom/increase-font-size
 "S--" #'doom/decrease-font-size
 "S-0" #'doom/reset-font-size

 ;; Replace zap-to-char with zap-up-to-char because I find it easier to grok.
 :g [remap zap-to-char] #'zap-up-to-char
 :g "M-S-z"             #'reverse-zap-up-to-char

 ;; Make home and end do the same thing as C-a/C-e rather than going to the
 ;; beginning/end of a buffer.
 :g "<home>" #'doom/backward-to-bol-or-indent
 :g "<end>"  #'doom/forward-to-last-non-comment-or-eol

 ;; Allow C-a and C-e to work in normal mode as well.
 ;; TODO: make sure they also enter insert mode
 :n "C-a" #'doom/backward-to-bol-or-indent
 :n "C-e" #'doom/forward-to-last-non-comment-or-eol)
