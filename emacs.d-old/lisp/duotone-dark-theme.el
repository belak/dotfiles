;;; duotone-dark-theme --- an emacs theme based on duotone for atom
;;
;;; Commentary:
;; Scheme: Kaleb Elwert <belak@coded.io>
;;
;;; Code:

(deftheme duotone-dark)

(require 'color)

(defun theme-lighten (hsl amount)
  (color-lighten-hsl (car hsl)
                     (car (cdr hsl))
                     (car (cddr hsl))
                     (* 100 amount)))

(defun theme-darken (hsl amount)
  (color-darken-hsl (car hsl)
                    (car (cdr hsl))
                    (car (cddr hsl))
                    (* 100 amount)))

(defun theme-hex (hsl)
  (apply 'color-rgb-to-hex (apply 'color-hsl-to-rgb hsl)))

(let* ((syntax-uno (/ 250.0 360.0))
       (syntax-duo (/  30.0 360.0))

       ;; Uno hue
       (uno1 (list syntax-uno 1.00 0.96))
       (uno2 (list syntax-uno 0.98 0.86))
       (uno3 (list syntax-uno 0.96 0.78))
       (uno4 (list syntax-uno 0.12 0.46))

       ;; Duo hue
       (duo1 (list syntax-duo 1.00 0.80))
       (duo2 (list syntax-duo 0.72 0.62))
       (duo3 (list syntax-duo 0.06 0.46))

       ;; Base colors
       (syntax-fg               uno2)
       (syntax-bg               (list syntax-uno 0.14 0.18))
       (syntax-accent           (list syntax-duo 1.00 0.66))
       (syntax-guide            (theme-lighten syntax-bg 0.10))
       (syntax-selection        (theme-lighten syntax-bg 0.12))
       (syntax-selection-gutter (theme-lighten syntax-bg 0.03))

       (syntax-color-renamed  (list (/ 208.0 360.0) 1.0 0.6))
       (syntax-color-added    (list (/ 150.0 360.0) 1.0 0.6))
       (syntax-color-modified (list (/ 40.0 360.0)  1.0 0.6))
       (syntax-color-removed  (list (/ 0.0 360.0)   1.0 0.6))

       ;; TODO: Alpha isn't a thing so this needs to be fixed.
       (syntax-result-marker-color (theme-darken syntax-accent 0.09))

       ;; This doesn't quite match, but since alpha isn't a thing in
       ;; emacs, we need to fudge it a bit.
       (syntax-cursor-line (theme-darken syntax-selection 0.09)))

  (let ((uno1 (theme-hex uno1))
        (uno2 (theme-hex uno2))
        (uno3 (theme-hex uno3))
        (uno4 (theme-hex uno4))

        (duo1 (theme-hex duo1))
        (duo2 (theme-hex duo2))
        (duo3 (theme-hex duo3))

        (syntax-fg               (theme-hex syntax-fg))
        (syntax-bg               (theme-hex syntax-bg))
        (syntax-accent           (theme-hex syntax-accent))
        (syntax-guide            (theme-hex syntax-guide))
        (syntax-selection        (theme-hex syntax-selection))
        (syntax-selection-gutter (theme-hex syntax-selection-gutter))

        (syntax-color-renamed  (theme-hex syntax-color-renamed))
        (syntax-color-added    (theme-hex syntax-color-added))
        (syntax-color-modified (theme-hex syntax-color-modified))
        (syntax-color-removed  (theme-hex syntax-color-removed))

        (syntax-result-marker-color (theme-hex syntax-result-marker-color))

        (syntax-cursor-line  (theme-hex syntax-cursor-line)))

    (custom-theme-set-faces
     'duotone-dark

     ;; Built-in stuff (Emacs 23)
     ;;`(border ((t (:background ,base03))))
     `(border-glyph ((t (nil))))
     `(cursor ((t (:background ,syntax-accent))))
     `(default ((t (:background ,syntax-bg :foreground ,syntax-fg))))
     `(fringe ((t (:background ,syntax-bg))))
     ;;`(gui-element ((t (:background ,base03 :foreground ,base06))))
     `(highlight ((t (:background ,syntax-cursor-line))))
     ;;`(link ((t (:foreground ,base0D))))
     ;;`(link-visited ((t (:foreground ,base0E))))
     ;;`(minibuffer-prompt ((t (:foreground ,base0D))))

     `(mode-line ((t (:background ,syntax-selection :foreground ,syntax-fg :box nil))))
     `(mode-line-buffer-id ((t (:foreground ,uno1 :background nil))))
     `(mode-line-emphasis ((t (:foreground ,uno2 :slant italic))))
     `(mode-line-highlight ((t (:foreground ,uno1 :box nil :weight bold))))
     `(mode-line-inactive ((t (:background ,syntax-selection :foreground ,syntax-fg :box nil))))

     `(region ((t (:background ,syntax-selection))))
     ;;`(secondary-selection ((t (:background ,base03))))
     `(error ((t (:foreground ,syntax-color-removed :weight bold))))
     `(warning ((t (:foreground ,syntax-color-modified :weight bold))))
     `(success ((t (:foreground ,syntax-color-added :weight bold))))

     ;;`(header-line ((t (:inherit mode-line :foreground ,base0E :background nil))))

     ;; Font-lock stuff
     ;;`(font-lock-builtin-face ((t (:foreground ,base04))))
     ;;`(font-lock-comment-delimiter-face ((t (:foreground ,base02))))
     `(font-lock-comment-face ((t (:foreground ,uno4 :slant italic))))
     `(font-lock-constant-face ((t (:foreground ,duo1))))
     ;;`(font-lock-doc-face ((t (:foreground ,base03))))
     ;;`(font-lock-doc-string-face ((t (:foreground ,base04))))
     `(font-lock-function-name-face ((t (:foreground ,uno3))))
     `(font-lock-keyword-face ((t (:foreground ,duo2))))
     ;;`(font-lock-negation-char-face ((t (:foreground ,base0B))))
     ;;`(font-lock-preprocessor-face ((t (:foreground ,base0D))))
     ;;`(font-lock-regexp-grouping-backslash ((t (:foreground ,base0A))))
     ;;`(font-lock-regexp-grouping-construct ((t (:foreground ,base0E))))
     `(font-lock-string-face ((t (:foreground ,duo1))))
     `(font-lock-type-face ((t (:foreground ,syntax-fg))))
     `(font-lock-variable-name-face ((t (:foreground ,uno2))))
     ;;`(font-lock-warning-face ((t (:foreground ,base08))))

     ;; linum-mode
     `(linum ((t (:background ,syntax-bg, :foreground ,uno4))))
     `(linum-highlight-face ((t (:foreground ,uno2 :background ,syntax-bg))))

     ;; Search
     `(match ((t (:foreground ,syntax-accent))))
     `(isearch ((t (:background ,syntax-result-marker-color))))
     ;;`(isearch-lazy-highlight-face ((t (:foreground ,base0C :background ,base01 :inverse-video t))))
     ;;`(isearch-fail ((t (:background ,base01 :inherit font-lock-warning-face :inverse-video t))))

     ;; IDO
     ;;`(ido-subdir ((t (:foreground ,base04))))
     ;;`(ido-first-match ((t (:foreground ,base09 :weight bold))))
     ;;`(ido-only-match ((t (:foreground ,base08 :weight bold))))
     ;;`(ido-indicator ((t (:foreground ,base08 :background ,base01))))
     ;;`(ido-virtual ((t (:foreground ,base04))))

     `(trailing-whitespace ((t (:background ,syntax-guide))))
     ;;`(whitespace-empty ((t (:foreground ,base08 :background ,base0A))))
     ;;`(whitespace-hspace ((t (:background ,base04 :foreground ,base04))))
     ;;`(whitespace-indentation ((t (:background ,base0A :foreground ,base08))))
     ;;`(whitespace-line ((t (:background ,base01 :foreground ,base0F))))
     ;;`(whitespace-newline ((t (:foreground ,base04))))
     ;;`(whitespace-space ((t (:background ,base01 :foreground ,base04))))
     ;;`(whitespace-space-after-tab ((t (:background ,base0A :foreground ,base08))))
     ;;`(whitespace-space-before-tab ((t (:background ,base09 :foreground ,base08))))
     ;;`(whitespace-tab ((t (:background ,base04 :foreground ,base04))))
     `(whitespace-trailing ((t (:background ,syntax-guide))))

     ;; Parenthesis matching (built-in)
     `(show-paren-match ((t (:foreground ,syntax-accent))))
     ;;`(show-paren-mismatch ((t (:background ,base09 :foreground ,base03))))

     ;;`(link ((t (:foreground nil :underline t))))
     ;;`(widget-button ((t (:underline t))))
     ;;`(widget-field ((t (:background ,base03 :box (:line-width 1 :color ,base06)))))

     `(diff-changed ((t (:foreground ,syntax-color-modified))))
     `(diff-added ((t (:foreground ,syntax-color-added))))
     `(diff-removed ((t (:foreground ,syntax-color-removed))))

     `(diff-hl-change ((t (:foreground ,syntax-color-modified))))
     `(diff-hl-insert ((t (:foreground ,syntax-color-added))))
     `(diff-hl-delete ((t (:foreground ,syntax-color-removed))))

     ;;`(custom-variable-tag ((t (:foreground ,base0D))))
     ;;`(custom-group-tag ((t (:foreground ,base0D))))
     ;;`(custom-state ((t (:foreground ,base0B))))))
     )))

(provide-theme 'duotone-dark)

;;; duotone-dark-theme.el ends here
