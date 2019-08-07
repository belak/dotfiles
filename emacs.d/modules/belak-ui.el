;; Various appearance settings

(setq column-number-mode t
      line-number-mode t)

;; Highlight between matching parens

(electric-pair-mode 1)

(use-package paren
  :straight nil
  :config
  (show-paren-mode 1)
  (setq show-paren-style 'parenthesis
        show-paren-delay 0))

(use-package whitespace
  :straight nil
  :diminish global-whitespace-mode
  :config
  (setq whitespace-style '(trailing face tabs tab-mark lines-tail)
        whitespace-display-mappings '((space-mark 32 [183] [46])
                                      (newline-mark 10 [182 10])
                                      (tab-mark 9 [9655 9] [92 9])))
  (global-whitespace-mode t)
  (setq whitespace-global-modes '(text-mode prog-mode org-mode)))
