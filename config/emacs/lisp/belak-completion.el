;;; belak-completion.el -*- lexical-binding: t; -*-

(require 'belak-lib)

;;
;;; Minibuffer

;; A nice, no-nonsense completing-read replacement. I switched to this over
;; `selectrum' because it seems to be slightly better designed and I switched
;; from `ido-mode' (along with `ido-vertical-mode', `flx-ido', `smex', `anzu'
;; and more) because I get roughly the same features with much less
;; configuration.
(use-package vertico
  :hook (after-init . vertico-mode))

(use-package consult
  :bind (([remap goto-line]        . consult-goto-line)
         ([remap switch-to-buffer] . consult-buffer)))

;; Orderless lets us tweak the completion sorting/filtering with nausiating
;; detail.
(use-package orderless
  :demand t
  :config
  ;; Enable the orderless completion style
  (setq completion-styles '(orderless basic))

  ;; There's an odd issue when using TRAMP, that causes hostname completion to
  ;; not work, so there needs to be an override for files which tries basic
  ;; first.
  (setq completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion)))))

;; This makes completing-read frameworks work more like helm with useful columns
;; of information, but with way less configuration.
(use-package marginalia
  :hook (after-init . marginalia-mode)
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle)))


;;
;;; In-Buffer

(use-package corfu
  :hook (after-init . global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.25))

(use-package corfu-history
  :hook (after-init . corfu-history-mode))

(use-package corfu-popupinfo
  :hook (after-init . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.5 . 0.2)))


(provide 'belak-completion)
;;; belak-completion.el ends here
