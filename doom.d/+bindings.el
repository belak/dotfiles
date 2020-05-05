;;; $DOOMDIR/+bindings.el -*- lexical-binding: t; -*-

;; There are a number of keybinds I'm not a huge fan of, so we just override
;; them.
(map!
 "s-k"                               #'kill-this-buffer
 "s-N"                               #'+default/new-buffer

 "s-}"                               #'forward-paragraph
 "s-{"                               #'backward-paragraph

 "s-0"                               #'doom/reset-font-size

 "C-s-<up>"                          #'windmove-up
 "C-s-<down>"                        #'windmove-down
 "C-s-<right>"                       #'windmove-right
 "C-s-<left>"                        #'windmove-left

 "C-<tab>"                           #'+workspace/switch-right
 "C-S-<tab>"                         #'+workspace/switch-left

 ;; Plugins
 "C-:"                               #'ace-jump-mode

 ;; Smartparens
 (:after smartparens
   (:map smartparens-mode-map
     "C-M-a"                         #'sp-beginning-of-sexp
     "C-M-e"                         #'sp-end-of-sexp
     "C-M-f"                         #'sp-forward-sexp
     "C-M-b"                         #'sp-backward-sexp
     "C-M-d"                         #'sp-splice-sexp
     "C-M-k"                         #'sp-kill-sexp
     "C-M-t"                         #'sp-transpose-sexp
     ;; TODO rethink these bindings
     "C-<right>"                     nil
     "M-<right>"                     nil
     "C-<left>"                      nil
     "M-<left>"                      nil
     "C-M-d"                         nil))

 "C-S-<up>"    #'drag-stuff-up
 "C-S-<down>"  #'drag-stuff-down
 "C-S-<left>"  #'drag-stuff-left
 "C-S-<right>" #'drag-stuff-right

 ;; Unfortunately re-mapping M-<right> and M-<left> isn't as simple as setting
 ;; them. We need to remove them from smartparens (as that map is loaded
 ;; globally) and then override the drag-stuff bindings which overrode the
 ;; original keys we're setting them back to.
 "C-<right>" #'right-word
 "C-<left>"  #'left-word
 "M-<right>" #'right-word
 "M-<left>"  #'left-word

 "M-x" #'smex
 "M-X" #'smex-major-mode-commands)
