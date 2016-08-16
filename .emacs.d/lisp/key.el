;;; key.el --- For miscellaneous keybindings that don't fit elsewhere
;;; Commentary:
;;; Code:

(defvar key/leader "SPC")
(defvar key/leader-double "SPC SPC")
(defvar key/fallback-leader "M-SPC") ; Fallback used when evil is inactive
(defvar key/fallback-leader-double "M-SPC SPC")

(setq general-default-states '(normal insert emacs))

(defun key/leader-map (&rest maps)
  "Assign the given MAPS with prefix leader in evil normal mode and fallback-leader elsewhere."
  (apply 'general-define-key
         :prefix key/leader
         :non-normal-prefix key/fallback-leader
         maps))

(defun key/leader-double-map (&rest maps)
  "Assign the given MAPS with prefix leader in evil normal mode and fallback-leader elsewhere."
  (apply 'general-define-key
         :prefix key/leader-double
         :non-normal-prefix key/fallback-leader-double
         maps))

;; Miscellaneous
(key/leader-map "d" 'switch-to-buffer)

;; File operations
(key/leader-map "f d" 'vc-delete-file
                "f f" 'find-file
                "f k" 'kill-this-buffer
                "f r" 'vc-rename-file
                "f s" 'save-buffer)

;; Emacs-Lisp-related
(key/leader-map "l e" 'eval-last-sexp
                "l i" 'eval-print-last-sexp)

;; Toggles
(key/leader-map "t h" 'evil-ex-nohighlight
                "t m" 'menu-bar-mode)

(global-set-key (kbd "<f2>") 'lib/echo-major-mode)
(global-set-key (kbd "<f9>") 'lib/open-emacs-config)
(global-set-key (kbd "<f12>") 'list-packages)

(provide 'key)

;;; key.el ends here
