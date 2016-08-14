;;; key.el --- For miscellaneous keybindings that don't fit elsewhere

(setq leader "SPC"
      leader-double "SPC SPC"
      fallback-leader "M-SPC"  ; Fallback used when evil is inactive
      fallback-leader-double "M-SPC SPC")

(setq general-default-states '(normal insert emacs))

(defun key/leader-map (&rest maps)
  "Assign the given maps with prefix leader in evil normal mode and
fallback-leader elsewhere."
  (apply 'general-define-key
         :prefix leader
         :non-normal-prefix fallback-leader
         maps))

(defun key/leader-double-map (&rest maps)
  "Assign the given maps with prefix leader in evil normal mode and
fallback-leader elsewhere."
  (apply 'general-define-key
         :prefix leader-double
         :non-normal-prefix fallback-leader-double
         maps))

;; Miscellaneous
(key/leader-map "d" 'switch-to-buffer)

;; File operations
(key/leader-map "f f" 'find-file
                "f w" 'save-buffer)

;; Toggles
(key/leader-map "t h" 'evil-ex-nohighlight
                "t m" 'menu-bar-mode)

(global-set-key (kbd "<f2>") 'lib/echo-major-mode)
(global-set-key (kbd "<f9>") 'lib/open-emacs-config)
(global-set-key (kbd "<f12>") 'list-packages)

(provide 'key)

;;; key.el ends here
