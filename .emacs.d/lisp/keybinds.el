;;; keybinds.el --- For miscellaneous keybindings that don't fit elsewhere

(setq leader "SPC"
      leader-double "SPC SPC"
      fallback-leader "M-SPC"  ; Fallback used when evil is inactive
      fallback-leader-double "M-SPC SPC")

(setq general-default-states '(normal insert emacs))

(defun leader-map (&rest maps)
  "Assign the given maps with prefix leader in evil normal mode and
fallback-leader elsewhere."
  (apply 'general-define-key
         :prefix leader
         :non-normal-prefix fallback-leader
         maps))

(defun leader-double-map (&rest maps)
  "Assign the given maps with prefix leader in evil normal mode and
fallback-leader elsewhere."
  (apply 'general-define-key
         :prefix leader-double
         :non-normal-prefix fallback-leader-double
         maps))

;; Miscellaneous
(leader-map "d" 'switch-to-buffer)

;; File operations
(leader-map "f f" 'find-file
            "f w" 'save-buffer)

;; Toggles
(leader-map "t h" 'evil-ex-nohighlight
            "t m" 'menu-bar-mode)

(global-set-key (kbd "<f2>") 'lib/echo-major-mode)
(global-set-key (kbd "<f9>") 'lib/open-emacs-config)
(global-set-key (kbd "<f12>") 'list-packages)

(provide 'keybinds)

;;; keybinds.el ends here
