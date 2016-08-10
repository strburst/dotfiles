;;; keybinds.el --- For miscellaneous keybindings that don't fit elsewhere

(setq leader "SPC"
      leader-double "SPC SPC"
      fallback-leader "C-c C-d"  ; Fallback used when evil is inactive
      fallback-leader-double "C-c C-d C-d")

(defmacro leader-map (&rest maps)
  "Assign the given maps with prefix leader in evil normal mode and
fallback-leader elsewhere."
  `(progn
     (general-define-key :keymaps 'evil-normal-state-map
                         :prefix leader
                         ,@maps)
     (general-define-key :prefix fallback-leader
                         ,@maps)))

(leader-map "f" 'find-file
            "d" 'switch-to-buffer)

(global-set-key (kbd "<f2>") 'base-lib/echo-major-mode)
(global-set-key (kbd "<f9>") 'base-lib/open-emacs-config)
(global-set-key (kbd "<f12>") 'base-lib/list-packages)

(provide 'keybinds)

;;; keybinds.el ends here
