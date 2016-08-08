;;; base-keybinds.el --- For keybindings independent of external packages

(global-set-key (kbd "<f2>") 'base-lib/print-major-mode)
(global-set-key (kbd "<f9>") 'base-lib/open-emacs-config)
(global-set-key (kbd "<f10>") 'base-lib/source-emacs-config)
(global-set-key (kbd "<f12>") 'base-lib/list-packages)

(provide 'base-keybinds)

;;; base-keybinds.el ends here
