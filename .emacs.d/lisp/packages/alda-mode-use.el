;;; alda-mode-use.el --- Load and enable solarized dark theme

(use-package alda-mode
  :config
  (define-key evil-motion-state-map "gp" 'alda-evil-play-region))

(provide 'alda-mode-use)

;;; alda-mode-use.el ends here
