;;; alda-mode-use.el --- Load and enable solarized dark theme

(use-package alda-mode
  :bind (:map evil-motion-state-map
         ("gp" . alda-evil-play-region)))

(provide 'alda-mode-use)

;;; alda-mode-use.el ends here
