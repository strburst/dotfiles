;;; alda-mode-cfg.el --- Load and enable solarized dark theme
;;; Commentary:
;;; Code:

(require 'evil)

(use-package alda-mode
  :config
  (define-key evil-motion-state-map "gp" 'alda-evil-play-region))

(provide 'alda-mode-cfg)

;;; alda-mode-cfg.el ends here
