;;; smart-mode-line-cfg.el --- Nicer mode line
;;; Commentary:
;;; Code:

(use-package smart-mode-line
  :config
  (setq sml/theme 'respectful)
  (sml/setup)
  (display-battery-mode 1))

(provide 'smart-mode-line-cfg)

;;; smart-mode-line-cfg.el ends here
