;;; paradox-cfg.el --- Improved list-packages interface
;;; Commentary:
;;; Code:

(require 'evil)

(use-package paradox
  :config
  (evil-set-initial-state 'paradox-menu-mode 'emacs)

  (setq paradox-execute-asynchronously t)  ; Don't ask to use async execution

  (global-set-key (kbd "S-<f12>") 'paradox-list-packages))

(provide 'paradox-cfg)

;;; paradox-cfg.el ends here
