;;; paradox-use.el --- Improved list-packages interface
;;; Commentary:
;;; Code:

(require 'evil)

(use-package paradox
  :config
  (evil-set-initial-state 'paradox-menu-mode 'emacs)

  (global-set-key (kbd "S-<f12>") 'paradox-list-packages))

(provide 'paradox-use)

;;; paradox-use.el ends here
