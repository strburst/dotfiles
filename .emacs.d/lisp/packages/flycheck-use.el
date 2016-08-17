;;; flycheck-use.el --- Integrated syntax checker/linter
;;; Commentary:
;;; Code:

(require 'lib)
(require 'key)

(use-package flycheck
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit)

  (defvar flycheck-use/flycheck-auto-hooks '(emacs-lisp-mode-hook)
    "Hooks to automatically call flycheck-mode in.")

  (lib/add-minor-mode-hooks flycheck-use/flycheck-auto-hooks 'flycheck-mode)

  (key/leader-map "t f" 'flycheck-mode))

(provide 'flycheck-use)

;;; flycheck-use.el ends here
