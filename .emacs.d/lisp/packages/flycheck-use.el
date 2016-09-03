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

  (lib/add-mode-hooks flycheck-use/flycheck-auto-hooks 'flycheck-mode)

  (key/leader-map "t f" 'flycheck-mode)

  (key/leader-map "c C-c" 'flycheck-compile
                  "c C-w" 'flycheck-copy-errors-as-kill
                  "c ?" 'flycheck-describe-checker
                  "c C" 'flycheck-clear
                  "c H" 'display-local-help
                  "c V" 'flycheck-version
                  "c c" 'flycheck-buffer
                  "c e" 'flycheck-set-checker-executable
                  "c h" 'flycheck-display-error-at-point
                  "c i" 'flycheck-manual
                  "c l" 'flycheck-list-errors
                  "c n" 'flycheck-next-error
                  "c p" 'flycheck-previous-error
                  "c s" 'flycheck-select-checker
                  "c v" 'flycheck-verify-setup
                  "c x" 'flycheck-disable-checker))

(provide 'flycheck-use)

;;; flycheck-use.el ends here
