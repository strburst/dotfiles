;;; winner-cfg.el --- Revert to previous window configurations (builtin package)
;;; Commentary:
;;; Code:

(use-package winner
  :config
  (winner-mode 1)

  (general-define-key :keymaps 'winner-mode-map
                      "C-w C-h" 'winner-undo
                      "C-w C-l" 'winner-redo))

(provide 'winner-cfg)

;;; winner-cfg.el ends here
