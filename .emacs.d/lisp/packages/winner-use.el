;;; winner-use.el --- Revert to previous window states (bundled with Emacs)

(use-package winner
  :bind
  (:map winner-mode-map
        ("C-w C-h" . winner-undo)
        ("C-w C-l" . winner-redo))

  :config
  (winner-mode 1)

  :demand)

(provide 'winner-use)

;;; winner-use.el ends here
