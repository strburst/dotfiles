;;; whitespace-use.el --- Show whitespace indicators (builtin package)

(use-package whitespace
  :config
  (setq whitespace-line-column nil)  ; Use value of fill-column instead

  (leader-map "t w" 'whitespace-mode))

(provide 'whitespace-use)

;;; whitespace-use.el ends here
