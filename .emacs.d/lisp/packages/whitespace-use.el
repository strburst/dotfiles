;;; whitespace-use.el --- Show whitespace indicators (builtin package)

(use-package whitespace
  :config
  (setq whitespace-style (cons 'lines-tail (delete 'lines whitespace-style))
        whitespace-line-column nil)  ; Use value of fill-column instead

  (key/leader-map "t w" 'whitespace-mode))

(provide 'whitespace-use)

;;; whitespace-use.el ends here
