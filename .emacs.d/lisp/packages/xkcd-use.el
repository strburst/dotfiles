;;; xkcd-use.el --- Load xkcd browser

(use-package xkcd
  :bind (([f8] . xkcd)
         :map xkcd-mode-map
         ("h" . xkcd-prev)
         ("j" . xkcd-next)
         ("k" . xkcd-prev)
         ("l" . xkcd-next))

  :config
  (evil-set-initial-state 'xkcd-mode 'emacs)
  (add-hook 'xkcd-mode-hook  ; Clear visual distractions
            (lambda ()
              (linum-mode -1)
              (blink-cursor-mode -1)
              (setq cursor-type nil)))

  (define-key image-map (kbd "r") nil))

(provide 'xkcd-use)

;;; xkcd-use.el ends here
