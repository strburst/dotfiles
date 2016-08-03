;;; elscreen-use.el --- Add tabs to Emacs, similar to GNU Screen

(use-package elscreen
  :init
  ;; Change prefix key to C-f
  (setq elscreen-prefix-key (kbd "C-f"))
  (define-key evil-motion-state-map (kbd "C-f") nil)
  (define-key global-map (kbd "C-f") nil)

  :config
  (elscreen-start)
  (setq elscreen-tab-display-control nil)  ; Don't display dummy control tab

  (elscreen-toggle-display-tab))

(provide 'elscreen-use)

;;; elscreen-use.el ends here
