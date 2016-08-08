;;; magit-use.el --- Git porcelain/interface

(use-package magit
  :config
  (setq magit-completing-read-function 'magit-ido-completing-read)

  (global-set-key (kbd "C-x g") 'magit-status)
  (global-set-key (kbd "C-x C-g") 'magit-status))

(use-package evil-magit)

(use-package gitattributes-mode)

(use-package gitconfig-mode)

(use-package gitignore-mode)

(provide 'magit-use)

;;; magit-use.el ends here
