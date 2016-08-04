;;; ido-use.el --- Intelligent auto-completion (builtin package)

(use-package ido
  :init
  (setq ido-enable-flex-matching t
        ido-everywhere t)

  :config
  (ido-mode 1)
  (ido-everywhere 1)

  (setq ido-create-new-buffer 'always))  ; Let ido make new files

(use-package smex
  :config
  (smex-initialize)

  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))

(use-package ido-ubiquitous
  :config
  (ido-ubiquitous-mode 1))

(provide 'ido-use)

;;; ido-use.el ends here
