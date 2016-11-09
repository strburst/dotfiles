;;; undo-tree-cfg.el --- Vim-like undo tree
;;; Commentary:
;;; Code:

(use-package undo-tree
  :config
  ;; Persist undo history between sessions
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))
        undo-tree-auto-save-history t)
  (global-undo-tree-mode 1)
  :diminish undo-tree-mode)

(provide 'undo-tree-cfg)

;;; undo-tree-cfg.el ends here
