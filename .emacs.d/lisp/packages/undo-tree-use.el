;;; undo-tree-use.el --- Vim-like undo tree
;;; Commentary:
;;; Code:

(use-package undo-tree
  :config
  ;; Persist undo history between sessions
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))
        undo-tree-auto-save-history t)
  (global-undo-tree-mode 1))

(provide 'undo-tree-use)

;;; undo-tree-use.el ends here
